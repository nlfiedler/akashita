%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Nathan Fiedler
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% This modules contains the bulk of the backup logic.
%%
-module(akashita_backup).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {timer, vault}).

% TODO: setup application configuration (most likely sys.config)

%%
%% Client API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, TRef} = fire_soon(),
    Vault = next_eligible_vault(),
    State = #state{timer=TRef, vault=Vault},
    {ok, State}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(process, State) ->
    NewState = process_uploads(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    lager:notice("unexpected message: ~w", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Process the pending uploads until it is time to pause, or we are finished.
process_uploads(State) ->
    {ok, GoTimes} = application:get_env(akashita_backup, go_times),
    {{_Y, _M, _D}, {Hour,  Min, _S}} = erlang:localtime(),
    case akashita:is_go_time(GoTimes, Hour, Min) of
        true ->
            Vault = State#state.vault,
            NextVault = case process_one_archive(Vault) of
                true -> next_eligible_vault();
                false -> Vault
            end,
            NewState = #state{timer=State#state.timer, vault=NextVault},
            case is_backup_complete() of
                true ->
                    % TODO: is this sufficient to shutdown the entire application?
                    %       calling application:stop/1 won't do it, according to the docs
                    gen_server:call(akashita_backup, terminate),
                    NewState;
                false ->
                    process_uploads(NewState)
            end;
        false ->
            {ok, TRef} = fire_later(),
            State#state{timer=TRef}
    end.

% Process a single archive in the given Vault. If this vault is now
% completed, return true, otherwise false.
process_one_archive(undefined) ->
    % just in case this happens, pretend we finished this "vault"
    true;
process_one_archive(Vault) ->
    AppConfig = application:get_all_env(akashita_backup),
    Tag = akashita_app:retrieve_tag(),
    {ok, Snapshot} = akashita:ensure_snapshot_exists(Tag, Vault),
    VaultList = proplists:get_value(vaults, AppConfig),
    VaultConf = proplists:get_value(Vault, VaultList),
    CloneBase = proplists:get_value(clone_base, VaultConf),
    CloneName = filename:join(CloneBase, Vault),
    ok = akashita:ensure_clone_exists(CloneName, Snapshot, AppConfig),
    ok = akashita:ensure_vault_created(Vault),
    ArchiveDir = akashita:ensure_archives(Vault, Tag, VaultConf),
    case file:list_dir(ArchiveDir) of
        [] ->
            % this should not have happened
            error(empty_archive_dir);
        [Archive|Rest] ->
            lager:info("uploading archive: ~s", [Archive]),
            ok = akashita:upload_archive(Archive),
            ok = file:delete(Archive),
            case length(Rest) of
                0 ->
                    ok = akashita_app:remember_completed_vault(Vault),
                    ok = akashita:destroy_dataset(CloneName, AppConfig),
                    ok = akashita:destroy_dataset(Snapshot, AppConfig),
                    ok = file:del_dir(ArchiveDir),
                    lager:info("vault completed: ~s", [Vault]),
                    true;
                _ ->
                    false
            end
    end.

% Determine if the entire backup process has been completed or not.
is_backup_complete() ->
    {ok, Vaults} = application:get_env(akashita_backup, vaults),
    VaultNames = proplists:get_keys(Vaults),
    Completed = fun(Name) ->
        akashita_app:is_vault_completed(Name)
    end,
    lists:all(Completed, VaultNames).

% Find the next vault that has not yet been processed, or undefined if none.
next_eligible_vault() ->
    {ok, Vaults} = application:get_env(akashita_backup, vaults),
    VaultNames = proplists:get_keys(Vaults),
    NotCompleted = fun(Name) ->
        akashita_app:is_vault_completed(Name) == false
    end,
    case lists:filter(NotCompleted, VaultNames) of
        [] -> undefined;
        [H|_T] -> H
    end.

% Start a timer to cast a 'process' message to us in 10 minutes.
fire_later() ->
    M = gen_server,
    F = cast,
    A = [akashita_backup, process],
    timer:apply_after(1000*60*10, M, F, A).

% Start a timer to cast a 'process' message to us in 10 seconds.
fire_soon() ->
    M = gen_server,
    F = cast,
    A = [akashita_backup, process],
    timer:apply_after(1000*10, M, F, A).
