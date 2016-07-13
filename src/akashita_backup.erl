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
%% Primary driver of the backup procedure.
%%
-module(akashita_backup).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([link_process/1, process_vault/1]).

-record(state, {timer, vault, worker, callback}).

%%
%% Client API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% gen_server callbacks
%%
init([]) ->
    Vault = next_eligible_vault(),
    {ok, TRef} = start_timer(),
    State = #state{timer=TRef, vault=Vault},
    {ok, State}.

handle_call(begin_backup, _From, State) ->
    % Kick off the backup process from the beginning, assuming nothing
    % about the current state. Does nothing if a worker is already running,
    % which implies the current backup is not yet finished.
    case State#state.worker of
        undefined ->
            cancel_timer(State#state.timer),
            {ok, NewState} = init([]),
            {reply, ok, NewState};
        _Pid -> {reply, ok, State}
    end;
handle_call(test_backup, {FromPid, _FromTag}, State) ->
    % Used only by the test suite, will send a message to the sender when
    % the backup has completed.
    State1 = case is_go_time() of
        true ->
            cancel_timer(State#state.timer),
            ensure_worker(State#state{callback=FromPid, timer=undefined});
        false ->
            lager:info("not time to backup"),
            State
    end,
    {reply, ok, State1}.

handle_cast(get_to_work, State) ->
    % Ensure there is a worker handling the uploads.
    {noreply, ensure_worker(State)};
handle_cast(completed, State) ->
    % A vault has been completed, check if there is any additional work to
    % be done, and start a new worker if true. Otherwise, stop the interval
    % timer, clear the cache, and go to sleep.
    State1 = terminate_worker(State),
    State2 = case is_backup_complete() of
        true ->
            cancel_timer(State1#state.timer),
            ok = akashita_app:delete_cache(),
            % for the sake of the test code...
            case State1#state.callback of
                undefined -> ok;
                Callback -> Callback ! backup_finished
            end,
            lager:info("backup complete"),
            #state{};
        false ->
            lager:info("backup not yet complete"),
            ensure_worker(State1#state{vault=next_eligible_vault()})
    end,
    {noreply, State2};
handle_cast(incomplete, State) ->
    % The worker ran out of time; clear it from the supervisor so we can
    % start a new worker next time.
    {noreply, terminate_worker(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    lager:notice("unexpected message: ~w", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Business logic functions
%%

% Ensure a worker process is running, if the time to upload is now. Return
% the updated server state, with the worker process pid.
ensure_worker(State) ->
    Worker = case is_go_time() of
        true ->
            % If there is no worker, start one now and have it supervised
            % by our supervisor. It will immediately start uploading
            % archives for the named vault. The supervisor will be restart
            % the worker automatically if it terminates abnormally.
            case State#state.worker of
                undefined ->
                    ChildSpec = #{
                        id => backup_worker,
                        start => {akashita_backup, link_process, [State#state.vault]},
                        restart => transient,
                        shutdown => 5000,
                        type => worker,
                        modules => [akashita_backup]
                    },
                    {ok, Pid} = supervisor:start_child(akashita_sup, ChildSpec),
                    Pid;
                Pid -> Pid
            end;
        false -> State#state.worker
    end,
    State#state{worker=Worker}.

% Spawn the worker process and return the result expected by supervisor.
% Must link with the spawned process for the sake of the supervisor.
link_process(Vault) ->
    {ok, spawn_link(akashita_backup, process_vault, [Vault])}.

% Terminate the worker process, remove it from the supervisor, and return
% the updated server state. This is done to make it easier to start a new
% worker process when the time comes.
terminate_worker(State) ->
    ok = supervisor:terminate_child(akashita_sup, backup_worker),
    ok = supervisor:delete_child(akashita_sup, backup_worker),
    State#state{worker=undefined}.

% Process a single vault until it is finished, or the time for uploading
% comes to an end. Casts a message to the gen_server in either case.
process_vault(Vault) ->
    case is_go_time() of
        true ->
            case process_one_archive(Vault) of
                true -> gen_server:cast(akashita_backup, completed);
                false -> process_vault(Vault)
            end;
        false ->
            lager:info("reached end of upload window"),
            gen_server:cast(akashita_backup, incomplete)
    end.

% Process a single archive in the given Vault. If this vault is now
% completed, return true, otherwise false.
process_one_archive(undefined) ->
    % just in case this happens, pretend we finished this "vault"
    true;
process_one_archive(Vault) ->
    AppConfig = application:get_all_env(akashita),
    Tag = akashita_app:retrieve_tag(),
    VaultTag = Vault ++ "-" ++ Tag,
    VaultList = proplists:get_value(vaults, AppConfig),
    VaultConf = proplists:get_value(Vault, VaultList),
    Dataset = proplists:get_value(dataset, VaultConf),
    {ok, Snapshot} = akashita:ensure_snapshot_exists(Tag, Dataset, AppConfig),
    CloneName = proplists:get_value(clone_base, VaultConf),
    ok = akashita:ensure_clone_exists(CloneName, Snapshot, AppConfig),
    ok = akashita:ensure_vault_created(VaultTag),
    ArchiveDir = akashita:ensure_archives(Vault, Tag, AppConfig),
    lager:info("archive dir: ~s", [ArchiveDir]),
    case file:list_dir(ArchiveDir) of
        {ok, []} ->
            % this should not have happened
            error(empty_archive_dir);
        {ok, [Archive|Rest]} ->
            lager:info("uploading archive: ~s", [Archive]),
            Desc = "filename:" ++ Archive,
            ArchivePath = filename:join(ArchiveDir, Archive),
            {Time, ok} = timer:tc(akashita, upload_archive, [ArchivePath, Desc, VaultTag]),
            lager:info("archive ~s uploaded in ~.2f minutes", [Archive, Time / 60000000]),
            ok = file:delete(ArchivePath),
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
    {ok, Vaults} = application:get_env(akashita, vaults),
    VaultNames = proplists:get_keys(Vaults),
    Completed = fun(Name) ->
        akashita_app:is_vault_completed(Name)
    end,
    lists:all(Completed, VaultNames).

% Find the next vault that has not yet been processed, or undefined if none.
next_eligible_vault() ->
    {ok, Vaults} = application:get_env(akashita, vaults),
    VaultNames = proplists:get_keys(Vaults),
    NotCompleted = fun(Name) ->
        akashita_app:is_vault_completed(Name) == false
    end,
    case lists:filter(NotCompleted, VaultNames) of
        [] -> undefined;
        [H|_T] -> H
    end.

% Start an interval timer to cast a 'get_to_work' message to the gen_server
% every 10 minutes.
start_timer() ->
    M = gen_server,
    F = cast,
    A = [akashita_backup, get_to_work],
    timer:apply_interval(1000*60*10, M, F, A).

% Cancel the timer; does nothing if timer is 'undefined'.
cancel_timer(undefined) ->
    ok;
cancel_timer(TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.

% Convenience function to determine if it is time to upload archives.
is_go_time() ->
    {ok, GoTimes} = application:get_env(akashita, go_times),
    {{_Y, _M, _D}, {Hour,  Min, _S}} = erlang:localtime(),
    akashita:is_go_time(GoTimes, Hour, Min).
