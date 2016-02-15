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

-record(state, {timer}).

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
    State = #state{timer=TRef},
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
    % TODO: load the go times from the configuration file, not app env
    {ok, GoTimes} = application:get_env(akashita_backup, go_times),
    {{_Y, _M, _D}, {Hour,  Min, _S}} = erlang:localtime(),
    case akashita:is_go_time(GoTimes, Hour, Min) of
        true ->
            process_one_archive(),
            WeAreDone = false, % TODO: determine if we are done or not
            case WeAreDone of
                true ->
                    % TODO: is this sufficient to shutdown the entire application?
                    gen_server:call(akashita_backup, terminate),
                    State;
                false ->
                    process_uploads(State)
            end;
        false ->
            {ok, TRef} = fire_later(),
            State#state{timer=TRef}
    end.

process_one_archive() ->
    % TODO: ensure the vault has been created
    % WorkDir = "TODO",
    % Prefix = "TODO",
    % Tag = "TODO",
    % Paths = ["TODO"],
    % SourceDir = "TODO",
    % akashita:ensure_archives(WorkDir, Prefix, Tag, Paths, SourceDir),
    % TODO: check the records for which vaults have been completed
    % TODO: upload a single archive
    % TODO: record each vault after successful completion
    % TODO: when vault is finished, destroy the zfs clone and snapshot
    ok.

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
