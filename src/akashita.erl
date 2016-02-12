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
%% Backup ZFS datasets to Amazon Glaicer.
%%
%% This module contains the low-level, easily testable functions.
%%

-module(akashita).

-export([main/1, is_go_time/3, create_archives/5]).

main(_Args) ->
    io:format("Starting backup process...~n"),
    {ok, _Started} = application:ensure_all_started(akashita_app),
    % block forever (this process is not expected to receive messages)
    receive
        _ -> ok
    end.

% TODO: read the akashita.config configuration file (in Erlang expressions format)
% TODO: code and test the vault/archive completion cache
% TODO: code and test the "tag" computation and caching
% TODO: code the zfs snapshot function
% TODO: code the zfs clone function
% TODO: code the zfs dataset destroy function

% Determine if given time falls within upload window(s).
is_go_time(Windows, Hour, Minute)
        when Hour >= 0 andalso Hour < 24
        andalso Minute >= 0 andalso Minute < 60 ->
    TheTime = {Hour, Minute},
    WindowList = re:split(Windows, ",", [{return, list}]),
    ConvertHour = fun(Str) ->
        case list_to_integer(Str) of
            X when X >= 0 andalso X < 24 -> X;
            _ -> erlang:error(badarg)
        end
    end,
    ConvertMinute = fun(Str) ->
        case list_to_integer(Str) of
            X when X >= 0 andalso X < 60 -> X;
            _ -> erlang:error(badarg)
        end
    end,
    InWindow = fun(Elem) ->
        [StartStr, EndStr] = re:split(Elem, "-", [{return, list}]),
        [StartHour, StartMin] = re:split(StartStr, ":", [{return, list}]),
        Start = {ConvertHour(StartHour), ConvertMinute(StartMin)},
        [EndHour, EndMin] = re:split(EndStr, ":", [{return, list}]),
        End = {ConvertHour(EndHour), ConvertMinute(EndMin)},
        case End < Start of
            true  -> (Start =< TheTime) or (TheTime =< End);
            false -> (Start =< TheTime) and (TheTime =< End)
        end
    end,
    lists:any(InWindow, WindowList).

% Produce the tar archives (split into reasonably sized files) for the given
% list of Paths (relative to the SourceDir directory), with the split files having
% the given Prefix. The split files will be created in the SplitDir directory.
%
% The Options argument is a proplist consisting of the following properties:
%
%   split_size: a string passed directly to the split command; default "64M"
%   compress: a bool that indicates whether to compress (bzip2) the tar files; default false
%   excludes: a list of patterns (filename()) to be excluded from the tar files; default []
%
create_archives(Paths, Prefix, SourceDir, SplitDir, Options) ->
    lager:info("generating tar archives...~n"),
    SplitSize = proplists:get_value(split_size, Options, "64M"),
    Compress = proplists:get_value(compress, Options, false),
    Exclusions = proplists:get_value(excludes, Options, []),
    TarCmd = string:join(tar_cmd(Paths, Compress, Exclusions), " "),
    TarPort = erlang:open_port({spawn, TarCmd}, [exit_status, binary, {cd, SourceDir}]),
    SplitCmd = string:join(split_cmd(Prefix, SplitSize), " "),
    SplitPort = erlang:open_port({spawn, SplitCmd}, [exit_status, binary, {cd, SplitDir}]),
    % start with a ClosedCount of 1, otherwise split hangs indefinitely
    {ok, 0} = pipe_until_exit(TarPort, SplitPort, 1),
    % close the ports so the programs know to terminate (especially split)
    case erlang:port_info(TarPort) of
        undefined -> ok;
        _         -> true = erlang:port_close(TarPort)
    end,
    case erlang:port_info(SplitPort) of
        undefined -> ok;
        _         -> true = erlang:port_close(SplitPort)
    end,
    lager:info("tar archive creation complete~n"),
    ok.

% Generate the command to invoke tar for the given set of file paths.
tar_cmd(Paths, Compress, Exclusions) ->
    Copt = case Compress of
        true -> ["-j"];
        false -> []
    end,
    Eopt = case Exclusions of
        [] -> [];
        _ -> Exclusions
    end,
    ["tar", "-c"] ++ Copt ++ Eopt ++ Paths.

% Generate the command to invoke split, reading from standard input, and
% producing files whose names begin with the given prefix. The SplitSize
% is passed directly to the split command (e.g. "256K" is 262144 bytes).
split_cmd(Prefix, SplitSize) ->
    [
        %
        % These options should work for both GNU split and BSD split for the
        % sake of testing on various systems.
        %
        "split",
        "-d",
        % The split command fails if it runs out of suffix digits, so give it
        % enough digits to handle our rather large files.
        "-a", "5",
        "-b", SplitSize,
        "-",
        Prefix
    ].

% Receive data from SendPort and write to RecvPort. Waits for both ports to close.
pipe_until_exit(SendPort, RecvPort, ClosedCount) ->
    receive
        {_Port, {exit_status, Status}} ->
            if ClosedCount == 1 -> {ok, Status};
                true -> pipe_until_exit(SendPort, RecvPort, ClosedCount + 1)
            end;
        {Port, {data, Data}} ->
            if Port =:= SendPort ->
                    RecvPort ! {self(), {command, Data}},
                    pipe_until_exit(Port, RecvPort, ClosedCount);
                true ->
                    lager:notice("received data from receiving port~n")
            end;
        {'EXIT', Port, Reason} ->
            lager:info("port ~p exited, ~p~n", [Port, Reason])
    end.
