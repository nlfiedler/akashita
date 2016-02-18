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

-export([main/1, is_go_time/3]).
-export([ensure_archives/3]).
-export([ensure_clone_exists/3, ensure_snapshot_exists/2]).
-export([destroy_dataset/2]).
-export([ensure_vault_created/1, upload_archive/3]).

main(_Args) ->
    io:format("Starting backup process...~n"),
    {ok, _Started} = application:ensure_all_started(akashita),
    % get the backup process started immediately
    ok = gen_server:cast(akashita_backup, process),
    % block forever (this process is not expected to receive messages)
    receive
        _ -> ok
    end.

% Determine if given time falls within upload window(s). Windows is a list
% of strings in HH:MM-HH:MM format. The hours are 24-hour. The times can
% span midnight, if needed.
is_go_time(Windows, Hour, Minute)
        when Hour >= 0 andalso Hour < 24
        andalso Minute >= 0 andalso Minute < 60 ->
    TheTime = {Hour, Minute},
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
    lists:any(InWindow, Windows).

% Ensure the named Vault has been created.
ensure_vault_created(Vault) ->
    case application:get_env(akashita, test_log) of
        undefined ->
            PrivPath = code:priv_dir(akashita_backup),
            Cmd = filename:join(PrivPath, "klutlan"),
            Args = ["-create", "-vault", Vault],
            Port = erlang:open_port({spawn_executable, Cmd}, [exit_status, {args, Args}]),
            {ok, 0} = wait_for_port(Port),
            ok;
        {ok, LogFile} ->
            % in test mode, write to a log file
            {ok, IoDevice} = file:open(LogFile, [append]),
            Record = io_lib:format("vault ~s created\n", [Vault]),
            ok = file:write(IoDevice, list_to_binary(Record)),
            ok = file:close(IoDevice)
    end.

% Upload a single archive file, retrying as needed.
upload_archive(Archive, Desc, Vault) ->
    case application:get_env(akashita, test_log) of
        undefined ->
            PrivPath = code:priv_dir(akashita_backup),
            Cmd = filename:join(PrivPath, "klutlan"),
            Args = ["-upload", Archive, "-desc", Desc, "-vault", Vault],
            Port = erlang:open_port({spawn_executable, Cmd}, [exit_status, {args, Args}]),
            case wait_for_port(Port) of
                {ok, 0} -> ok;
                {ok, _C} ->
                    % keep trying until it works or we get an error
                    upload_archive(Archive, Desc, Vault);
                {error, Reason} ->
                    lager:error("upload archive ~s failed, ~s", [Archive, Reason]),
                    error(Reason)
            end;
        {ok, LogFile} ->
            % in test mode, write to a log file
            {ok, IoDevice} = file:open(LogFile, [append]),
            Record = io_lib:format("archive ~s uploaded\n", [Archive]),
            ok = file:write(IoDevice, list_to_binary(Record)),
            ok = file:close(IoDevice)
    end.

% Create the ZFS snapshot, if it is missing, where Name is the snapshot
% name, and Dataset is the name of the zfs dataset for which a snapshot
% will be created. Returns {ok, SnapshotName} on success.
ensure_snapshot_exists(Name, Dataset) ->
    Snapshot = io_lib:format("~s@glacier:~s", [Dataset, Name]),
    case os:find_executable("zfs") of
        false ->
            lager:info("missing 'zfs' in PATH"),
            error(missing_zfs);
        ZfsBin ->
            ListPort = erlang:open_port({spawn_executable, ZfsBin},
                [exit_status, {args, ["list", "-H", Snapshot]}]),
            case wait_for_port(ListPort, true) of
                {ok, 0} ->
                    % zfs snapshot already exists
                    ok;
                {ok, _C} ->
                    % create the zfs snapshot (does not require sudo)
                    lager:info("creating zfs snapshot ~s", [Snapshot]),
                    SnapArgs = ["snapshot", "-o", "com.sun:auto-snapshot=false", Snapshot],
                    SnapPort = erlang:open_port({spawn_executable, ZfsBin},
                        [exit_status, {args, SnapArgs}]),
                    {ok, 0} = wait_for_port(SnapPort),
                    lager:info("zfs snapshot ~s created", [Snapshot])
            end,
            {ok, Snapshot}
    end.

% Ensure that the named zfs Clone exists for the given Snapshot. Returns ok.
ensure_clone_exists(Clone, Snapshot, Config) ->
    case os:find_executable("zfs") of
        false ->
            lager:info("missing 'zfs' in PATH"),
            error(missing_zfs);
        ZfsBin ->
            ListPort = erlang:open_port({spawn_executable, ZfsBin},
                [exit_status, {args, ["list", "-H", Clone]}]),
            case wait_for_port(ListPort, true) of
                {ok, 0} ->
                    % zfs clone already exists
                    ok;
                {ok, _C} ->
                    % create the zfs clone
                    lager:info("creating zfs clone ~s of ~s", [Clone, Snapshot]),
                    CloneArgs = [
                        "clone",
                        "-o", "com.sun:auto-snapshot=false",
                        "-p",
                        Snapshot,
                        Clone
                    ],
                    % creating a clone may require sudo
                    ClonePort = case proplists:get_bool(use_sudo, Config) of
                        false ->
                            erlang:open_port({spawn_executable, ZfsBin},
                                [exit_status, {args, CloneArgs}]);
                        true ->
                            case os:find_executable("sudo") of
                                false ->
                                    lager:info("missing 'sudo' in PATH"),
                                    error(missing_sudo);
                                SudoBin ->
                                    erlang:open_port({spawn_executable, SudoBin},
                                        [exit_status, {args, [ZfsBin] ++ CloneArgs}])
                            end
                    end,
                    {ok, 0} = wait_for_port(ClonePort),
                    lager:info("zfs clone ~s created", [Clone]),
                    ok
            end
    end.

% Destroy the named zfs dataset. The Config indicates if sudo is needed or
% not. Returns ok on success (raises error otherwise).
destroy_dataset(Dataset, Config) ->
    case os:find_executable("zfs") of
        false ->
            lager:info("missing 'zfs' in PATH"),
            error(missing_zfs);
        ZfsBin ->
            % destroying a dataset may require sudo
            ZfsArgs = ["destroy", Dataset],
            ZfsPort = case proplists:get_bool(use_sudo, Config) of
                false ->
                    erlang:open_port({spawn_executable, ZfsBin},
                        [exit_status, {args, ZfsArgs}]);
                true ->
                    case os:find_executable("sudo") of
                        false ->
                            lager:info("missing 'sudo' in PATH"),
                            error(missing_sudo);
                        SudoBin ->
                            erlang:open_port({spawn_executable, SudoBin},
                                [exit_status, {args, [ZfsBin] ++ ZfsArgs}])
                    end
            end,
            {ok, 0} = wait_for_port(ZfsPort),
            lager:info("zfs destroy ~s successful", [Dataset]),
            ok
    end.

% Ensure the archives are created for the named Vault, with the Tag as the
% suffix of the working directory where the archives will be created. The
% Config is a proplist taken from the application configuration. Returns
% the path of the generated archives.
ensure_archives(Vault, Tag, Config) ->
    WorkDir = proplists:get_value(tmpdir, Config),
    ArchiveDir = filename:join(WorkDir, io_lib:format("~s-~s", [Vault, Tag])),
    CreateArchives = fun() ->
        VaultList = proplists:get_value(vaults, Config),
        VaultConf = proplists:get_value(Vault, VaultList),
        SplitSize = proplists:get_value(split_size, Config, "64M"),
        DefaultExcludes = proplists:get_value(default_excludes, Config, []),
        create_archives(Vault, ArchiveDir, SplitSize, VaultConf, DefaultExcludes)
    end,
    EnsureArchives = fun() ->
        case file:list_dir(ArchiveDir) of
            {error, enoent} ->
                ok = filelib:ensure_dir(ArchiveDir),
                ok = file:make_dir(ArchiveDir),
                CreateArchives();
            {ok, []} -> CreateArchives();
            {ok, _Filenames} -> ok  % files exist, nothing to do
        end
    end,
    % ensure the target directory exists, and if it is empty, create the archives
    try EnsureArchives() of
        ok -> ArchiveDir
    catch
        error:Error ->
            lager:error("error creating archives: ~w", [Error]),
            os:cmd("rm -rf " ++ ArchiveDir),
            error(Error)
    end.

% Produce the tar archives (split into reasonably sized files) for the list
% of 'paths' defined in the Options proplist (relative to the 'dataset'
% directory, also defined in Options), with the split files having the
% given Vault name as a prefix. The split files will be created in the
% SplitDir directory.
create_archives(Vault, SplitDir, SplitSize, Options, DefaultExcludes) ->
    lager:info("generating tar archives..."),
    Paths = proplists:get_value(paths, Options),
    SourceDir = "/" ++ proplists:get_value(dataset, Options),
    Compress = proplists:get_bool(compress, Options),
    Exclusions = proplists:get_value(excludes, Options, DefaultExcludes),
    TarCmd = string:join(tar_cmd(Paths, Compress, Exclusions), " "),
    TarPort = erlang:open_port({spawn, TarCmd}, [exit_status, binary, {cd, SourceDir}]),
    SplitCmd = string:join(split_cmd(Vault, SplitSize), " "),
    SplitPort = erlang:open_port({spawn, SplitCmd}, [exit_status, binary, {cd, SplitDir}]),
    % start with a ClosedCount of 1, otherwise split hangs indefinitely
    {ok, 0} = pipe_until_exit(TarPort, SplitPort, 1),
    % close the ports so the programs know to terminate (especially split)
    ensure_port_closed(TarPort),
    ensure_port_closed(SplitPort),
    lager:info("tar archive creation complete"),
    ok.

% Generate the command to invoke tar for the given set of file paths.
tar_cmd(Paths, Compress, Exclusions) ->
    Copt = case Compress of
        true -> ["-j"];
        false -> []
    end,
    Eopt = case Exclusions of
        [] -> [];
        _ -> ["--exclude " ++ E || E <- Exclusions]
    end,
    % Need the "-f -" for bsdtar, otherwise it attempts to use the default
    % tape drive device (/dev/sa0).
    ["tar", "-f", "-", "-c"] ++ Copt ++ Eopt ++ Paths.

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
        {SendPort, {data, Data}} ->
            RecvPort ! {self(), {command, Data}},
            pipe_until_exit(SendPort, RecvPort, ClosedCount);
        {RecvPort, {data, _Data}} ->
            lager:notice("received data from receiving port"),
            pipe_until_exit(SendPort, RecvPort, ClosedCount);
        {'EXIT', Port, Reason} ->
            lager:info("port ~w exited, ~w", [Port, Reason])
    end.

% Wait for the given Port to complete and return the exit code in the form
% of {ok, Status}. Any output received is written to the log. If the port
% experiences an error, returns {error, Reason}.
wait_for_port(Port) ->
    wait_for_port(Port, false).

% Wait for the given Port to complete and return the exit code in the form
% of {ok, Status}. Any output received is written to the log. If the port
% experiences an error, returns {error, Reason}. If Quiet is true, output
% from the port is ignored.
wait_for_port(Port, Quiet) when is_boolean(Quiet) ->
    receive
        {Port, {exit_status, Status}} ->
            ensure_port_closed(Port),
            {ok, Status};
        {Port, {data, Data}} ->
            if Quiet -> lager:notice("output from port ignored...");
                true -> lager:notice("received output from port: ~s", [Data])
            end,
            wait_for_port(Port);
        {'EXIT', Port, Reason} ->
            lager:info("port ~w exited, ~w", [Port, Reason]),
            {error, Reason}
    end.

% Ensure that the given Port has been properly closed. Does nothing if the
% port is not open.
ensure_port_closed(Port) ->
    case erlang:port_info(Port) of
        undefined -> ok;
        _         -> erlang:port_close(Port)
    end.
