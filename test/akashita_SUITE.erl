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
-module(akashita_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(SPLIT_SIZE, 262144).

init_per_suite(Config) ->
    % ensure lager is configured for testing
    ok = application:set_env(lager, lager_common_test_backend, debug),
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    akashita_app:ensure_schema([node()]),
    ok = application:start(mnesia),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(akashita),
    ok = application:stop(mnesia).

all() ->
    [
        is_go_time_test,
        ensure_archives_test,
        ensure_snapshot_exists_test,
        ensure_clone_exists_test,
        destroy_dataset_test,
        retrieve_tag_test,
        vault_completed_test,
        process_uploads_test
    ].

%% Test the is_go_time/3 function.
is_go_time_test(_Config) ->
    % one window
    ?assert(akashita:is_go_time(["10:05-12:30"], 10, 5)),
    ?assert(akashita:is_go_time(["10:05-12:30"], 10, 15)),
    ?assert(akashita:is_go_time(["10:05-12:30"], 12, 15)),
    ?assert(akashita:is_go_time(["10:05-12:30"], 12, 30)),
    ?assertNot(akashita:is_go_time(["10:05-12:30"], 10, 0)),
    ?assertNot(akashita:is_go_time(["10:05-12:30"], 12, 35)),
    ?assertNot(akashita:is_go_time(["10:05-12:30"], 9, 0)),
    ?assertNot(akashita:is_go_time(["10:05-12:30"], 13, 0)),

    % two windows
    ?assertNot(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 15, 0)),
    ?assertNot(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 23, 35)),
    ?assertNot(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 9, 35)),
    ?assertNot(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 12, 35)),
    ?assertNot(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 15, 59)),
    ?assertNot(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 23, 1)),
    ?assert(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 10, 0)),
    ?assert(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 11, 0)),
    ?assert(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 12, 0)),
    ?assert(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 16, 0)),
    ?assert(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 23, 0)),
    ?assert(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 17, 35)),
    ?assert(akashita:is_go_time(["10:00-12:00", "16:00-23:00"], 11, 59)),

    % backward times
    ?assert(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 22, 0)),
    ?assert(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 6, 0)),
    ?assert(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 10, 0)),
    ?assert(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 16, 0)),
    ?assert(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 23, 0)),
    ?assert(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 12, 0)),
    ?assertNot(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 21, 0)),
    ?assertNot(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 17, 0)),
    ?assertNot(akashita:is_go_time(["22:00-06:00", "10:00-16:00"], 8, 0)),

    % midnight
    ?assert(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 8, 0)),
    ?assert(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 5, 0)),
    ?assert(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 13, 0)),
    ?assert(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 0, 0)),
    ?assert(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 18, 0)),
    ?assertNot(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 15, 0)),
    ?assertNot(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 4, 0)),
    ?assertNot(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 13, 1)),
    ?assertNot(akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 16, 59)),

    % malformed time strings
    ?assertError({badmatch, [[]]}, akashita:is_go_time([""], 0, 0)),
    ?assertError({badmatch, ["05001300"]}, akashita:is_go_time(["05001300"], 0, 0)),
    ?assertError({badmatch, ["0500"]}, akashita:is_go_time(["0500-1300"], 0, 0)),
    ?assertError(badarg, akashita:is_go_time(["aa:bb-cc:dd"], 0, 0)),

    % invalid hours and minutes
    ?assertError(function_clause, akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 25, 59)),
    ?assertError(function_clause, akashita:is_go_time(["05:00-13:00", "17:00-00:00"], -1, 59)),
    ?assertError(function_clause, akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 21, 79)),
    ?assertError(function_clause, akashita:is_go_time(["05:00-13:00", "17:00-00:00"], 21, -30)),

    % invalid hours and minutes in time string
    ?assertError(badarg, akashita:is_go_time(["25:00-13:00"], 0, 0)),
    ?assertError(badarg, akashita:is_go_time(["23:71-13:00"], 0, 0)),
    ?assertError(badarg, akashita:is_go_time(["23:11-72:00"], 0, 0)),
    ?assertError(badarg, akashita:is_go_time(["23:11-13:99"], 0, 0)),
    ok.

% Test the ensure_archives/3 function.
ensure_archives_test(_Config) ->
    % create a temporary directory for the split files
    TmpDir = string:strip(os:cmd("mktemp -d"), right, $\n),
    % sanity check the result of mktemp so we don't clobber random files
    ?assertEqual($/, hd(TmpDir)),
    ct:log(default, 50, "splits dir: ~s", [TmpDir]),
    Cwd = os:getenv("PWD"),
    ct:log(default, 50, "PWD: ~s", [Cwd]),
    SplitSize = integer_to_list(?SPLIT_SIZE),
    % exclude the ever-growing logs directory tree
    Options = [{paths, ["."]}, {dataset, tl(Cwd)}, {excludes, ["logs"]}],
    Vault = "xfiles",
    Tag = "10-14-2005",
    AppConfig = [{split_size, SplitSize}, {tmpdir, TmpDir}, {vaults, [{Vault, Options}]}],
    % create the archives
    SplitsDir = akashita:ensure_archives(Vault, Tag, AppConfig),
    NumSplits1 = verify_split_files(SplitsDir, "xfiles"),
    % call the same function again, should not create any new files
    SplitsDir = akashita:ensure_archives(Vault, Tag, AppConfig),
    NumSplits2 = verify_split_files(SplitsDir, "xfiles"),
    ?assertEqual(NumSplits1, NumSplits2),
    % remove the split files and the temp directory
    os:cmd("rm -rf " ++ TmpDir),
    ok.

% Test the ensure_snapshot_exists/2 function.
ensure_snapshot_exists_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            ct:log("missing 'zfs' in PATH, skipping test..."),
            ok;
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            os:cmd("mkfile 64m " ++ FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            os:cmd("sudo zpool create panzer " ++ FSFile),
            {ok, Snapshot} = akashita:ensure_snapshot_exists("10-14-2005", "panzer"),
            Snapshots = os:cmd("zfs list -H -r -t snapshot panzer@glacier:10-14-2005"),
            [DatasetName|_Rest] = re:split(Snapshots, "\t", [{return, list}]),
            ?assertEqual("panzer@glacier:10-14-2005", DatasetName),
            % do it again to make sure it does not crash, and returns the same name
            {ok, Snapshot} = akashita:ensure_snapshot_exists("10-14-2005", "panzer"),
            os:cmd("sudo zpool destroy panzer"),
            ok = file:delete(FSFile)
    end.

% Verify that the split files are all ?SPLIT_SIZE bytes in size, except the
% last one, which is completely ignored. The split files have Prefix as
% their filename prefix. Returns the number of split files found.
verify_split_files(SplitsDir, Prefix) ->
    {ok, SplitFiles} = file:list_dir(SplitsDir),
    {_LastSplit, SameSizeSplits} = lists:split(1, lists:reverse(lists:sort(SplitFiles))),
    ?assert(length(SameSizeSplits) > 10),
    % we expect the split names to be like "splits00001"
    {ok, MP} = re:compile(Prefix ++ "\\d{5}"),
    CorrectFile = fun(Filename) ->
        case re:run(Filename, MP) of
            {match, _Captured} ->
                Path = filename:join(SplitsDir, Filename),
                {ok, #file_info{size = Size}} = file:read_file_info(Path),
                Size == ?SPLIT_SIZE;
            nomatch -> false
        end
    end,
    ?assert(lists:all(CorrectFile, SameSizeSplits)),
    length(SplitFiles).

% Test the ensure_clone_exists/3 function.
ensure_clone_exists_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            ct:log("missing 'zfs' in PATH, skipping test..."),
            ok;
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            ct:log(os:cmd("mkfile 64m " ++ FSFile)),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            ct:log(os:cmd("sudo zpool create panzer " ++ FSFile)),
            ct:log(os:cmd("zfs snapshot panzer@glacier:10-14-2005")),
            AppConfig = [{use_sudo, true}],
            ok = akashita:ensure_clone_exists(
                "panzer/parts", "panzer@glacier:10-14-2005", AppConfig),
            Datasets = os:cmd("zfs list"),
            ct:log(default, 50, "datasets: ~s", [Datasets]),
            ?assert(string:str(Datasets, "panzer/parts") > 0),
            % do it again to make sure it does not crash
            ok = akashita:ensure_clone_exists(
                "panzer/parts", "panzer@glacier:10-14-2005", AppConfig),
            ct:log(os:cmd("sudo zpool destroy panzer")),
            ok = file:delete(FSFile)
    end.

% Test the destroy_dataset/2 function.
destroy_dataset_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            ct:log("missing 'zfs' in PATH, skipping test..."),
            ok;
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            ct:log(os:cmd("mkfile 64m " ++ FSFile)),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            ct:log(os:cmd("sudo zpool create panzer " ++ FSFile)),
            ct:log(os:cmd("zfs snapshot panzer@glacier:10-14-2005")),
            AppConfig = [{use_sudo, true}],
            ok = akashita:destroy_dataset("panzer@glacier:10-14-2005", AppConfig),
            Datasets = os:cmd("zfs list"),
            ct:log(default, 50, "datasets: ~s", [Datasets]),
            ?assertEqual(0, string:str(Datasets, "panzer@glacier:10-14-2005")),
            ct:log(os:cmd("sudo zpool destroy panzer")),
            ok = file:delete(FSFile)
    end.

% Test the retrieve_tag/0 function.
retrieve_tag_test(_Config) ->
    Tag1 = akashita_app:retrieve_tag(),
    Tag2 = akashita_app:retrieve_tag(),
    ?assertEqual(Tag1, Tag2),
    ok.

% Test the functions related to remembering which vaults have been
% completed. This includes negative tests, positive tests, and clearing the
% cache.
vault_completed_test(_Config) ->
    ?assertNot(akashita_app:is_vault_completed("foo")),
    ok = akashita_app:remember_completed_vault("foo"),
    ?assert(akashita_app:is_vault_completed("foo")),
    ok = akashita_app:remember_completed_vault("bar"),
    ok = akashita_app:remember_completed_vault("baz"),
    ok = akashita_app:remember_completed_vault("quux"),
    ?assert(akashita_app:is_vault_completed("foo")),
    ?assert(akashita_app:is_vault_completed("bar")),
    ?assert(akashita_app:is_vault_completed("baz")),
    ?assert(akashita_app:is_vault_completed("quux")),
    ok = akashita_app:delete_cache(),
    ?assertNot(akashita_app:is_vault_completed("foo")),
    ?assertNot(akashita_app:is_vault_completed("bar")),
    ?assertNot(akashita_app:is_vault_completed("baz")),
    ?assertNot(akashita_app:is_vault_completed("quux")),
    ok.

% Test the akashita_backup application by setting it up to process a vault,
% uploading archives and running to completion.
process_uploads_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            ct:log("missing 'zfs' in PATH, skipping test..."),
            ok;
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            ct:log(os:cmd("mkfile 64m " ++ FSFile)),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            ct:log(os:cmd("sudo zpool create panzer " ++ FSFile)),
            ct:log(os:cmd("sudo zfs create panzer/shared")),
            Cwd = os:getenv("PWD"),
            % copy everything except the logs which contains our 64MB file
            ct:log(os:cmd("sudo rsync --exclude logs -r " ++ Cwd ++ "/* /panzer/shared")),
            BackupLog = filename:join(PrivDir, "backup.log"),
            % set up the application environment to backup our data
            ok = application:set_env(akashita, test_log, BackupLog),
            ok = application:set_env(akashita, use_sudo, true),
            ok = application:set_env(akashita, go_times, ["00:00-23:59"]),
            ok = application:set_env(akashita, tmpdir, PrivDir),
            ok = application:set_env(akashita, split_size, "256K"),
            % ignore the bothersome special directory
            ok = application:set_env(akashita, default_excludes, [".fseventsd"]),
            VaultsConf = [
                {"shared", [
                    {dataset, "panzer/shared"},
                    {clone_base, "panzer/glacier"},
                    {paths, ["."]},
                    {compressed, false}
                ]}
            ],
            ok = application:set_env(akashita, vaults, VaultsConf),
            % fire up the application and wait for it to finish
            {ok, _Started} = application:ensure_all_started(akashita),
            ok = gen_server:call(akashita_backup, begin_backup, infinity),
            % examine the log file to ensure it created a vault and uploaded archives
            {ok, BackupBin} = file:read_file(BackupLog),
            BackupText = binary_to_list(BackupBin),
            ?assert(string:str(BackupText, "vault shared created") > 0),
            ?assert(string:str(BackupText, "shared00000 uploaded") > 0),
            ?assert(string:str(BackupText, "shared00002 uploaded") > 0),
            ?assert(string:str(BackupText, "shared00004 uploaded") > 0),
            ?assert(string:str(BackupText, "shared00006 uploaded") > 0),
            ?assert(string:str(BackupText, "shared00008 uploaded") > 0),
            ?assert(string:str(BackupText, "shared00010 uploaded") > 0),
            ct:log(os:cmd("sudo zpool destroy panzer")),
            ok = file:delete(FSFile)
    end.
