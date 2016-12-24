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
%% The test suite.
%%
-module(akashita_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("enenra/include/enenra.hrl").

-define(SPLIT_SIZE, 65536).
-define(DEFAULT_EXCLUDES, [".VolumeIcon.icns", ".fseventsd", ".Trashes"]).

init_per_suite(Config) ->
    %
    % No attempt is made to automatically remove the test pool prior to the
    % test run. If such a pool exists at the time that we try to create it,
    % we should fail without causing any damage. It is far better to have
    % to clean up after a failed test run than to have someone's data
    % deleted because they had a pool with the same name as ours.
    %
    % ensure lager is configured for testing
    ok = application:set_env(lager, lager_common_test_backend, debug),
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    akashita_app:ensure_schema([node()]),
    ok = application:start(mnesia),
    {ok, _Apps} = application:ensure_all_started(enenra),
    Config.

end_per_suite(_Config) ->
    case application:stop(akashita) of
        ok -> ok;
        {error, {not_started, akashita}} -> ok;
        {error, Reason} -> error(Reason)
    end,
    ok = application:stop(enenra),
    ok = application:stop(mnesia).

all() ->
    Tests = [
        is_go_time_test,
        ensure_objects_test,
        ensure_snapshot_exists_test,
        ensure_clone_exists_test,
        destroy_dataset_test,
        retrieve_tag_test,
        bucket_completed_test,
        process_uploads_test
    ],
    case os:getenv("AKASHITA_LIVE_TEST") of
        false -> Tests;
        _Set  -> Tests ++ [process_uploads_live_test]
    end.

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

% Test the ensure_objects/3 function.
ensure_objects_test(_Config) ->
    % create a temporary directory for the split files
    TmpDir = string:strip(?cmd("mktemp -d"), right, $\n),
    % sanity check the result of mktemp so we don't clobber random files
    ?assertEqual($/, hd(TmpDir)),
    Cwd = os:getenv("PWD"),
    SplitSize = integer_to_list(?SPLIT_SIZE),
    % exclude the typically large build directory tree
    Options = [{paths, ["."]}, {dataset, tl(Cwd)}, {excludes, ["_build"]}],
    Bucket = "xfiles",
    Tag = "10-14-2005",
    AppConfig = [{split_size, SplitSize}, {tmpdir, TmpDir}, {buckets, [{Bucket, Options}]}],
    % create the objects
    SplitsDir = akashita:ensure_objects(Bucket, Tag, AppConfig),
    NumSplits1 = verify_split_files(SplitsDir, "xfiles"),
    % call the same function again, should not create any new files
    SplitsDir = akashita:ensure_objects(Bucket, Tag, AppConfig),
    NumSplits2 = verify_split_files(SplitsDir, "xfiles"),
    ?assertEqual(NumSplits1, NumSplits2),
    % remove the split files and the temp directory
    ?assertCmd("rm -rf " ++ TmpDir),
    ok.

% Test the ensure_snapshot_exists/2 function.
ensure_snapshot_exists_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            error("missing 'zfs' in PATH");
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            mkfile(FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            ?assertCmd("sudo zpool create panzer " ++ FSFile),
            AppConfig = [{use_sudo, true}],
            {ok, Snapshot} = akashita:ensure_snapshot_exists("10-14-2005", "panzer", AppConfig),
            Snapshots = ?cmd("sudo zfs list -H -r -t snapshot panzer@akashita:10-14-2005"),
            [DatasetName|_Rest] = re:split(Snapshots, "\t", [{return, list}]),
            ?assertEqual("panzer@akashita:10-14-2005", DatasetName),
            % do it again to make sure it does not crash, and returns the same name
            {ok, Snapshot} = akashita:ensure_snapshot_exists("10-14-2005", "panzer", AppConfig),
            ?assertCmd("sudo zpool destroy panzer"),
            ok = file:delete(FSFile)
    end.

% Verify that the split files are all ?SPLIT_SIZE bytes in size, except the
% last one, which is completely ignored. The split files have Prefix as
% their filename prefix. Returns the number of split files found.
verify_split_files(SplitsDir, Prefix) ->
    {ok, SplitFiles} = file:list_dir(SplitsDir),
    {_LastSplit, SameSizeSplits} = lists:split(1, lists:reverse(lists:sort(SplitFiles))),
    ?assert(length(SameSizeSplits) > 4),
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
            error("missing 'zfs' in PATH");
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            mkfile(FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            ?assertCmd("sudo zpool create panzer " ++ FSFile),
            ?assertCmd("sudo zfs snapshot panzer@akashita:10-14-2005"),
            AppConfig = [{use_sudo, true}],
            ok = akashita:ensure_clone_exists(
                "panzer/parts", "panzer@akashita:10-14-2005", AppConfig),
            Datasets = ?cmd("sudo zfs list"),
            ?assert(string:str(Datasets, "panzer/parts") > 0),
            % do it again to make sure it does not crash
            ok = akashita:ensure_clone_exists(
                "panzer/parts", "panzer@akashita:10-14-2005", AppConfig),
            ?assertCmd("sudo zpool destroy panzer"),
            ok = file:delete(FSFile)
    end.

% Test the destroy_dataset/2 function.
destroy_dataset_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            error("missing 'zfs' in PATH");
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            mkfile(FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            ?assertCmd("sudo zpool create panzer " ++ FSFile),
            ?assertCmd("sudo zfs snapshot panzer@akashita:10-14-2005"),
            AppConfig = [{use_sudo, true}],
            ok = akashita:destroy_dataset("panzer@akashita:10-14-2005", AppConfig),
            Datasets = ?cmd("sudo zfs list"),
            ?assertEqual(0, string:str(Datasets, "panzer@akashita:10-14-2005")),
            ?assertCmd("sudo zpool destroy panzer"),
            ok = file:delete(FSFile)
    end.

% Test the retrieve_tag/0 function.
retrieve_tag_test(_Config) ->
    Tag1 = akashita_app:retrieve_tag(),
    Tag2 = akashita_app:retrieve_tag(),
    ?assertEqual(Tag1, Tag2),
    ok.

% Test the functions related to remembering which buckets have been
% completed. This includes negative tests, positive tests, and clearing the
% cache.
bucket_completed_test(_Config) ->
    ?assertNot(akashita_app:is_bucket_completed("foo")),
    ok = akashita_app:remember_completed_bucket("foo"),
    ?assert(akashita_app:is_bucket_completed("foo")),
    ok = akashita_app:remember_completed_bucket("bar"),
    ok = akashita_app:remember_completed_bucket("baz"),
    ok = akashita_app:remember_completed_bucket("quux"),
    ?assert(akashita_app:is_bucket_completed("foo")),
    ?assert(akashita_app:is_bucket_completed("bar")),
    ?assert(akashita_app:is_bucket_completed("baz")),
    ?assert(akashita_app:is_bucket_completed("quux")),
    ok = akashita_app:delete_cache(),
    ?assertNot(akashita_app:is_bucket_completed("foo")),
    ?assertNot(akashita_app:is_bucket_completed("bar")),
    ?assertNot(akashita_app:is_bucket_completed("baz")),
    ?assertNot(akashita_app:is_bucket_completed("quux")),
    ok.

% Test the akashita_backup application by setting it up to process a bucket,
% uploading objects and running to completion.
process_uploads_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            error("missing 'zfs' in PATH");
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            mkfile(FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind.
            % Specify a mount point to avoid unexpected defaults (e.g. ZFS on Mac).
            ?assertCmd("sudo zpool create -m /panzer panzer " ++ FSFile),
            ?assertCmd("sudo zfs create panzer/shared"),
            ?assertCmd("sudo chmod 777 /panzer/shared"),
            ?assertCmd("sudo zfs create panzer/photos"),
            ?assertCmd("sudo chmod 777 /panzer/photos"),
            Cwd = os:getenv("PWD"),
            % copy everything except the test directory which contains our 64MB file
            ?assertCmd("rsync --exclude=_build -r " ++ Cwd ++ "/* /panzer/shared"),
            % copy something to the photos "bucket", that will result in a single object
            {ok, _BC} = file:copy(filename:join(Cwd, "test/akashita_SUITE.erl"),
                "/panzer/photos/akashita_SUITE.erl"),
            BackupLog = filename:join(PrivDir, "backup.log"),
            % load the application so we can override the config params
            ok = application:load(akashita),
            % set up the application environment to backup our data
            ok = application:set_env(akashita, test_log, BackupLog),
            ok = application:set_env(akashita, use_sudo, true),
            % test in which nothing will happen because it's not the right time
            ok = application:set_env(akashita, go_times, ["23:59-00:00"]),
            ok = application:set_env(akashita, tmpdir, PrivDir),
            ok = application:set_env(akashita, split_size, "64K"),
            % ignore the bothersome special directories and files
            ok = application:set_env(akashita, default_excludes, ?DEFAULT_EXCLUDES),
            BucketsConf = [
                {"shared", [
                    {dataset, "panzer/shared"},
                    {clone_base, "panzer/akashita"},
                    {paths, ["."]},
                    {compressed, false}
                ]},
                {"photos", [
                    {dataset, "panzer/photos"},
                    {clone_base, "panzer/akashita"},
                    {paths, ["."]},
                    {compressed, true}
                ]}
            ],
            ok = application:set_env(akashita, buckets, BucketsConf),
            % fire up the application and wait for it to finish
            {ok, _Started} = application:ensure_all_started(akashita),
            ok = gen_server:call(akashita_backup, test_backup),
            % verify nothing happened (because of go_times)
            {error, Reason} = file:read_file_info(BackupLog),
            ?assertEqual(enoent, Reason),
            % correct the time and try again, this time something should happen
            ok = application:set_env(akashita, go_times, ["00:00-23:59"]),
            ok = gen_server:call(akashita_backup, test_backup),
            wait_for_backup_to_finish(),
            % examine the log file to ensure it created a bucket and uploaded files
            {ok, BackupBin} = file:read_file(BackupLog),
            BackupText = binary_to_list(BackupBin),
            Tag = akashita_app:retrieve_tag(),
            % check that the buckets were created...
            BucketNames = proplists:get_keys(BucketsConf),
            BucketRegexs = [bucket_regex(Name, Tag) || Name <- BucketNames],
            FindBucketsCreated = fun(BucketRegex) ->
                ?assert(case re:run(BackupText, BucketRegex) of
                    {match, _Captured} -> true;
                    nomatch -> false
                end)
            end,
            lists:foreach(FindBucketsCreated, BucketRegexs),
            % check that the objects were uploaded...
            SharedObjects = [lists:flatten(io_lib:format("shared~5.10.0B", [I])) || I <- lists:seq(0, 1)],
            FindObjectsUploaded = fun(ObjectName) ->
                E = lists:flatten(io_lib:format("~s uploaded to \\w{26}-shared-~s", [ObjectName, Tag])),
                ?assert(case re:run(BackupText, E) of
                    {match, _Captured} -> true;
                    nomatch -> false
                end)
            end,
            lists:foreach(FindObjectsUploaded, SharedObjects),
            % check that only one photos object was uploaded
            Regex1 = io_lib:format("photos00001 uploaded to \\w{26}-photos-~s", [Tag]),
            ?assert(case re:run(BackupText, lists:flatten(Regex1)) of
                % should _not_ match...
                {match, _Captured} -> false;
                nomatch -> true
            end),
            ?assertCmd("sudo zpool destroy panzer"),
            ok = file:delete(FSFile)
    end.

% Test the full backup process, including uploading content to the cloud.
process_uploads_live_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            error("missing 'zfs' in PATH"),
            ok;
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            FSFile = filename:join(PrivDir, "tank_file"),
            mkfile(FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind.
            % Specify a mount point to avoid unexpected defaults (e.g. ZFS on Mac).
            ?assertCmd("sudo zpool create -m /panzer panzer " ++ FSFile),
            ?assertCmd("sudo zfs create panzer/shared"),
            ?assertCmd("sudo chmod 777 /panzer/shared"),
            ?assertCmd("sudo zfs create panzer/photos"),
            ?assertCmd("sudo chmod 777 /panzer/photos"),
            Cwd = os:getenv("PWD"),
            % copy everything except the test directory which contains our 64MB file
            ?assertCmd("rsync --exclude=_build -r " ++ Cwd ++ "/* /panzer/shared"),
            % copy something to the photos "bucket", that will result in a single object
            {ok, _BC} = file:copy(filename:join(Cwd, "test/akashita_SUITE.erl"),
                "/panzer/photos/akashita_SUITE.erl"),
            % application already loaded...
            % ok = application:load(akashita),
            % set application environment to backup data to the cloud
            ok = application:unset_env(akashita, test_log),
            ok = application:set_env(akashita, gcs_region, get_env("GCS_REGION")),
            ok = application:set_env(akashita, gcp_credentials, get_env("GCP_CREDENTIALS")),
            ok = application:set_env(akashita, use_sudo, true),
            ok = application:set_env(akashita, tmpdir, PrivDir),
            ok = application:set_env(akashita, go_times, ["00:00-23:59"]),
            ok = application:set_env(akashita, split_size, "64K"),
            % ignore the bothersome special directories and files
            ok = application:set_env(akashita, default_excludes, ?DEFAULT_EXCLUDES),
            BucketsConf = [
                {"shared", [
                    {dataset, "panzer/shared"},
                    {clone_base, "panzer/akashita"},
                    {paths, ["."]},
                    {compressed, false}
                ]},
                {"photos", [
                    {dataset, "panzer/photos"},
                    {clone_base, "panzer/akashita"},
                    {paths, ["."]},
                    {compressed, true}
                ]}
            ],
            ok = application:set_env(akashita, buckets, BucketsConf),
            % fire up the application and wait for it to finish
            {ok, _Started} = application:ensure_all_started(akashita),
            ok = gen_server:call(akashita_backup, test_backup),
            % while that's running, get the cloud names of the buckets
            Tag = akashita_app:retrieve_tag(),
            SharedRemoteName = akashita_app:retrieve_bucket_name("shared", Tag),
            PhotosRemoteName = akashita_app:retrieve_bucket_name("photos", Tag),
            wait_for_backup_to_finish(),
            % verify that the objects were uploaded
            SharedObjects = list_bucket(SharedRemoteName),
            ct:log(default, 50, "shared objects: ~s", [SharedObjects]),
            ?assert(length(SharedObjects) > 2),
            ?assertEqual(<<"shared00000">>, hd(SharedObjects)),
            PhotosObjects = list_bucket(PhotosRemoteName),
            ?assertEqual(1, length(PhotosObjects)),
            ?assertEqual(<<"photos00000">>, hd(PhotosObjects)),
            ?assertCmd("sudo zpool destroy panzer"),
            ok = file:delete(FSFile)
    end.

% Retrieve an environment variable, ensuring it is defined.
get_env(Name) ->
    case os:getenv(Name) of
        false ->
            error(lists:flatten(io_lib:format("must define ~p environment variable", [Name])));
        Value -> Value
    end.

% Run the mkfile command (or its Linux equivalent) to create a temporary
% filesytem for ZFS to use as a storage pool.
mkfile(FSFile) ->
    case os:find_executable("mkfile") of
        false ->
            % Hipster Linux doesn't use your grandfather's mkfile...
            case os:find_executable("fallocate") of
                false ->
                    error("need either 'mkfile' or 'fallocate' to run tests");
                FBin ->
                    ?assertCmd(FBin ++ " -l 64M " ++ FSFile)
            end;
        MBin ->
            ?assertCmd(MBin ++ " 64m " ++ FSFile)
    end,
    ok.

wait_for_backup_to_finish() ->
    receive
        backup_finished -> ok
    end.

% Retrieve the contents of the named Bucket as a list of strings.
list_bucket(Bucket) when is_list(Bucket) ->
    list_bucket(list_to_binary(Bucket));
list_bucket(Bucket) when is_binary(Bucket) ->
    {ok, Credentials} = application:get_env(akashita, gcp_credentials),
    {ok, Creds} = enenra:load_credentials(Credentials),
    {ok, Objects} = enenra:list_objects(Bucket, Creds),
    lists:sort([O#object.name || O <- Objects]).

% Produce a compiled regex to match the "created" log entry for the given
% bucket and tag.
bucket_regex(Bucket, Tag) ->
    % ULID string representation is 26 characters long.
    {ok, MP} = re:compile("bucket \\w{26}-" ++ Bucket ++ "-" ++ Tag ++ " created"),
    MP.
