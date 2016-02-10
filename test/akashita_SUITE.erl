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

init_per_suite(Config) ->
    % ensure lager is configured for testing
    ok = application:set_env(lager, lager_common_test_backend, debug),
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [
        is_go_time_test,
        create_archives_test
    ].

%% Test the is_go_time/3 function.
is_go_time_test(_Config) ->
    % one window
    ?assert(akashita:is_go_time("10:05-12:30", 10, 5)),
    ?assert(akashita:is_go_time("10:05-12:30", 10, 15)),
    ?assert(akashita:is_go_time("10:05-12:30", 12, 15)),
    ?assert(akashita:is_go_time("10:05-12:30", 12, 30)),
    ?assertNot(akashita:is_go_time("10:05-12:30", 10, 0)),
    ?assertNot(akashita:is_go_time("10:05-12:30", 12, 35)),
    ?assertNot(akashita:is_go_time("10:05-12:30", 9, 0)),
    ?assertNot(akashita:is_go_time("10:05-12:30", 13, 0)),

    % two windows
    ?assertNot(akashita:is_go_time("10:00-12:00,16:00-23:00", 15, 0)),
    ?assertNot(akashita:is_go_time("10:00-12:00,16:00-23:00", 23, 35)),
    ?assertNot(akashita:is_go_time("10:00-12:00,16:00-23:00", 9, 35)),
    ?assertNot(akashita:is_go_time("10:00-12:00,16:00-23:00", 12, 35)),
    ?assertNot(akashita:is_go_time("10:00-12:00,16:00-23:00", 15, 59)),
    ?assertNot(akashita:is_go_time("10:00-12:00,16:00-23:00", 23, 1)),
    ?assert(akashita:is_go_time("10:00-12:00,16:00-23:00", 10, 0)),
    ?assert(akashita:is_go_time("10:00-12:00,16:00-23:00", 11, 0)),
    ?assert(akashita:is_go_time("10:00-12:00,16:00-23:00", 12, 0)),
    ?assert(akashita:is_go_time("10:00-12:00,16:00-23:00", 16, 0)),
    ?assert(akashita:is_go_time("10:00-12:00,16:00-23:00", 23, 0)),
    ?assert(akashita:is_go_time("10:00-12:00,16:00-23:00", 17, 35)),
    ?assert(akashita:is_go_time("10:00-12:00,16:00-23:00", 11, 59)),

    % backward times
    ?assert(akashita:is_go_time("22:00-06:00,10:00-16:00", 22, 0)),
    ?assert(akashita:is_go_time("22:00-06:00,10:00-16:00", 6, 0)),
    ?assert(akashita:is_go_time("22:00-06:00,10:00-16:00", 10, 0)),
    ?assert(akashita:is_go_time("22:00-06:00,10:00-16:00", 16, 0)),
    ?assert(akashita:is_go_time("22:00-06:00,10:00-16:00", 23, 0)),
    ?assert(akashita:is_go_time("22:00-06:00,10:00-16:00", 12, 0)),
    ?assertNot(akashita:is_go_time("22:00-06:00,10:00-16:00", 21, 0)),
    ?assertNot(akashita:is_go_time("22:00-06:00,10:00-16:00", 17, 0)),
    ?assertNot(akashita:is_go_time("22:00-06:00,10:00-16:00", 8, 0)),

    % midnight
    ?assert(akashita:is_go_time("05:00-13:00,17:00-00:00", 8, 0)),
    ?assert(akashita:is_go_time("05:00-13:00,17:00-00:00", 5, 0)),
    ?assert(akashita:is_go_time("05:00-13:00,17:00-00:00", 13, 0)),
    ?assert(akashita:is_go_time("05:00-13:00,17:00-00:00", 0, 0)),
    ?assert(akashita:is_go_time("05:00-13:00,17:00-00:00", 18, 0)),
    ?assertNot(akashita:is_go_time("05:00-13:00,17:00-00:00", 15, 0)),
    ?assertNot(akashita:is_go_time("05:00-13:00,17:00-00:00", 4, 0)),
    ?assertNot(akashita:is_go_time("05:00-13:00,17:00-00:00", 13, 1)),
    ?assertNot(akashita:is_go_time("05:00-13:00,17:00-00:00", 16, 59)),

    % malformed time strings
    ?assertError({badmatch, [[]]}, akashita:is_go_time("", 0, 0)),
    ?assertError({badmatch, ["05001300"]}, akashita:is_go_time("05001300", 0, 0)),
    ?assertError({badmatch, ["0500"]}, akashita:is_go_time("0500-1300", 0, 0)),
    ?assertError(badarg, akashita:is_go_time("aa:bb-cc:dd", 0, 0)),

    % invalid hours and minutes
    ?assertError(function_clause, akashita:is_go_time("05:00-13:00,17:00-00:00", 25, 59)),
    ?assertError(function_clause, akashita:is_go_time("05:00-13:00,17:00-00:00", -1, 59)),
    ?assertError(function_clause, akashita:is_go_time("05:00-13:00,17:00-00:00", 21, 79)),
    ?assertError(function_clause, akashita:is_go_time("05:00-13:00,17:00-00:00", 21, -30)),

    % invalid hours and minutes in time string
    ?assertError(badarg, akashita:is_go_time("25:00-13:00", 0, 0)),
    ?assertError(badarg, akashita:is_go_time("23:71-13:00", 0, 0)),
    ?assertError(badarg, akashita:is_go_time("23:11-72:00", 0, 0)),
    ?assertError(badarg, akashita:is_go_time("23:11-13:99", 0, 0)),
    ok.

% Test the create_archives/4 function.
create_archives_test(_Config) ->
    % create a temporary directory for the split files
    TempDir = string:strip(os:cmd("mktemp -d"), right, $\n),
    Cwd = os:getenv("PWD"),
    ?assertEqual(ok, akashita:create_archives(["."], "splits", Cwd, TempDir)),
    % TODO: verify that there are at least N split files in TempDir
    % TODO: remove the split files and the temp directory
    ok.
