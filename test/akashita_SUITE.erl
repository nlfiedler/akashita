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
        is_go_time_test
    ].

%% Test the is_go_time/3 function.
is_go_time_test(_Config) ->
    % one window
    ?assertEqual(true, akashita:is_go_time("10:05-12:30", 10, 5)),
    ?assertEqual(true, akashita:is_go_time("10:05-12:30", 10, 15)),
    ?assertEqual(true, akashita:is_go_time("10:05-12:30", 12, 15)),
    ?assertEqual(true, akashita:is_go_time("10:05-12:30", 12, 30)),
    ?assertEqual(false, akashita:is_go_time("10:05-12:30", 10, 0)),
    ?assertEqual(false, akashita:is_go_time("10:05-12:30", 12, 35)),
    ?assertEqual(false, akashita:is_go_time("10:05-12:30", 9, 0)),
    ?assertEqual(false, akashita:is_go_time("10:05-12:30", 13, 0)),

    % two windows
    ?assertEqual(false, akashita:is_go_time("10:00-12:00,16:00-23:00", 15, 0)),
    ?assertEqual(false, akashita:is_go_time("10:00-12:00,16:00-23:00", 23, 35)),
    ?assertEqual(false, akashita:is_go_time("10:00-12:00,16:00-23:00", 9, 35)),
    ?assertEqual(false, akashita:is_go_time("10:00-12:00,16:00-23:00", 12, 35)),
    ?assertEqual(false, akashita:is_go_time("10:00-12:00,16:00-23:00", 15, 59)),
    ?assertEqual(false, akashita:is_go_time("10:00-12:00,16:00-23:00", 23, 1)),
    ?assertEqual(true, akashita:is_go_time("10:00-12:00,16:00-23:00", 10, 0)),
    ?assertEqual(true, akashita:is_go_time("10:00-12:00,16:00-23:00", 11, 0)),
    ?assertEqual(true, akashita:is_go_time("10:00-12:00,16:00-23:00", 12, 0)),
    ?assertEqual(true, akashita:is_go_time("10:00-12:00,16:00-23:00", 16, 0)),
    ?assertEqual(true, akashita:is_go_time("10:00-12:00,16:00-23:00", 23, 0)),
    ?assertEqual(true, akashita:is_go_time("10:00-12:00,16:00-23:00", 17, 35)),
    ?assertEqual(true, akashita:is_go_time("10:00-12:00,16:00-23:00", 11, 59)),

    % backward times
    ?assertEqual(true, akashita:is_go_time("22:00-06:00,10:00-16:00", 22, 0)),
    ?assertEqual(true, akashita:is_go_time("22:00-06:00,10:00-16:00", 6, 0)),
    ?assertEqual(true, akashita:is_go_time("22:00-06:00,10:00-16:00", 10, 0)),
    ?assertEqual(true, akashita:is_go_time("22:00-06:00,10:00-16:00", 16, 0)),
    ?assertEqual(true, akashita:is_go_time("22:00-06:00,10:00-16:00", 23, 0)),
    ?assertEqual(true, akashita:is_go_time("22:00-06:00,10:00-16:00", 12, 0)),
    ?assertEqual(false, akashita:is_go_time("22:00-06:00,10:00-16:00", 21, 0)),
    ?assertEqual(false, akashita:is_go_time("22:00-06:00,10:00-16:00", 17, 0)),
    ?assertEqual(false, akashita:is_go_time("22:00-06:00,10:00-16:00", 8, 0)),

    % midnight
    ?assertEqual(true, akashita:is_go_time("05:00-13:00,17:00-00:00", 8, 0)),
    ?assertEqual(true, akashita:is_go_time("05:00-13:00,17:00-00:00", 5, 0)),
    ?assertEqual(true, akashita:is_go_time("05:00-13:00,17:00-00:00", 13, 0)),
    ?assertEqual(true, akashita:is_go_time("05:00-13:00,17:00-00:00", 0, 0)),
    ?assertEqual(true, akashita:is_go_time("05:00-13:00,17:00-00:00", 18, 0)),
    ?assertEqual(false, akashita:is_go_time("05:00-13:00,17:00-00:00", 15, 0)),
    ?assertEqual(false, akashita:is_go_time("05:00-13:00,17:00-00:00", 4, 0)),
    ?assertEqual(false, akashita:is_go_time("05:00-13:00,17:00-00:00", 13, 1)),
    ?assertEqual(false, akashita:is_go_time("05:00-13:00,17:00-00:00", 16, 59)),
    ok.
