%%%----------------------------------------------------------------------
%%% File    : mod_platypus_api.erl
%%% Author  : Jimmy Zöger <jimmy.zoger@erlang-solutions.com>
%%% Purpose : API wrapper for Platypus, a push notification server
%%%
%%%
%%% Copyright (C) 2016  Erlang Solutions
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_platypus_api).

-behaviour(gen_mod).
-behaviour(gen_push_server_api).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% API
-export([add_device/3,
         send_push/3]).

-include("ejabberd.hrl").
-include("mod_push.hrl").

%%%----------------------------------------------------------------------
%%% Macro definitions
%%%----------------------------------------------------------------------

-define(PROTOCOL, "http").
-define(HOST, "localhost").
-define(PORT, "8888").
-define(URL_PREFIX, [?PROTOCOL, "://", ?HOST, ":", ?PORT]).

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

%% TODO: Replace macros with configuration values from Opts
start(_Host, _Opts) ->
    ibrowse:set_max_pipeline_size(?HOST, ?PORT, 100),
    ibrowse:set_max_sessions(?HOST, ?PORT, 100).

stop(_Host) ->
    ok.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec(add_device/3 ::
(
    Username :: binary(),
    Type :: binary() | atom(),
    Token :: binary())
    -> ok | {error, Reason :: any()}
).
add_device(Username, Type, Token) ->
    Url = to_flat_string([?URL_PREFIX, "/users/", Username, "/devices/", Token]),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Body = [<<"{\"device_type\": \"">>, to_string(Type), <<"\"}">>],
    case ibrowse:send_req(Url, Headers, put, Body) of
        {ok, "201", _, _} ->
            ?DEBUG("Device for ~p added to Platypus, type: ~p", [Username, Type]);
        {ok, "204", _, _} -> % If the device was already added cowboy returns 204
            ?DEBUG("Device for ~p added to Platypus, type: ~p", [Username, Type]);
        {ok, Status, _, RespBody} ->
            ?ERROR_MSG("Device for ~p could not be added, Platypus returned status ~p, body: ~p",
                       [Username, Status, RespBody]);
        {error, Reason} ->
            ?ERROR_MSG("Error when trying to add device for ~p, reason: ~p",
                       [Username, Reason]),
            {error, Reason}
    end.

%%-----------------------------------------------------------------------

-spec(send_push/3 ::
(
    Username :: binary(),
    MsgSender :: binary(),
    MsgBody :: binary())
    -> ok | {error, Reason :: any()}
).
send_push(Username, MsgSender, MsgBody) ->
    Url = to_flat_string([?URL_PREFIX, "/notifications"]),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Msg = << MsgSender/binary, ": ", MsgBody/binary >>,
    Body = jsx:encode(
        [   {<<"data">>, [
                {<<"android">>, [
                    {<<"message">>, Msg}
                ]},
                {<<"ios">>, [
                    {<<"aps">>, [
                        {<<"alert">>, Msg}
                    ]}
                ]}
            ]},
            {<<"usernames">>, [
                Username
            ]}
        ]
    ),
    case ibrowse:send_req(Url, Headers, post, Body) of
        {ok, "200", _, Body} ->
            ?DEBUG("Notification for ~p pushed to Platypus, body: ~p", [Username, Body]);
        {ok, Status, _, Body} ->
            ?ERROR_MSG("Notification for ~p could not be pushed, Platypus returned status ~p, "
                       "body: ~p", [Username, Status, Body]);
        {error, Reason} ->
            ?ERROR_MSG("Error when trying to push notification for ~p, reason: ~p",
                       [Username, Reason]),
            {error, Reason}
    end.

%%%----------------------------------------------------------------------
%%% General utility functions
%%%----------------------------------------------------------------------

to_flat_string(List) ->
    lists:flatten(lists:map(fun to_string/1, List)).

to_string(X) when is_atom(X)    -> atom_to_list(X);
to_string(X) when is_binary(X)  -> binary_to_list(X);
to_string(X) when is_list(X)    -> X.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_flat_string_test_() ->
    Username = <<"alice@localhost">>,
    Token = <<"abc123">>,
    [?_test(?assertEqual(
        "http://localhost/users/alice@localhost/devices/abc123",
        to_flat_string([["http", "://", "localhost"], "/users/", Username, "/devices/", Token])
    ))].

-endif.
