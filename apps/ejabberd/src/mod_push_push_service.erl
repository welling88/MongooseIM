%%%----------------------------------------------------------------------
%%% File    : mod_push_push_service.erl
%%% Authors : Christian Ulrich <christian@rechenwerk.net>
%%%           Jimmy Zöger <jimmy.zoger@erlang-solutions.com>
%%% Purpose : The XMPP Push Service implementation as per XEP-0357 version 0.2
%%%
%%%
%%% Copyright (C) 2015  Christian Ulrich
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

-module(mod_push_push_service).

-xep([{xep, 357}, {version, "0.2"}]).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

% API
-export([create_node/1]).

% Hook handlers
-export([on_push/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push.hrl").

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

-spec(start/2 ::
(
    Host :: binary(),
    Opts :: [any()])
    -> any()
).
start(Host, _Opts) ->
    PubsubHost = mod_pubsub:host(Host),
    ejabberd_hooks:add(node_push_publish_item, PubsubHost, ?MODULE, on_push, 50).

%%-----------------------------------------------------------------------

-spec(stop/1 ::
(
    Host :: binary())
    -> any()
).
stop(Host) ->
    PubsubHost = mod_pubsub:host(Host),
    ejabberd_hooks:delete(node_push_publish_item, PubsubHost, ?MODULE, on_push, 50).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% TODO: RegisterHost and ServerHost may be all mixed upp, check it up
-spec(create_node/1 ::
(
  RegisterHost :: binary())
  -> {ok, Node :: binary()}
).
create_node(RegisterHost) ->
    PubsubHost = mod_pubsub:host(RegisterHost),
    Node = list_to_binary(randoms:get_string()), %% TODO: Change to something with higher entropy
    Owner = jid:make({<<>>, RegisterHost, <<>>}),
    NodeType = <<"push">>,
    Access = all,
    Config = [],
    {result, _} =
        mod_pubsub:create_node(PubsubHost, RegisterHost, Node, Owner, NodeType, Access, Config),
    {ok, PubsubHost, Node}.

%%%----------------------------------------------------------------------
%%% Hook handlers
%%%----------------------------------------------------------------------

-spec(on_push/4 ::
(
    _HookAcc :: any(),
    Node :: binary(),
    Payload :: jlib:xmlel(),
    PubOpts :: jlib:xmlel())
    -> any()
).
on_push(_HookAcc,
        Node,
        [#xmlel{name = <<"notification">>,
                attrs = [{<<"xmlns">>, ?NS_PUSH}],
                children = Children}|_],
        _PubOpts) ->
    ?DEBUG("Incoming notification, Node: ~p", [Node]),
    XDataForms = jlib:filter_xdata_elements(Children, ?NS_PUSH_SUMMARY),
    mod_push_app_server:incoming_notification(Node, XDataForms).


%%-----------------------------------------------------------------------
