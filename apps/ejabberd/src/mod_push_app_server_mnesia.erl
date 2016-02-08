%%%----------------------------------------------------------------------
%%% File    : mod_push_app_server_mnesia.erl
%%% Purpose : mod_push_app_server mnesia backend
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

-module(mod_push_app_server_mnesia).

-behaviour(mod_push_app_server).

%% mod_push_app_server callbacks
-export([init/2,
         get_push_registration_from_username/1,
         get_push_registration_from_node/1,
         write_push_registration/1,
         delete_push_registration/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push_app_server.hrl").

%%%----------------------------------------------------------------------
%%% mod_push callbacks
%%%----------------------------------------------------------------------

init(_Host, _Opts) ->
    mnesia:create_table(push_registration_key_username,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, [username, node]}]),
    mnesia:create_table(push_registration_key_node,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, [node, username]}]),
    % case mnesia:table_info(push_registration, attributes) of
    %     Fields -> ok;
    %     _ -> mnesia:transform_table(push_registration, ignore, Fields)
    % end,
    ok.

%%-----------------------------------------------------------------------

-spec(get_push_registration_from_username/1 ::
(
    Username :: binary())
    -> {ok, mod_push_app_server:push_registration()} | {error, not_found}
).
get_push_registration_from_username(Username) ->
    case mnesia:dirty_read(push_registration_key_username, Username) of
        [{_, Username, Node}] ->
            {ok, #push_registration{username = Username, node = Node}};
        [] ->
            {error, not_found}
    end.

%%-----------------------------------------------------------------------

-spec(get_push_registration_from_node/1 ::
(
    Node :: binary())
    -> {ok, mod_push_app_server:push_registration()} | {error, not_found}
).
get_push_registration_from_node(Node) ->
    case mnesia:dirty_read(push_registration_key_node, Node) of
        [{_, Node, Username}] ->
            {ok, #push_registration{node = Node, username = Username}};
        [] ->
            {error, not_found}
    end.

%%-----------------------------------------------------------------------

-spec(write_push_registration/1 ::
(
    PushReg :: mod_push_app_server:push_registration())
    -> ok
).
write_push_registration(#push_registration{username = Username, node = Node}) ->
    ok = mnesia:dirty_write({push_registration_key_username, Username, Node}),
    ok = mnesia:dirty_write({push_registration_key_node, Node, Username}).

%%-----------------------------------------------------------------------

-spec(delete_push_registration/1 ::
(
    PushReg :: mod_push_app_server:push_registration())
    -> ok
).
delete_push_registration(#push_registration{username = Username, node = Node}) ->
    ok = mnesia:dirty_delete(push_registration_key_username, Username),
    ok = mnesia:dirty_delete(push_registration_key_node, Node).

%%-----------------------------------------------------------------------
