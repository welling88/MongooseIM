%%%----------------------------------------------------------------------
%%% File    : mod_push_mnesia.erl
%%% Purpose : mod_push mnesia backend
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

-module(mod_push_mnesia).

-behaviour(mod_push).

%% mod_push callbacks
-export([init/2,
         get_push_user/1,
         write_push_user/1,
         delete_push_user/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push.hrl").



%%%----------------------------------------------------------------------
%%% mod_push callbacks
%%%----------------------------------------------------------------------

init(_Host, _Opts) ->
    mnesia:create_table(push_user,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, push_user)}]),
    % case mnesia:table_info(push_user, attributes) of
    %     UserFields -> ok;
    %     _ -> mnesia:transform_table(push_user, ignore, UserFields)
    % end,
    ok.

%%-----------------------------------------------------------------------

-spec(get_push_user/1 ::
(
    BareUserJid :: ejabberd:simple_bare_jid())
    -> {ok, mod_push:push_user()} | {error, not_found}
).
get_push_user(BareUserJid) ->
    case mnesia:dirty_read(push_user, BareUserJid) of
        [PushUser] ->
            {ok, PushUser};
        [] ->
            {error, not_found}
    end.

%%-----------------------------------------------------------------------

-spec(write_push_user/1 ::
(
    PushUser :: mod_push:push_user())
    -> ok
).
write_push_user(PushUser) ->
    ok = mnesia:dirty_write(PushUser).

%%-----------------------------------------------------------------------

-spec(delete_push_user/1 ::
(
    BareUserJid :: ejabberd:simple_bare_jid())
    -> ok
).
delete_push_user(BareUserJid) ->
    ok = mnesia:dirty_delete(push_user, BareUserJid).

