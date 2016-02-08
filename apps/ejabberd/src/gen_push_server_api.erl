%%%----------------------------------------------------------------------
%%% File    : gen_push_server_api.erl
%%% Author  : Jimmy Zöger <jimmy.zoger@erlang-solutions.com>
%%% Purpose : Push Server API for mod_push_app_server
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

-module(gen_push_server_api).

-callback(add_device/3 ::
(
    Username :: binary(),
    Type :: binary() | atom(),
    Token :: binary())
    -> ok | {error, Reason :: any()}
).

-callback(send_push/3 ::
(
    Username :: binary(),
    MsgSender :: binary(),
    MsgBody :: binary())
    -> ok | {error, Reason :: any()}
).

%% TODO: remove_device, list_devices, (remove_user not yet implemented in platypus)
