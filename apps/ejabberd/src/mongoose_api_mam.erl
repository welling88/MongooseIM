%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongoose_api_mam).

%% mongoose_api callbacks
-export([prefix/0,
         routes/0,
         handle_options/2,
         handle_get/2]).

-compile(export_all).

-include("jlib.hrl").
-define(ERROR, {error, unprocessable}).
-define(MAX_RESULT_LIMIT, 50).
-define(DEFAULT_RESULT_LIMIT, 50).

%%--------------------------------------------------------------------
%% mongoose_api callbacks
%%--------------------------------------------------------------------
-spec prefix() -> mongoose_api:prefix().
prefix() ->
    "/archive".

-spec routes() -> mongoose_api:routes().
routes() ->
    [{"/host/:host/username/:username", [simple_archive]}].

-spec handle_options(mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:methods().
handle_options(_Bindings, _Options) ->
    [get].

-spec handle_get(mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:response().
handle_get(Bindings, [simple_archive]) ->
    handle_simple(Bindings).

%%--------------------------------------------------------------------
%% mongoose_api commands actual handlers
%%--------------------------------------------------------------------
handle_simple(Bindings) ->
    Host = gen_mod:get_opt(host, Bindings),
    Username = gen_mod:get_opt(username, Bindings),
    ArcJID = jlib:make_jid(Username, Host, <<>>),
    ArcID = mod_mam:archive_id(Host, Username),
    RSM = undefined,
    Borders = undefined,
    Start = undefined,
    End = undefined,
    Now = mod_mam_utils:now_to_microseconds(os:timestamp()),
    With = undefined,
    PageSize = ?MAX_RESULT_LIMIT,
    LimitPassed = false,
    MaxResultLimit = ?MAX_RESULT_LIMIT,
    IsSimple = true,
    Result = mod_mam:lookup_messages(Host, ArcID, ArcJID, RSM, Borders,
                                     Start, End, Now, With, PageSize,
                                     LimitPassed, MaxResultLimit, IsSimple),
    {ok, {_, _, Messages}} = Result,
    Proplist = [message_to_proplist(Message) || Message <- Messages],
    {ok, Proplist}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
message_to_proplist({MsgId, Jid, Stanza}) ->
    {stanza, [{id, MsgId},
              {jid, jlib:jid_to_binary(Jid)},
              {payload, Stanza}]}.
