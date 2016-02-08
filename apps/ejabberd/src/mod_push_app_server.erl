%%%----------------------------------------------------------------------
%%% File    : mod_push_app_server.erl
%%% Authors : Christian Ulrich <christian@rechenwerk.net>
%%%           Jimmy Zöger <jimmy.zoger@erlang-solutions.com>
%%% Purpose : The App Server implementation as per XEP-0357 version 0.2
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

-module(mod_push_app_server).

-xep([{xep, 357}, {version, "0.2"}]).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% API
-export([incoming_notification/2]).

% Hook handlers
-export([process_adhoc_command/4]).

-include("adhoc.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push.hrl").
-include("mod_push_app_server.hrl").

%%%----------------------------------------------------------------------
%%% Type declarations
%%%----------------------------------------------------------------------

-type push_registration() :: #push_registration{}.

%%%----------------------------------------------------------------------
%%% Backend callback attributes
%%%----------------------------------------------------------------------

-callback(init/2 ::
(
    Host :: binary(),
    Opts :: list())
    -> ok
).
-callback(get_push_registration_from_username/1 ::
(
    Username :: binary())
    -> {ok, mod_push_app_server:push_registration()} | {error, not_found}
).
-callback(get_push_registration_from_node/1 ::
(
    Node :: binary())
    -> {ok, mod_push_app_server:push_registration()} | {error, not_found}
).
-callback(write_push_registration/1 ::
(
    PushReg :: mod_push_app_server:push_registration())
    -> ok
).
-callback(delete_push_registration/1 ::
(
    PushReg :: mod_push_app_server:push_registration())
    -> ok
).

%%%----------------------------------------------------------------------
%%% Macro definitions
%%%----------------------------------------------------------------------

-define(BACKEND, mod_push_app_server_backend).
-define(PUSH_SERVER_BACKEND, mod_platypus_api).

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

-spec(start/2 ::
(
    Host :: binary(),
    Opts :: [any()])
    -> any()
).
start(Host, Opts) ->
    gen_mod:start_backend_module(?MODULE, Opts),
    ?BACKEND:init(Host, Opts),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE, process_adhoc_command, 75).
    %% TODO: Add disco hooks for adhoc commands
    %% Get opts for push server api module

%%-----------------------------------------------------------------------

-spec(stop/1 ::
(
    Host :: binary())
    -> any()
).
stop(Host) ->
    ejabberd_hooks:delete(adhoc_local_commands, Host, ?MODULE, process_adhoc_command, 75).

%%%----------------------------------------------------------------------
%%% Hook handlers
%%%----------------------------------------------------------------------

%% TODO: Add access management?
-spec(process_adhoc_command/4 ::
(
    AccIn :: any(),
    From :: jid(),
    To :: jid(),
    Request :: adhoc:request())
    -> adhoc:response() | {error, _} | any()
).
process_adhoc_command(Acc,
                      From,
                      #jid{lserver = LServer},
                      #adhoc_request{node = Command,
                                     action = <<"execute">>,
                                     xdata = XData} = Request) ->
    ?DEBUG("Processing '~s' command", [Command]),
    Result = case Command of
        <<"register-push">> ->
            Parsed = parse_form([XData],
                                undefined,
                                [{{single, <<"type">>}, fun binary_type_to_atom/1},
                                 {single, <<"token">>}],
                                []),
            case Parsed of
                {result, [Type, Token]} ->
                    register_client(From, LServer, Type, Token);
                _ -> error
            end;

        % <<"unregister-push">> ->
        %     Parsed = parse_form([XData],
        %                         undefined,
        %                         [],
        %                         [{single, <<"device-id">>},
        %                          {multi, <<"nodes">>}]),
        %     case Parsed of
        %         {result, [DeviceId, NodeIds]} ->
        %             unregister_client(From, DeviceId, NodeIds);

        %         not_found ->
        %             unregister_client(From, undefined, []);

        %         _ -> error
        %     end;

        % <<"list-push-registrations">> ->
        %     list_registrations(From);

        _ -> unknown
    end,
    case Result of
        unknown -> Acc;

        {registered, PubsubHost, Node} ->
            ?DEBUG("User ~p registered for push at jid: ~p, node: ~p", [From, PubsubHost, Node]),
            JidField = jlib:form_field([{<<"var">>, <<"jid">>}], PubsubHost),
            NodeField = jlib:form_field([{<<"var">>, <<"node">>}], Node),
            Response = adhoc_response(completed, [JidField, NodeField]),
            adhoc:produce_response(Request, Response);

        % {unregistered, ok} ->
        %     Response = adhoc_response(completed, []),
        %     adhoc:produce_response(Request, Response);

        % {unregistered, UnregisteredNodeIds} ->
        %     Field = jlib:form_field([{<<"type">>, <<"list-multi">>}, {<<"var">>, <<"nodes">>}],
        %                             UnregisteredNodeIds),
        %     Response = adhoc_response(completed, [Field]),
        %     adhoc:produce_response(Request, Response);


        % {registrations, RegList} ->
        %     Items =
        %         lists:map(
        %             fun(Reg) ->
        %                 Field = jlib:form_field([{<<"var">>, <<"node">>}],
        %                                         Reg#push_registration.node),
        %                 #xmlel{name = <<"item">>,
        %                        children = [Field]}
        %              end,
        %             RegList),
        %     Response = adhoc_response(completed, Items),
        %     adhoc:produce_response(Request, Response);

        error -> {error, ?ERR_BAD_REQUEST};

        {error, Error} -> {error, Error}
    end;

process_adhoc_command(Acc, _From, _To, _Request) ->
    Acc.

%-------------------------------------------------------------------------

-spec(register_client/4 ::
(
    User :: jid(),
    RegisterHost :: binary(),
    Type :: android | ios,
    Token :: binary())
    -> {registered,
        PubsubHost :: binary(),
        Node :: binary()}
).
%% TODO: Check that push type is valid
%% TODO: Add supported types to module opts
register_client(User, RegisterHost, Type, Token) ->
    %% TODO: Check if there is already a registration for the user and use that one if it exists
    % Create pubsub node
    {ok, PubsubHost, Node} = mod_push_push_service:create_node(RegisterHost),
    % Store relationship between Username and Node
    Username = jid:to_binary(jid:to_bare(User)),
    ?BACKEND:write_push_registration(#push_registration{username = Username, node = Node}),
    % Register att push server
    ?PUSH_SERVER_BACKEND:add_device(Username, Type, Token),
    {registered, PubsubHost, Node}.

%-------------------------------------------------------------------------

%% Callback for workers

% -spec(unregister_client/1 ::
% (
%     Args :: {binary(),
%     erlang:timestamp()})
%     -> error | {error, jlib:xmlel()} | {unregistered, ok} |
%        {unregistered, [binary()]}
% ).

% unregister_client({Node, Timestamp}) ->
%     unregister_client(undefined, undefined, Timestamp, [Node]).

%-------------------------------------------------------------------------

%% Either device ID or a list of node IDs must be given. If none of these are in
%% the payload, the resource of the from jid will be interpreted as device ID.
%% If both device ID and node list are given, the device_id will be ignored and
%% only registrations matching a node ID in the given list will be removed.

% -spec(unregister_client/3 ::
% (
%     Jid :: jid(),
%     DeviceId :: binary(),
%     NodeIds :: [binary()])
%     -> error | {error, jlib:xmlel()} | {unregistered, ok} |
%        {unregistered, [binary()]}
% ).

% unregister_client(Jid, DeviceId, NodeIds) ->
%     unregister_client(Jid, DeviceId, '_', NodeIds).

%-------------------------------------------------------------------------

% -spec(unregister_client/4 ::
% (
%     UserJid :: jid(),
%     DeviceId :: binary(),
%     Timestamp :: erlang:timestamp(),
%     Nodes :: [binary()])
%     -> error | {error, jlib:xmlel()} | {unregistered, ok} |
%        {unregistered, [binary()]}
% ).

% unregister_client(#jid{lresource = <<"">>}, undefined, _, []) -> error;
% unregister_client(#jid{lresource = <<"">>}, <<"">>, _, []) -> error;
% unregister_client(undefined, undefined, _, []) -> error;
% unregister_client(undefined, <<"">>, _, []) -> error;

% unregister_client(UserJid, DeviceId, Timestamp, Nodes) ->
%     DisableIfLocal =
%     fun(#push_registration{node = Node,
%                            bare_jid = {User, Server},
%                            backend_id = BackendId}) ->
%         MatchHeadBackend =
%         #push_backend{id = BackendId, pubsub_host = '$1', _='_'},
%         Selected =
%         mnesia:select(push_backend, [{MatchHeadBackend, [], ['$1']}]),
%         case Selected of
%             [] -> ?DEBUG("++++ Backend does not exist!", []);
%             [PubsubHost] ->
%                 PubsubJid = ljid_to_jid({<<"">>, PubsubHost, <<"">>}),
%                 UserBareJid = ljid_to_jid({User, Server, <<"">>}),
%                 case is_local_domain(Server) of
%                     false ->
%                         PubsubNotification =
%                             #xmlel{
%                                 name = <<"pubsub">>,
%                                 attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
%                                 children =
%                                 [#xmlel{
%                                     name = <<"affiliations">>,
%                                     attrs = [{<<"node">>, Node}],
%                                     children =
%                                     [#xmlel{
%                                         name = <<"affiliation">>,
%                                         attrs = [{<<"jid">>,
%                                                   jlib:jid_to_string(UserBareJid)},
%                                                  {<<"affiliation">>,
%                                                   <<"none">>}]}]}]},
%                         PubsubMessage =
%                             #xmlel{
%                                name = <<"message">>,
%                                attrs = [],
%                                children = [PubsubNotification]},
%                             ejabberd_router:route(
%                                 PubsubJid,
%                                 UserBareJid,
%                                 PubsubMessage);

%                     true ->
%                         disable(UserBareJid, PubsubJid, Node, true)
%                 end
%         end
%     end,
%     F = fun() ->
%         case Nodes of
%             [] ->
%                 #jid{luser = LUser, lserver= LServer, lresource = LResource} =
%                 UserJid,
%                 ChosenDeviceId = case DeviceId of
%                     undefined -> LResource;
%                     <<"">> -> LResource;
%                     _ -> DeviceId
%                 end,
%                 MatchHead =
%                 #push_registration{bare_jid = {LUser, LServer},
%                                    device_id = ChosenDeviceId,
%                                    timestamp = Timestamp,
%                                    _='_'},
%                 MatchingReg =
%                 mnesia:select(push_registration, [{MatchHead, [], ['$_']}]),
%                 case MatchingReg of
%                     [] -> error;

%                     [Reg] ->
%                         ?DEBUG("+++++ deleting registration of user ~p whith node "
%                                "~p",
%                                [Reg#push_registration.bare_jid,
%                                 Reg#push_registration.node]),
%                         mnesia:delete_object(Reg),
%                         DisableIfLocal(Reg),
%                         ok
%                 end;

%             GivenNodes ->
%                 UnregisteredNodes =
%                 lists:foldl(
%                     fun(Node, Acc) ->
%                         RegResult = mnesia:read({push_registration, Node}),
%                         case RegResult of
%                             [] -> Acc;
%                             [Reg] ->
%                                 UserOk =
%                                 case UserJid of
%                                     #jid{luser = LUser, lserver = LServer} ->
%                                         BareJid =
%                                         Reg#push_registration.bare_jid,
%                                         BareJid =:= {LUser, LServer};
%                                     undefined -> true
%                                 end,
%                                 case UserOk of
%                                     true ->
%                                         mnesia:delete_object(Reg),
%                                         DisableIfLocal(Reg),
%                                         [Node|Acc];
%                                     false -> [error|Acc]
%                                 end
%                         end
%                     end,
%                     [],
%                     GivenNodes),
%                 case [El || El <- UnregisteredNodes, El =:= error] of
%                     [] -> UnregisteredNodes;
%                     _ -> error
%                 end
%         end
%     end,
%     case mnesia:transaction(F) of
%         {aborted, Reason} ->
%             ?DEBUG("+++++ unregister_client error: ~p", [Reason]),
%             {error, ?ERR_INTERNAL_SERVER_ERROR};
%         {atomic, error} -> error;
%         {atomic, Result} -> {unregistered, Result}
%     end.


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec(incoming_notification/2 ::
(
    Node :: binary(),
    XDataForms :: [jlib:xmlel()])
    -> ok
).
incoming_notification(Node, XDataForms) ->
    case ?BACKEND:get_push_registration_from_node(Node) of
        {ok, #push_registration{username = Username}} ->
            ParseResult =
                parse_form(
                    XDataForms,
                    ?NS_PUSH_SUMMARY,
                    [{single, <<"last-message-sender">>},
                     {single, <<"last-message-body">>}],
                    []),
            case ParseResult of
                {result, [MsgSender, MsgBody]} ->
                    ?PUSH_SERVER_BACKEND:send_push(Username, MsgSender, MsgBody);
                Error ->
                   ?DEBUG("parse_form returned ~p", [Error]),
                   ?INFO_MSG("Cancelling dispatch of push notification: item published on node ~p "
                                 "contains malformed data form: ~p", [Node, XDataForms])
            end;
        {error, not_found} ->
            ?INFO_MSG("No push registration found for node ~p, can not push to user", [Node])
    end.


%%%----------------------------------------------------------------------
%%% General utility functions
%%%----------------------------------------------------------------------

-spec(parse_form/4 ::
(
    XDataForms :: [false | jlib:xmlel()],
    FormType :: binary(),
    RequiredFields :: [ {multi, binary()}
                      | {single, binary()}
                      | {{multi, binary()}, fun((binary()) -> any())}
                      | {{single, binary()}, fun((binary()) -> any())}
                      ],
    OptionalFields :: [ {multi, binary()}
                      | {single, binary()}
                      | {{multi, binary()}, fun((binary()) -> any())}
                      | {{single, binary()}, fun((binary()) -> any())}
                      ])
    -> not_found | error | {result, [any()]}
).

parse_form([], _FormType, _RequiredFields, _OptionalFields) ->
    not_found;

parse_form([false|T], FormType, RequiredFields, OptionalFields) ->
    parse_form(T, FormType, RequiredFields, OptionalFields);

parse_form([XDataForm|T], FormType, RequiredFields, OptionalFields) ->
    #xmlel{name = <<"x">>, children = FieldElems} = XDataForm,
    Fields = jlib:parse_xdata_fields(FieldElems),
    case get_xdata_value(<<"FORM_TYPE">>, Fields) of
        FormType ->
            GetValues =
                fun
                    ({multi, Key}) -> get_xdata_values(Key, Fields);
                    ({single, Key}) -> get_xdata_value(Key, Fields);
                    ({KeyTuple, Convert}) ->
                        case KeyTuple of
                            {multi, Key} ->
                                Values = get_xdata_values(Key, Fields),
                                Converted = lists:foldl(
                                    fun
                                        (_, error) -> error;
                                        (undefined, Acc) -> [undefined|Acc];
                                        (B, Acc) ->
                                            try [Convert(B)|Acc]
                                            catch error:badarg -> error
                                            end
                                    end,
                                    [],
                                    Values),
                                lists:reverse(Converted);

                            {single, Key} ->
                                case get_xdata_value(Key, Fields) of
                                    error -> error;
                                    undefined -> undefined;
                                    Value ->
                                       try Convert(Value)
                                       catch error:badarg -> error
                                       end
                                end
                        end
                end,
            RequiredValues = lists:map(GetValues, RequiredFields),
            OptionalValues = lists:map(GetValues, OptionalFields),
            RequiredOk =
                lists:all(
                    fun(V) ->
                        (V =/= undefined) and (V =/= []) and (V =/= error)
                    end,
                    RequiredValues),
            OptionalOk =
                lists:all(
                    fun(V) ->
                        V =/= error
                    end,
                    OptionalValues),
            case RequiredOk and OptionalOk of
                false -> error;
                true ->
                    {result, RequiredValues ++ OptionalValues}
            end;

        _ -> parse_form(T, FormType, RequiredFields, OptionalFields)
    end.

%%-----------------------------------------------------------------------

-spec(get_xdata_value/2 ::
(
    FieldName :: binary(),
    Fields :: [{binary(), [binary()]}])
    -> binary() | error | undefined
).
get_xdata_value(FieldName, Fields) ->
    get_xdata_value(FieldName, Fields, undefined).

%%-----------------------------------------------------------------------

-spec(get_xdata_value/3 ::
(
    FieldName :: binary(),
    Fields :: [{binary(), [binary()]}],
    DefaultValue :: any())
    -> any()
).
get_xdata_value(FieldName, Fields, DefaultValue) ->
    case proplists:get_value(FieldName, Fields, [DefaultValue]) of
        [Value] -> Value;
        _ -> error
    end.

%%-----------------------------------------------------------------------

-spec(get_xdata_values/2 ::
(
    FieldName :: binary(),
    Fields :: [{binary(), [binary()]}])
    -> [binary()]
).
get_xdata_values(FieldName, Fields) ->
    get_xdata_values(FieldName, Fields, []).

%%-----------------------------------------------------------------------

-spec(get_xdata_values/3 ::
(
    FieldName :: binary(),
    Fields :: [{binary(), [binary()]}],
    DefaultValue :: any())
    -> any()
).
get_xdata_values(FieldName, Fields, DefaultValue) ->
    proplists:get_value(FieldName, Fields, DefaultValue).

%%-----------------------------------------------------------------------

binary_type_to_atom(<<"android">>) -> android;
binary_type_to_atom(<<"ios">>) -> ios.

%%-----------------------------------------------------------------------

-spec(adhoc_response/2 ::
(
    Status :: completed,
    FormFields :: [jlib:xmlel()])
    -> adhoc:response()
).
adhoc_response(completed, FormFields) ->
    #adhoc_response{
        status = completed,
        elements = [#xmlel{name = <<"x">>,
                           attrs = [{<<"xmlns">>, ?NS_XDATA},
                                    {<<"type">>, <<"result">>}],
                           children = FormFields}]}.
