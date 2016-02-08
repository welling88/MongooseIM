%%%----------------------------------------------------------------------
%%% File    : mod_push.erl
%%% Authors : Christian Ulrich <christian@rechenwerk.net>
%%%           Jimmy Zöger <jimmy.zoger@erlang-solutions.com>
%%% Purpose : The XMPP Server implementation of XEP-0357 version 0.2
%%%
%%% Created : 22 Dec 2014 by Christian Ulrich <christian@rechenwerk.net>
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

-module(mod_push).

-xep([{xep, 357}, {version, "0.2"}]).

-author('christian@rechenwerk.net').
-author('mongoose-im@erlang-solutions.com').

-behaviour(gen_mod).

% gen_mod callbacks
-export([start/2,
         stop/1]).

% Hook handlers
-export([%on_affiliation_removal/4,
         on_disco_pubsub_info/5,
         on_disco_local_features/5,
         % on_disco_sm_identity/5,
         on_remove_user/2,
         process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push.hrl").

-define(MAX_INT, 4294967295).

%%%----------------------------------------------------------------------
%%% Type declarations
%%%----------------------------------------------------------------------

-type subscription() :: #subscription{}.
-type push_user() :: #push_user{}.

-export_type([subscription/0,
              push_user/0]).

% -type payload_key() ::
%     'last-message-sender' | 'last-subscription-sender' | 'message-count' |
%     'pending-subscription-count' | 'last-message-body'.
% -type push_registration() :: #push_registration{}.
% -type reg_type() :: {local_reg, binary(), binary()} | % pubsub host, secret
%                   {remote_reg, jid(), binary()}.  % pubsub host, secret

%%%----------------------------------------------------------------------
%%% Backend callback attributes
%%%----------------------------------------------------------------------

-callback(init/2 ::
(
    Host :: binary(),
    Opts :: list())
    -> ok
).
-callback(get_push_user/1 ::
(
    BareUserJid :: ejabberd:simple_bare_jid())
    -> {ok, push_user()} | {error, not_found}
).
-callback(write_push_user/1 ::
(
    PushUser :: push_user())
    -> ok
).
-callback(delete_push_user/1 ::
(
    BareUserJid :: ejabberd:simple_bare_jid())
    -> ok
).

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
    mod_push_backend:init(Host, Opts),
    % TEMPLATE: ejabberd_hooks:add(Hook, Host, Module, Function, Priority)
    % TEMPLATE: gen_iq_handler:add_iq_handler(Component, Host, NS, Module, Function, Type)
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUSH, ?MODULE, process_iq, one_queue),
    ejabberd_hooks:add(offline_message_hook, Host,
                       mod_push_on_offline_message, on_offline_message, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, on_disco_local_features, 50),
    % ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, on_affiliation_removal, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, on_remove_user, 50),
    % FIXME: disco_sm_info is not implemented in mod_disco!
    % ejabberd_hooks:add(disco_sm_info, Host, ?MODULE, on_disco_sm_info, 50),
    add_disco_hooks(Host).

%%-----------------------------------------------------------------------

-spec(stop/1 ::
(
    Host :: binary())
    -> any()
).
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUSH),
    ejabberd_hooks:delete(offline_message_hook, Host,
                          mod_push_on_offline_message, on_offline_message, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, on_disco_local_features, 50),
    % ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, on_affiliation_removal, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, on_remove_user, 50),
    % FIXME: disco_sm_info is not implemented in mod_disco!
    % ejabberd_hooks:delete(disco_sm_info, Host, ?MODULE, on_disco_sm_info, 50),
    delete_disco_hooks(Host).


%%-----------------------------------------------------------------------

add_disco_hooks(Host) ->
    % ejabberd_hooks:add(disco_local_identity, PubsubHost, ?MODULE, on_disco_pubsub_identity, 50),
    % FIXME: this is a workaround, see below
    ejabberd_hooks:add(disco_info, Host, ?MODULE, on_disco_pubsub_info, 101),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, on_disco_sm_identity, 49).

%%-----------------------------------------------------------------------

delete_disco_hooks(Host) ->
    ejabberd_hooks:delete(disco_info, Host, ?MODULE, on_disco_pubsub_info, 101),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, on_disco_sm_identity, 49).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec(enable/4 ::
(
    UserJid :: jid(),
    PubsubJid :: jid(),
    PubsubNode :: mod_pubsub:nodeId(),
    XData :: [jlib:xmlel()])
    -> {enabled, ok} | {enabled, [jlib:xmlel()]} | {error, jlib:xmlel()}
).
enable(_UserJid, _PubsubJid, undefined, _XData) ->
    {error, ?ERR_NOT_ACCEPTABLE};
enable(_UserJid, _PubsubJid, <<"">>, _XData) ->
    {error, ?ERR_NOT_ACCEPTABLE};
enable(#jid{luser = LUser, lserver = LServer, lresource = LResource},
       #jid{} = PubsubJid,
       PubsubNode,
       XData) ->
    Subscription = #subscription{pubsub_jid = PubsubJid,
                                 pubsub_node = PubsubNode,
                                 user_resource = LResource,
                                 xdata = XData},
    BareUserJid = {LUser, LServer},
    ok = add_subscription(BareUserJid, Subscription),
    {enabled, ok}.

%%-----------------------------------------------------------------------

-spec(add_subscription/2 ::
(
    BareUserJid :: ejabberd:simple_bare_jid(),
    Subscription :: subscription())
    -> ok | error %% TODO: correct?
).
add_subscription(BareUserJid, Subscription) ->
    NewPushUser =
        case mod_push_backend:get_push_user(BareUserJid) of
            {ok, #push_user{subscriptions = Subscriptions} = PushUser} ->
                % TODO: If there already is a subscription for the user_resource, replace it
                PushUser#push_user{subscriptions = [Subscription | Subscriptions]};
            {error, not_found} ->
                #push_user{bare_jid = BareUserJid,
                           subscriptions = [Subscription]}
        end,
    mod_push_backend:write_push_user(NewPushUser).

%%-----------------------------------------------------------------------

-spec(disable/3 ::
(
    UserJid :: jid(),
    PubsubJid :: jid(),
    PubsubNode :: binary())
    -> {error, jlib:xmlel()} | {disabled, ok}
).
disable(_From, _PubsubJid, <<"">>) ->
    {error, ?ERR_NOT_ACCEPTABLE};
disable(#jid{luser = LUser, lserver = LServer},
        #jid{} = PubsubJid,
        PubsubNode) ->
    BareUserJid = {LUser, LServer},
    case remove_subscriptions(BareUserJid, PubsubJid, PubsubNode) of
        {ok, _} -> {disabled, ok};
        {error, _} -> {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

%%-----------------------------------------------------------------------

-spec(remove_subscriptions/3 ::
(
    BareUserJid :: ejabberd:simple_bare_jid(),
    PubsubJid :: ejabberd:jid(),
    PubsubNode :: mod_pubsub:nodeId() | undefined)
    -> {ok, any()} | {error, any()}
).
remove_subscriptions(BareUserJid, PubsubJid, PubsubNode) ->
    case mod_push_backend:get_push_user(BareUserJid) of
        error ->
            ?DEBUG("remove_subscriptions: No existing push user: ~p",
                   [BareUserJid]),
            ok;
        {ok, PushUser} ->
            Subs = PushUser#push_user.subscriptions,
            SubsToKeep = filter_subscriptions(Subs, PubsubJid, PubsubNode),
            if
                SubsToKeep =:= [] ->
                    mod_push_backend:remove_push_user(BareUserJid),
                    ok;
                true ->
                    NewPushUser = PushUser#push_user{subscriptions = SubsToKeep},
                    mod_push_backend:write_push_user(NewPushUser)
            end
    end.

%%-----------------------------------------------------------------------

%% @doc Filters subscriptions so that the ones matching PubsubJid and PubsubNode are removed.
%% If PubsubNode is undefined, all subscriptions matching PubsubJid are removed.
filter_subscriptions(Subs, PubsubJid, PubsubNode) ->
    Pred =
        fun (#subscription{pubsub_jid = Jid, pubsub_node = Node}) ->
                MatchingJid = jid:are_equal(Jid, PubsubJid),
                MatchingNode = PubsubNode =:= undefined orelse PubsubNode =:= Node,
                not (MatchingJid andalso MatchingNode)
        end,
    lists:filter(Subs, Pred).

%%%----------------------------------------------------------------------
%%% Hook handlers
%%%----------------------------------------------------------------------

-spec(on_remove_user/2 ::
(
    LUser :: binary(),
    LServer :: binary())
    -> ok
).
on_remove_user(LUser, LServer) ->
    mod_push_backend:delete_push_user({LUser, LServer}).

%%-----------------------------------------------------------------------

% % TODO: For Remote Disabling of Notifications

% -spec(on_affiliation_removal/4 ::
% (
%     _User :: jid(),
%     From :: jid(),
%     To :: jid(),
%     Packet :: jlib:xmlel())
%     -> ok
% ).

% 8. Remote Disabling of Notifications
% on_affiliation_removal(User, From, _To,
%                        #xmlel{name = <<"message">>, children = Children}) ->
%     FindNodeAffiliations =
%     fun
%     F([#xmlel{name = <<"pubsub">>, attrs = Attrs, children = PChildr}|T]) ->
%         case proplists:get_value(<<"xmlns">>, Attrs) of
%             ?NS_PUBSUB ->
%                 case PChildr of
%                     [#xmlel{name = <<"affiliations">>} = A] ->
%                         case proplists:get_value(<<"node">>, A#xmlel.attrs) of
%                             undefined -> error;
%                             Node -> {Node, A#xmlel.children}
%                         end;
%                     _ -> not_found
%                 end;
%             _ -> F(T)
%         end;
%     F([_|T]) -> F(T);
%     F([]) -> not_found
%     end,
%     FindJid =
%     fun
%     F([#xmlel{name = <<"affiliation">>, attrs = Attrs}|T]) ->
%         case proplists:get_value(<<"affiliation">>, Attrs) of
%             <<"none">> ->
%                 case proplists:get_value(<<"jid">>, Attrs) of
%                     J when is_binary(J) -> jlib:string_to_jid(J);
%                     _ -> error
%                 end;
%             undefined -> F(T)
%         end;
%     F([_|T]) -> F(T);
%     F([]) -> not_found
%     end,
%     ErrMsg =
%     fun() ->
%         ?INFO_MSG("Received invalid affiliation removal notification from ~p",
%                   [jlib:jid_to_string(From)])
%     end,
%     case FindNodeAffiliations(Children) of
%         not_found -> ok;
%         error -> ErrMsg();
%         {Node, Affiliations} ->
%             BareUserJid = jlib:jid_remove_resource(jlib:jid_tolower(User)),
%             case FindJid(Affiliations) of
%                 not_found -> ok;
%                 BareUserJid -> disable(BareUserJid, From, Node, true);
%                 _ -> ErrMsg()
%             end
%     end;

% on_affiliation_removal(_Jid, _From, _To, _) -> ok.


%%-----------------------------------------------------------------------

%% @doc Processes IQ stanzas which either should be about enabling or disabling notifications
-spec(process_iq/3 ::
(
    From :: jid(),
    _To :: jid(),
    IQ :: iq())
    -> iq()
).
process_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    JidB = proplists:get_value(<<"jid">>, SubEl#xmlel.attrs),
    Node = proplists:get_value(<<"node">>, SubEl#xmlel.attrs),
    case JidB of
        undefined -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        _ ->
            case jid:from_binary(JidB) of
                error ->
                    IQ#iq{type = error, sub_el = [?ERR_JID_MALFORMED, SubEl]};

                Jid ->
                    case {Type, SubEl} of
                        {set, #xmlel{name = <<"enable">>,
                                     children = Children}} ->
                            XDataForms = jlib:filter_xdata_elements(Children, ?NS_PUBLISH_OPTIONS),
                            case enable(From, Jid, Node, XDataForms) of
                                {enabled, ok} ->
                                    IQ#iq{type = result, sub_el = []};

                                {enabled, ResponseForm} ->
                                    NewSubEl =
                                    SubEl#xmlel{children = ResponseForm},
                                    IQ#iq{type = result, sub_el = [NewSubEl]};

                                {error, Error} ->
                                    IQ#iq{type = error,
                                          sub_el = [Error, SubEl]}
                            end;

                        {set, #xmlel{name = <<"disable">>}} ->
                            case disable(From, Jid, Node) of
                                {disabled, ok} ->
                                    IQ#iq{type = result, sub_el = []};

                                {error, Error} ->
                                    IQ#iq{type = error,
                                          sub_el = [Error, SubEl]}
                            end;

                        _ ->
                            ?DEBUG("Received invalid push IQ from ~p",
                                   [jlib:jid_to_string(From)]),
                            IQ#iq{type = error,
                                  sub_el = [?ERR_NOT_ALLOWED, SubEl]}
                    end
            end
    end.

%%-----------------------------------------------------------------------

-spec(on_disco_local_features/5 ::
(
    Acc :: any(),
    _From :: jid(),
    _To :: jid(),
    Node :: binary(),
    _Lang :: binary())
    -> any()
).

on_disco_local_features(empty, _From, _To, <<"">>, _Lang) ->
    ?DEBUG("+++++++++ on_disco_local_features, returning ~p",
           [{result, [?NS_PUSH]}]),
    {result, [?NS_PUSH]};

on_disco_local_features({result, Features}, _From, _To, <<"">>, _Lang) ->
    ?DEBUG("+++++++++ on_disco_local_features, returning ~p",
           [{result, [?NS_PUSH|Features]}]),
    {result, [?NS_PUSH|Features]};

on_disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    ?DEBUG("+++++++++ on_disco_local_features, returning ~p", [Acc]),
    Acc.

%%-----------------------------------------------------------------------

% FIXME: this is a workaround, it adds identity and features to the info data
% created by mod_disco when mod_pubsub calls the hook disco_info. Instead
% mod_pubsub should set mod_disco:process_local_iq_info as iq handler for its
% pubsub host. Then on_disco_identity can hook up with disco_local_identity and
% disco_local_features
on_disco_pubsub_info(Acc, _ServerHost, mod_pubsub, <<>>, <<>>) ->
    PushIdentity = #xmlel{name = <<"identity">>,
                          attrs = [{<<"category">>, <<"pubsub">>},
                                   {<<"type">>, <<"push">>}],
                          children = []},
    PushFeature = #xmlel{name = <<"feature">>,
                         attrs = [{<<"var">>, ?NS_PUSH}],
                         children = []},
    [PushIdentity, PushFeature | Acc];
on_disco_pubsub_info(Acc, _ServerHost, _Module, _, _) ->
    Acc.

%%-----------------------------------------------------------------------

%on_disco_pubsub_identity(Acc, _From, #jid{lserver = PubsubHost}, <<"">>, _) ->
%    F = fun() ->
%        MatchHead = #push_backend{pubsub_host = PubsubHost, _='_'},
%        case mnesia:select(push_backend, [{MatchHead, [], ['$_']}]) of
%            [] -> Acc;
%            _ ->
%                PushIdentity =
%                #xmlel{name = <<"identity">>,
%                       attrs = [{<<"category">>, <<"pubsub">>},
%                                {<<"type">>, <<"push">>}],
%                       children = []},
%                [PushIdentity|Acc]
%        end
%    end,
%    case mnesia:transaction(F) of
%        {atomic, AccOut} -> AccOut;
%        _ -> Acc
%    end;
%
%on_disco_pubsub_identity(Acc, _From, _To, _Node, _Lang) ->
%    Acc.


