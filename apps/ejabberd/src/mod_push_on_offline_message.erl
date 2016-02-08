%%%----------------------------------------------------------------------
%%% File    : mod_push_on_offline_message.erl
%%% Authors : Christian Ulrich <christian@rechenwerk.net>
%%%           Jimmy Zöger <jimmy.zoger@erlang-solutions.com>
%%% Purpose : Part of the XMPP Server implementation of XEP-0357 which
%%%           dispatches notifications to the XMPP Push Service
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

-module(mod_push_on_offline_message).

-export([on_offline_message/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push.hrl").

%%-----------------------------------------------------------------------

%% @doc Called on offline_message_hook
-spec(on_offline_message/3 ::
(
    _From :: ejabberd:jid(),
    To :: ejabberd:jid(),
    Packet :: jlib:xmlel())
    -> ok | stop
).
on_offline_message(_From,
                   #jid{luser = LUser, lserver = LServer} = To,
                   #xmlel{name = <<"message">>} = Packet) ->
    ?DEBUG("Offline message for ~p: ~p", [jid:to_lower(To), Packet]),
    BareUserJid = {LUser, LServer},
    case mod_push_backend:get_push_user(BareUserJid) of
        {ok, #push_user{subscriptions = Subscriptions}} ->
            Notification = make_notification(Packet),
            UserJid = jid:make(LUser, LServer, <<>>),
            [dispatch(Notification, UserJid, Subscription) || Subscription <- Subscriptions],
            ok;
        {error, Rsn} ->
            ?DEBUG("No subscriptions for ~p, reason: ~p", [BareUserJid, Rsn]),
            ok
    end.

%%-----------------------------------------------------------------------

-spec(dispatch/3 ::
(
    Notification :: jlib:xmlel(),
    BareUserJid :: ejabberd:jid(),
    Subscription :: mod_push:subscription())
    -> any()
).
dispatch(Notification,
         UserJid,
         #subscription{pubsub_jid = PubsubJid,
                       pubsub_node = PubsubNode,
                       xdata = XData}) ->

    PubOpts = make_publish_options(XData),
    Iq = make_iq_pubsub_publish_item_notification(PubsubNode, Notification, PubOpts),
    ejabberd_router:route(UserJid, PubsubJid, Iq).

%%-----------------------------------------------------------------------

-spec(make_notification/1 ::
(
    Packet :: jlib:xmlel())
    -> jlib:xmlel()
).
make_notification(Packet) ->
    Fields = make_notification_fields(Packet),
    #xmlel{name = <<"notification">>, attrs = [{<<"xmlns">>, ?NS_PUSH}],
           children =
           [#xmlel{name = <<"x">>,
                   attrs = [{<<"xmlns">>, ?NS_XDATA}],
                   children = Fields}]}.

%% Additional custom elements may also be included in a notification

%%-----------------------------------------------------------------------

%% @doc Implements two of the four fields defined in XEP-0357
%% I feel that the two counters don't belong here since such state should not be kept here // Jimmy
%% These are all four field:
%% * 'message-count'
%% * 'pending-subscription-count'
%% * 'last-message-sender'
%% * 'last-message-body'
-spec(make_notification_fields/1 ::
(
    Packet :: jlib:xmlel())
    -> [jlib:xmlel()]
).
make_notification_fields(#xmlel{name = <<"message">>} = Packet) ->
    Sender = proplists:get_value(<<"from">>, Packet#xmlel.attrs),
    Body = get_child_content(<<"body">>, Packet),
    [ jlib:form_field([{<<"var">>, <<"FORM_TYPE">>}], ?NS_PUSH_SUMMARY),
      jlib:form_field([{<<"var">>, <<"last-message-sender">>}], Sender),
      jlib:form_field([{<<"var">>, <<"last-message-body">>}], Body)].

%% If we also want to push 'subscribe' requests for 'presence' we need to listen to the hook
%% 'roster_in_subscription'

%%-----------------------------------------------------------------------

-spec(get_child_content/2 ::
(
    Name :: binary(),
    Elem :: jlib:xmlel())
    -> iodata() | undefined
).
get_child_content(Name, #xmlel{children = Children}) ->
    NamePred =
        fun (#xmlel{name = N}) when N =:= Name -> true;
            (_) -> false
        end,
    case lists:filter(NamePred, Children) of
        [] -> undefined;
        [#xmlel{children = [#xmlcdata{content = Content}|_]}] -> Content
    end.

%%-----------------------------------------------------------------------

-spec(make_publish_options/1 ::
(
    XData :: [jlib:xmlel()])
    -> [jlib:xmlel()]
).
make_publish_options([]) ->
    [];
make_publish_options(XData) ->
    [#xmlel{name = <<"publish-options">>,
            children = XData}].

%%-----------------------------------------------------------------------

-spec(make_iq_pubsub_publish_item_notification/3 ::
(
    Node :: mod_pubsub:nodeId(),
    Notification :: jlib:xmlel(),
    PubOpts :: [jlib:xmlel()])
    -> jlib:xmlel()
).
make_iq_pubsub_publish_item_notification(Node, Notification, PubOpts) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}],
           children =
                [#xmlel{name = <<"pubsub">>,
                        attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
                        children =
                            [#xmlel{name = <<"publish">>,
                                    attrs = [{<<"node">>, Node}],
                                    children =
                                        [#xmlel{name = <<"item">>,
                                                children = [Notification]}]}] ++
                            PubOpts}]}.

%%-----------------------------------------------------------------------
