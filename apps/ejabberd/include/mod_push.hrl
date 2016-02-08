%%%----------------------------------------------------------------------
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

-define(NS_PUSH, <<"urn:xmpp:push:0">>).
-define(NS_PUSH_SUMMARY, <<"urn:xmpp:push:summary">>).
-define(NS_PUSH_OPTIONS, <<"urn:xmpp:push:options">>).
-define(NS_PUBLISH_OPTIONS, <<"http://jabber.org/protocol/pubsub#publish-options">>).

-record(subscription,
        {
          pubsub_jid :: ejabberd:jid(), % JID of XMPP Push Service
          pubsub_node :: mod_pubsub:nodeId(), % PubSub node ID
          % Resource of user for which the subscription has been set up
          user_resource :: ejabberd:resource(),
          xdata :: [jlib:xmlel()] % Publish options as X Data (XEP-0004)
        }).

-record(push_user,
        {
          bare_jid :: ejabberd:simple_bare_jid(),
          subscriptions :: [mod_push:subscription()]
        }).
