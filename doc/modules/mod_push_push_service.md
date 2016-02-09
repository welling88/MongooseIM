mod_push_push_service
=====================

See [`mod_push`] for an overview of push notifications ([XEP-0357]).


Introduction
------------

The *XMPP Push Service* creates PubSub nodes on request by the *App Server* ([`mod_push_app_server`]).
Received notifications (PubSub publish requests) are forwarded to the *App Server*.


Options
-------

There are no options.


Requirements
------------

This module requires that `mod_pubsub` is also running.


Configuration
-------------

Add `<<"push">>` to the list `mod_pubsub` plugins.

**For example:**

```erlang
{mod_pubsub, [{plugins, [<<"dag">>, <<"push">>]},
              {nodetree, <<"dag">>}]},
```


Other files
-----------

* `node_push` is the node plugin to `mod_pubsub`


<!-- Links -->

[XEP-0050]: http://xmpp.org/extensions/xep-0050.html
[XEP-0060]: http://xmpp.org/extensions/xep-0060.html
[XEP-0357]: http://xmpp.org/extensions/xep-0357.html

[`mod_push`]: mod_push.md
[`mod_push_app_server`]: mod_push_app_server.md
