mod_push
========

Introduction
------------

[XEP-0357] defines a general way of handling push notifications for mobile devices.
The actual delivery of the notifications is done in an external service called the *App Server*.
The server through which the user is registered, the user's home server, is simply called the *XMPP Server*.
When the XMPP Server receives a messages which is intended for a user that is offline, it can forward the message data through a publish request to a PubSub node on the *App Server*.
This part of the *App Server* is called the *XMPP Push Service*.
To enable this, a user's client must, beforehand, register itself at the *App Server* and receive the PubSub host and node ID to which the *XMPP Server* should publish notifications.
The client must then send this information in an enable request to the *XMPP Server*.

The XEP does only specify the communications between the client and the *XMPP Server* as well as between the *XMPP Server* and the *XMPP Push Service*.
The communication between the client and the *App Server* can thus be done in any suitable way.
Neither does the XEP concern logic related to notifications, e.g. 'only push direct messages'.
If logic like this is required, it should be implemented in the *App Server*.


Structure
---------

* `mod_push` implements the *XMPP Server* part of [XEP-0357].
  It is not necessary to have `mod_pubsub` enabled on the *XMPP Server*.
  `mod_push` will craft an IQ stanza with the PubSub data that is sent to the *XMPP Push Service*.

* [`mod_push_app_server`] implements a simple *App Server* as per [XEP-0357].
  Clients can register themselves by sending adhoc commands ([XEP-0050]) to the *App Server*.

* [`mod_push_push_service`] implements the *XMPP Push Service* part of [XEP-0357].
  The *App Server* creates PubSub nodes at the *Push Service* and the *XMPP Push Service* forwards incoming notifications to the *App Server*.
  PubSub as per [XEP-0060] is required on this server.


Options
-------

* `backend` (atom, default: `mnesia`) - Storage backend. Currently only `mnesia` is supported


Example Configuration
---------------------

```
{mod_push, []}
```


<!-- Links -->

[XEP-0050]: http://xmpp.org/extensions/xep-0050.html
[XEP-0060]: http://xmpp.org/extensions/xep-0060.html
[XEP-0357]: http://xmpp.org/extensions/xep-0357.html

[`mod_push_app_server`]: mod_push_app_server.md
[`mod_push_push_service`]: mod_push_push_service.md
