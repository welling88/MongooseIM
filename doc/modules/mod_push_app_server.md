mod_push_app_server
===================

See [`mod_push`] for an overview of push notifications ([XEP-0357]).


Introduction
------------

The *App Server* (as of [XEP-0357]) is the service to which the client registers for push notifications.
The *App Server* will then store the client's credentials (token and type of service) and create a PubSub node at the *XMPP Push Service* ([`mod_push_push_service`]).
The PubSub node ID and the PubSub host is sent back to the client which in turn sends them to the *XMPP Server* to enable push notifications.


Interface
---------

Adhoc commands ([XEP-0050]) are used for registering.

### Example request:

```xml
<iq id="my_iq_id" to="localhost" type="set">
    <command action="execute" node="register-push" xmlns="http://jabber.org/protocol/commands">
        <x type="submit" xmlns="jabber:x:data">
            <field var="type">
                <value>android</value>
            </field>
            <field var="token">
                <value>abc123xyz</value>
            </field>
        </x>
    </command>
</iq>
```

### Example response:


```xml
<iq from="localhost" id="my_iq_id" to="alice@localhost/res1" type="result">
    <command node="register-push" status="completed" xmlns="http://jabber.org/protocol/commands">
        <x type="result" xmlns="jabber:x:data">
            <field var="jid">
                <value>pubsub.localhost</value>
            </field>
            <field var="node">
                <value>3884768595</value>
            </field>
        </x>
    </command>
</iq>
```


Options
-------

* `backend` (atom, default: `mnesia`) - Storage backend. Currently only `mnesia` is supported

<!-- TODO: * `push_server_api` (atom) - Push server used to send the actual push notifications.
  Currently supported: [`mod_platypus_api`] -->


Example Configuration
---------------------

```
{mod_push_app_server, []}
```

TODO
----

- [ ] Add other database backends than Mnesia
- [ ] Add option `push_server_api`


<!-- Links -->

[XEP-0050]: http://xmpp.org/extensions/xep-0050.html
[XEP-0060]: http://xmpp.org/extensions/xep-0060.html
[XEP-0357]: http://xmpp.org/extensions/xep-0357.html

[`mod_push`]: mod_push.md
[`mod_push_push_service`]: mod_push_push_service.md
[`mod_platypus_api`]: mod_platypus_api.md