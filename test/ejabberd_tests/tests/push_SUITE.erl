%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Suite for testing push features as described in XEP-0357
%%% @end
%%%===================================================================

-module(push_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, push}
    ].

groups() ->
    [{push, [sequence],
      [register_at_app_server
      ]}
    ].

suite() ->
    escalus:suite().

-define(APP_SERVER_ADDR, <<"localhost">>).
-define(XMPP_PUSH_SERVICE_ADDR, <<"pubsub.localhost">>). %% Shouldn't be needed
-define(XMPP_SERVER_ADDR, <<"localhost">>).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    AppEnvs = ct:get_config(app_envs),
    [ [ application:set_env(App, Par, Val) || {Par, Val } <- Envs ] || {App, Envs} <- AppEnvs],
    {ok, Started1} = application:ensure_all_started(pp_http),
    {ok, Started2} = application:ensure_all_started(gcm_mock_server),
    escalus:init_per_suite([{started, Started1 ++ Started2} | Config]).

end_per_suite(Config) ->
    [ application:stop(App) || App <- proplists:get_value(started, Config)],
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}),
    ok.

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}),
    ok.

init_per_testcase(TestName, Config) ->
    meck:new(pp_push_pusher_gcm, [passthrough]),
    escalus:init_per_testcase(TestName, Config).

end_per_testcase(TestName, Config) ->
    meck:unload(),
    escalus:end_per_testcase(TestName, Config).

%%--------------------------------------------------------------------
%% Assertions
%%--------------------------------------------------------------------

-define(assert_non_empty_binary(X),
    ?assert(is_binary(X) andalso size(X) > 0)).

%%--------------------------------------------------------------------
%% General end-to-end test cases for the entities defined in XEP-0357 v0.2
%%
%%                         Relevant XEPs for communications
%% |-------------|----------------------------------|-----------------------------|
%% | From        | To                               | XEP                         |
%% |-------------|----------------------------------|-----------------------------|
%% | Client      | App Server / (XMPP Push Service) | XEP-0050 Ad-Hoc Commands    |
%% | Client      | XMPP Server                      | XEP-0357 Push Notifications |
%% | XMPP Server | XMPP Push Service / (App Server) | XEP-0060 Publish-Subscribe  |
%% |-------------|----------------------------------|-----------------------------|
%%
%%--------------------------------------------------------------------

%% TODO: Log all stanzas (look at pubsub_tools)
register_at_app_server(Config) ->
    escalus:story(
        Config,
        [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            %% Step 1
            %% Registering the client for push notifications at the App Server
            %% (The App Server can be another server than the user's XMPP Server)
            %% Not defined in any XEP
            %%
            %% Request:  adhoc command to node 'register-push'
            %% Response: adhoc response with status 'completed'
            escalus:send(
                Alice,
                escalus_stanza:to(
                    escalus_stanza:adhoc_request(
                        <<"register-push">>,
                        [escalus_stanza:x_data_form(
                            <<"submit">>,
                            escalus_stanza:form_fields(
                                [{<<"type">>, <<"android">>},
                                 {<<"token">>, <<"abc123xyz">>}]))]),
                    ct:get_config(ejabberd_domain))),

            AdHocResp = escalus:wait_for_stanza(Alice),
            ct:pal("AdHocResp: ~p", [AdHocResp]),
            escalus:assert(is_adhoc_response, [<<"register-push">>, <<"completed">>], AdHocResp),

            %% Check response data
            XData = exml_query:path(AdHocResp, [{element, <<"command">>}, {element, <<"x">>}]),
            Fields =  parse_xdata_fields(XData),
            PubsubJid = proplists:get_value(<<"jid">>, Fields),
            PubsubNode = proplists:get_value(<<"node">>, Fields),
            ?assert_non_empty_binary(PubsubJid),
            ?assert_non_empty_binary(PubsubNode),

            %% Step 2
            %% Enabling notifications at the XMPP Server
            %% XEP-0357 - Section 5
            %%
            %% Request:  Ex.8  Enabling Notifications (without publish options)
            %% Response:       IQ stanza with type 'result'
            IqRequest = escalus_stanza:push_enable(PubsubJid, PubsubNode),
            escalus:send(Alice, IqRequest),
            IqResponse = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, [IqRequest], IqResponse),

            %% Step 3
            %% Bob sends message to Alice when she's offline
            log_out(Alice),
            escalus:send(Bob, escalus_stanza:chat(Bob, Alice, <<"Where are you?">>)),
            wait_for_pushed_notification()

      end).

%% TODO: Verify that messages are not pushed when user is online
%% What about status "unavailable"?


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec(parse_xdata_fields/1 ::
(
    XData :: #xmlel{})
    -> [{binary(), binary()}]
).
parse_xdata_fields(XData) ->
    Vars = exml_query:paths(XData, [{element, <<"field">>},
                                    {attr, <<"var">>}]),
    Values = exml_query:paths(XData, [{element, <<"field">>},
                                      {element, <<"value">>},
                                      cdata]),
    lists:zip(Vars, Values).

log_out(User) ->
    Presence = escalus_stanza:presence(<<"unavailable">>),
    escalus_client:send(User, Presence),
    escalus_client:stop(User),
    timer:sleep(1000).

wait_for_pushed_notification() ->
    meck:wait(pp_push_pusher_gcm, push, ['_', '_', android, [<<"abc123xyz">>]], 1000).
