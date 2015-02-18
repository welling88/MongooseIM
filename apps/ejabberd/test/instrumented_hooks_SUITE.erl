-module(instrumented_hooks_SUITE).
-compile([export_all]).
-define(HOST, <<"localhost">>).

-import(ejabberd_hooks_mocks,
         [given_hooks_started/0,
          given_hook_added/5,
          given_module/3, given_fun/3]).
all() ->
     [an_instrumented_hook_has_success_and_fail_metrics_by_default].

an_instrumented_hook_has_success_and_fail_metrics_by_default(_) ->
    given_hooks_started(),
    given_module(handler_mod, handler_fun, const(ok)),

    given_hook_added(test_metric_hook, ?HOST, handler_mod, handler_fun, 1),

    M = get_metrics(),

    [{[?HOST, test_metric_hook, failed], _},
     {[?HOST, test_metric_hook, successful], _}
    ] = M.

get_metrics() ->
    lists:sort(mongoose_metrics:get_metric_values(?HOST)).


const(V) -> fun(_) -> V end.
