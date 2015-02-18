-module(ejabberd_hooks_mocks).
-compile([export_all]).

given_hooks_started() ->
    start_deps(),
    ejabberd_hooks:start_link().

start_deps() ->
    error_logger:tty(false),
    ejabberd:ensure_started(exometer),
    lager:set_loglevel(lager_console_backend, none).

given_hook_added(H,Domain,M,F,P) ->
    hook_is_added(H,Domain,M,F,P).

hook_is_added(HookName, Domain, ModName, FunName, Prio) ->
    ejabberd_hooks:add(HookName, Domain, ModName, FunName, Prio).

given_module(ModName, FunName, Fun) ->
    catch meck:unload(ModName),
    meck:new(ModName, [non_strict]),
    meck:expect(ModName, FunName, Fun).

given_fun(ModName, FunName, Fun) ->
    meck:expect(ModName, FunName, Fun).
