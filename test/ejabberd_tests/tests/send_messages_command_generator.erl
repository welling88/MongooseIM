-module(send_messages_command_generator).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([test/0, sample/0, prop/0]).

-export([initial_state/0, command/1, precondition/2, postcondition/3,
		 next_state/3]).

-export([connect_carol/0,
         connect_geralt/0,
         send_from_carol/2,
         send_from_geralt/2,
         wait_for_msgs_carol/2,
         wait_for_msgs_geralt/2]).

-export([client/1]).

-record(state, {carol,
				geralt,
                msgs_to_carol,
                msgs_to_geralt}).

test() ->
    proper:quickcheck(?MODULE:prop()).

sample() ->
    proper_gen:pick(commands(?MODULE)).

prop() ->
	?FORALL(Cmds, commands(?MODULE),
			?TRAPEXIT(
			   begin
				   {History,State,Result} = run_commands(?MODULE, Cmds),
                   maybe_stop_client(State#state.carol),
                   maybe_stop_client(State#state.geralt),
				   ?WHENFAIL(ct:pal(error, "History: ~p~nState: ~p\nResult: ~p~n",
									[History,State,Result]),
							 aggregate(command_names(Cmds), Result =:= ok))
			   end)).

maybe_stop_client(undefined) -> ok;
maybe_stop_client(Client) ->
    Client ! stop.

initial_state() ->
	#state{carol = undefined,
		   geralt = undefined,
           msgs_to_carol = [],
           msgs_to_geralt = []}.

command(S) ->
    Cmds = possible_commands(S),
    oneof(Cmds).

possible_commands(S) ->
    Carol = (S#state.carol /= undefined),
    Geralt = (S#state.geralt /= undefined),
    Users = Carol andalso Geralt,
    MsgsToCarol = (S#state.msgs_to_carol /= []),
    MsgsToGeralt = (S#state.msgs_to_geralt /= []),
    [{call, ?MODULE, connect_carol, []} || not Carol] ++
    [{call, ?MODULE, connect_geralt, []} || not Geralt] ++
    [{call, ?MODULE, send_from_carol, [S#state.carol, S#state.geralt]} || Users] ++
    [{call, ?MODULE, send_from_geralt, [S#state.geralt, S#state.carol]} || Users] ++
    [{call, ?MODULE, wait_for_msgs_carol, [S#state.carol, S#state.msgs_to_carol]}
     || MsgsToCarol] ++
    [{call, ?MODULE, wait_for_msgs_geralt, [S#state.geralt, S#state.msgs_to_geralt]}
     || MsgsToGeralt].


precondition(_, _) -> true.
postcondition(_, _, _) -> true.

next_state(S, V, {call, _, connect_carol, []}) ->
    S#state{carol = V};
next_state(S, V, {call, _, connect_geralt, []}) ->
    S#state{geralt = V};
next_state(#state{msgs_to_geralt = Msgs} = S, V, {call, _, send_from_carol, _}) ->
    S#state{msgs_to_geralt = [V | Msgs]};
next_state(#state{msgs_to_carol = Msgs} = S, V, {call, _, send_from_geralt, _}) ->
    S#state{msgs_to_carol = [V | Msgs]};
next_state(S, _, {call, _, wait_for_msgs_carol, _}) ->
    S#state{msgs_to_carol = []};
next_state(S, _, {call, _, wait_for_msgs_geralt, _}) ->
    S#state{msgs_to_geralt = []};
next_state(S, _, _) ->
    S.

connect_carol() ->
    connect_user(1).

connect_geralt() ->
    connect_user(1).

connect_user(I) ->
    spawn(?MODULE, client, [I]).

send_from_carol(Carol, Geralt) ->
    Msg = gen_msg(),
    Geralt ! {msg, Carol, Msg},
    Msg.

send_from_geralt(Geralt, Carol) ->
    Msg = gen_msg(),
    Carol ! {msg, Geralt, Msg},
    Msg.

gen_msg() ->
    Msg = base64:encode(crypto:rand_bytes(15)),
    Msg.

wait_for_msgs_carol(Carol, Msgs) ->
    wait_for_msgs(Carol, lists:reverse(Msgs)).

wait_for_msgs_geralt(Geralt, Msgs) ->
    wait_for_msgs(Geralt, lists:reverse(Msgs)).

wait_for_msgs(_Client, []) ->
    ok;
wait_for_msgs(Client, [Msg | Rest]) ->
    Msg = wait_for_message(Client),
    wait_for_msgs(Client, Rest).

wait_for_message(Client) ->
    Client ! {wait_for_message, self()},
    receive
        {msg, Msg} ->
            Msg;
        {error, timeout} ->
            {error, timeout}
    after
        2000 ->
            error
    end.

client(I) ->
    receive
        {wait_for_message, Pid} ->
            client_wait_for_message(I, Pid);
        stop ->
            ok
    end.

client_wait_for_message(I, Pid) ->
    receive
        {msg, _From, Msg} ->
            case I > 10 of
                true ->
                    Pid ! {msg, <<"wrong_message">>};
                _ ->
                    ok
            end,
            Pid ! {msg, Msg}
    after
        1000 ->
            Pid ! {error, timeout}
    end,
    client(I+1).

