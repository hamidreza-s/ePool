-module(blocking_queue).
-export([new/1,
         put_in/2,
         take_up/1]).

new(Cap) -> spawn(fun() -> listen(Cap, queue:new()) end).

put_in(Pid, Value) ->
   Pid ! {self(), {put, Value}},
   receive
      ok -> ok
   end.

take_up(Pid) ->
   Pid ! {self(), take},
   receive
      {ok, Value} -> Value
   end.

%% private

listen(Cap, Q) ->
   case {can_put(Cap, Q), can_take(Q)} of
      {true, true} -> %% can both put and take
         receive
            {From, {put, Value}} -> handle_put(Cap, Q, From, Value);
            {From, take} -> handle_take(Cap, Q, From)
         end;
      {true, false} -> %% can put
         receive
            {From, {put, Value}} -> handle_put(Cap, Q, From, Value)
         end;
      {false, true} -> %% can take
         receive
            {From, take} -> handle_take(Cap, Q, From)
         end
   end.

can_put(Cap, Q) -> Cap =:= 0 orelse queue:len(Q) < Cap.

can_take(Q) -> queue:len(Q) > 0.

handle_put(Cap, Q, From, Value) ->
   From ! ok,
   listen(Cap, queue:in(Value, Q)).

handle_take(Cap, Q, From) ->
   {Value, Q1} = queue:out(Q),
   From ! {ok, Value},
   listen(Cap, Q1).
