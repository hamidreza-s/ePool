-module(process_pool).
-export([new/1,
         submit_task/2,
         fetch_task/1,
         submit_future/2,
         fetch_future/1]).

new(Threads) ->
   Q1 = blocking_queue:new(0),
   Q2 = blocking_queue:new(0),
   Qs = {Q1, Q2},
   create_workers(Threads, Qs),
   Qs.

%% ---

submit_task(Q1, Task) ->
   blocking_queue:put_in(Q1, {task, {Task}}).

fetch_task(Q2) ->
   blocking_queue:take_up(Q2).

%% --

submit_future(Q1, Task) ->
   Pid = spawn(fun() -> wait() end),
   blocking_queue:put_in(Q1, {future, {Pid, Task}}),
   Pid.

fetch_future(Pid) ->
   Pid ! {self(), get},
   receive
      Value -> Value
   end.

%% private

create_workers(0, _) -> ok;
create_workers(Threads, Qs) ->
   spawn(fun() -> worker(Qs) end),
   create_workers(Threads-1, Qs).

worker({Q1, Q2}) ->
   case blocking_queue:take_up(Q1) of
      {value, {task, {Task}}} ->
         blocking_queue:put_in(Q2, {done, Task()});
      {value, {future, {Pid, Task}}} ->
         Pid ! {done, Task()}
   end,
   worker({Q1, Q2}).

wait() ->
   receive
      {done, Value} -> wait(Value)
   end.

wait(Value) ->
   receive
      {From, get} ->
         From ! Value,
         wait(Value)
   end.
