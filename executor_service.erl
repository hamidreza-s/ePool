-module(executor_service).
-export([new/1,
         submit/2,
         get/1]).

new(Threads) ->
   Q = blocking_queue:new(0),
   create_workers(Threads, Q),
   Q.

submit(Q, Task) ->
   Pid = spawn(fun() -> wait() end),
   blocking_queue:put_in(Q, {Pid, Task}),
   Pid.

get(Pid) ->
   Pid ! {self(), get},
   receive
      Value -> Value
   end.

%% private

create_workers(0, _) -> ok;
create_workers(Threads, Q) ->
   spawn(fun() -> worker(Q) end),
   create_workers(Threads-1, Q).

worker(Q) ->
   {value, {Pid, Task}} = blocking_queue:take_up(Q),
   Pid ! {done, Task()},
   worker(Q).

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
