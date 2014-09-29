-module(kai_pool).
-author('Adam Rutkowski <hq@mtod.org>').

-export([init/1]).
-export([fetch/0]).
-export([join/0]).

-define(POOL, ?MODULE).

init(N) ->
    {ok, _} = pg2:start(),
    lager:info("KairosDB Initializing pool of ~w connections.", [N]),
    ok = pg2:create(?POOL),
    spawn_connections(N).

join() ->
    pg2:join(?POOL, self()).

fetch() ->
    case pg2:get_closest_pid(?POOL) of
        P when is_pid(P) ->
            {ok, P};
        {error, {no_process, _}} ->
            {error, no_connections_available};
        {error, _}=E ->
            E
    end.

spawn_connections(N) ->
    [ begin
          {ok, _Pid} = kai_conn_sup:start_child(),
          timer:sleep(1000)
      end || _ <- lists:seq(1, N) ],
    ok.


