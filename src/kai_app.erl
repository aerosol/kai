-module(kai_app).
-author('Adam Rutkowski <hq@mtod.org>').

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, TopSup} = kai_sup:start_link(),
    ok = case kai:env(telnet_connections, 0) of
             N when is_integer(N) andalso N > 0 ->
                 kai_pool:init(N);
             _ ->
                 ok
         end,
    {ok, TopSup}.

stop(_State) ->
    ok.
