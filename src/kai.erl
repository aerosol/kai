-module(kai).
-author('Adam Rutkowski <hq@mtod.org>').

-export([start/0]).
-export([stop/0]).

-export([env/1, env/2]).

-export([put_metric/2, put_metric/3, put_metric/4]).
-export([kairosdb_version/0]).

-export([now_to_epoch_msecs/0]).

-define(APP, ?MODULE).

-type metric_name()  :: binary().
-type metric_value() :: number().
-type metric_tag()   :: {binary(), binary()}.
-type metric_tags()  :: list(metric_tag()).
-type timestamp()    :: non_neg_integer().

-export_type([metric_name/0,
              metric_value/0,
              metric_tag/0,
              metric_tags/0,
              timestamp/0]).

start() ->
    application:ensure_all_started(kai).

stop() ->
    application:stop(kai).

env(K) ->
    env(K, undefined).

env(K, D) ->
    application:get_env(?APP, K, D).

-spec put_metric(metric_name(), metric_value()) -> ok | {error, any()}.
put_metric(M, V) ->
    put_metric(M, V, []).

-spec put_metric(metric_name(), metric_value(), metric_tags()) ->
    ok | {error, any()}.
put_metric(M, V, Tags) ->
    TS = now_to_epoch_msecs(),
    put_metric(M, TS, V, Tags).

-spec put_metric(metric_name(), timestamp(), metric_value(), metric_tags()) ->
    ok | {error, any()}.
put_metric(M, TS, V, Tags) ->
    case kai_pool:fetch() of
        {ok, Conn} ->
            kai_conn:put_metric(Conn, M, TS, V, Tags);
        {error, R} ->
            {error, {kairosdb, R}}
    end.

-spec kairosdb_version() ->
    {ok, {kairosdb, binary()}} | {error, any()}.
kairosdb_version() ->
    case kai_pool:fetch() of
        {ok, Conn} ->
            kai_conn:version(Conn);
        {error, R} ->
            {error, {kairosdb, R}}
    end.

now_to_epoch_msecs() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    Megasecs * 1000000000 + Secs * 1000 + Microsecs div 1000.
