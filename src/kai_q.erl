-module(kai_q).
-author('Adam Rutkowski <hq@mtod.org>').

-export([new/1, new/2]).
-export([metric/1, metric/2]).
-export([tag/3]).
-export([sum/2, avg/2]).
-export([compose/2]).

-type q_time_unit()         :: milliseconds
                             | seconds
                             | minutes
                             | hours
                             | days
                             | weeks
                             | months
                             | years
                             .
-type timestamp()           :: pos_integer().
-type q_time_val_abs()      :: timestamp().
-type q_time_val_rel()      :: {pos_integer(), q_time_unit()}.
-type q_time_val()          :: q_time_val_rel()
                             | q_time_val_abs().

-type q_time()              :: q_time_val_rel()
                             | q_time_val_abs()
                             | undefined
                             .
-type metric_tag()          :: binary() | atom().
-type metric_tag_values()   :: binary() | atom() | [binary() | atom()].
-type metric_tag_filter()   :: {metric_tag(), metric_tag_values()}.
-type metric_tag_filters()  :: [metric_tag_filter()].
-type aggregator_name()     :: avg
                             | sum
                             .
-type aggregator_sampling() :: q_time_val_rel().

-record(q_metric_aggregator, {
          name     :: aggregator_name(),
          sampling :: aggregator_sampling()
         }).

-type metric_aggregators() :: [#q_metric_aggregator{}].

-record(q_metric, {
          name             :: kai:metric_name(),
          limit            :: non_neg_integer(),
          tags        = [] :: metric_tag_filters(),
          aggregators = [] :: metric_aggregators()
         }).

-record(q, {
          start_time :: q_time(),
          end_time   :: q_time(),
          metrics    :: [q_metric()]
         }).

-opaque q() :: #q{}.
-opaque q_metric() :: #q_metric{}.

-export_type([q/0, q_metric/0]).

%%% API

-spec new(q_time_val()) ->
    q() | {error, term()}.
new(Start) ->
    #q{start_time=Start}.

-spec new(q_time_val(), q_time_val()) ->
    q().
new(Start, End) ->
    #q{start_time=Start, end_time=End}.

-spec metric(kai:metric_name()) ->
    q_metric().
metric(Name) ->
    #q_metric{name=Name}.

-spec metric(kai:metric_name(),
             pos_integer()) ->
    q_metric().
metric(Name, Limit) ->
    #q_metric{name=Name, limit=Limit}.

-spec tag(q_metric(), metric_tag(), metric_tag_values()) ->
    q_metric().
tag(Metric, _Tag, []) ->
    Metric;
tag(Metric, Tag, V) ->
    Tags = Metric#q_metric.tags,
    Metric#q_metric{tags=[{Tag, wrap_list(V)}|Tags]}.

-spec sum(q_metric(), aggregator_sampling()) ->
    q_metric().
sum(Metric, {_V,_U}=Sampling) ->
    Ags = Metric#q_metric.aggregators,
    Ag = #q_metric_aggregator{name=sum, sampling=Sampling},
    Metric#q_metric{aggregators = [Ag|Ags]}.

-spec avg(q_metric(), aggregator_sampling()) ->
    q_metric().
avg(Metric, {_V, _U}=Sampling) ->
    Ags = Metric#q_metric.aggregators,
    Ag = #q_metric_aggregator{name=avg, sampling=Sampling},
    Metric#q_metric{aggregators = [Ag|Ags]}.

-spec compose(q(), q_metric() | [q_metric()]) ->
    jsx:json_term() | {error, no_metrics}.
compose(_Q, []) ->
    {error, no_metrics};
compose(Q, #q_metric{}=M) ->
    compose(Q, [M]);
compose(Q, Metrics) when is_list(Metrics) ->
    to_json(Q#q{metrics = Metrics}).

-spec to_json(q()) ->
    jsx:json_term().
to_json(#q{start_time=T1, end_time=T2, metrics=M}) ->
    StartTime = start_time(T1),
    EndTime   = end_time(T2),
    HowTo = [StartTime,
             EndTime,
             {metrics, M, fun unwrap_metrics/1}],
    unwrap(HowTo).

unwrap_metrics(Metrics) ->
    [unwrap_metric(M) || M <- Metrics].

unwrap_metric(#q_metric{name=N, limit=L, tags=T, aggregators=A}) ->
    HowTo = [{name,        N, noop},
             {limit,       L, noop},
             {tags,        T, fun unwrap_tags/1},
             {aggregators, A, fun unwrap_aggregators/1}],
    unwrap(HowTo).

unwrap_tags(Tags) ->
    lists:reverse([unwrap_tag(T) || T <- Tags]).

unwrap_tag({K, [_|_]=V}) ->
    {K, V};
unwrap_tag({K, V}) ->
    {K, [V]}.

unwrap_aggregators(Ags) ->
    [unwrap_aggregator(Ag) || Ag <- Ags].

unwrap_aggregator(#q_metric_aggregator{name=N, sampling={V,U}}) ->
    [{name, N},{sampling, [value(V),unit(U)]}].

unwrap(HowTo) ->
    lists:foldr(fun({_Key, undefined, _Unwrap}, AccIn) ->
                        AccIn;
                   ({Key, Value, noop}, AccIn) ->
                        [{Key,Value}|AccIn];
                   ({_Key, [], _Unwrap}, AccIn) ->
                        % kairosdb cannot into empty lists
                        % java.lang.IllegalStateException: Not a JSON Object: []
                        AccIn;
                   ({Key, Value, Unwrap}, AccIn) ->
                        [{Key,Unwrap(Value)}|AccIn]
                end, [], HowTo).

%%% Internal

start_time(undefined) ->
    {undefined, undefined, noop};
start_time(V) when is_integer(V) ->
    {start_absolute, V, noop};
start_time({V, U}) ->
    {start_relative, [value(V), unit(U)], noop}.


end_time(undefined) ->
    {undefined, undefined, noop};
end_time(V) when is_integer(V) ->
    {end_absolute, V, noop};
end_time({V, U}) ->
    {end_relative, [value(V), unit(U)], noop}.


unit(milliseconds = U) -> {unit, U};
unit(seconds      = U) -> {unit, U};
unit(minutes      = U) -> {unit, U};
unit(hours        = U) -> {unit, U};
unit(days         = U) -> {unit, U};
unit(weeks        = U) -> {unit, U};
unit(months       = U) -> {unit, U};
unit(years        = U) -> {unit, U}.


value(V) ->
    {value, V}.

wrap_list([_|_]=V) -> V;
wrap_list(V)       -> [V].
