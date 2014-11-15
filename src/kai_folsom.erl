-module(kai_folsom).
-author('Adam Rutkowski <hq@mtod.org>').

%%% kai.rest.query_metrics.OK - spiral
%%% kai.rest.query_metrics.NOK - spiral
%%% kai.rest.query_metrics.OK.latency - histogram
%%% kai.rest.query_metrics.NOK.latency - histogram
%%% kai.rest.query_metrics.OK.size - spiral
%%% kai.rest.query_metric_tags.OK - spiral
%%% kai.rest.query_metric_tags.NOK - spiral
%%% kai.rest.query_metric_tags.OK.latency - histogram
%%% kai.rest.query_metric_tags.NOK.latency - histogram
%%% kai.rest.query_metric_tags.OK.size - spiral
%%% kai.rest.delete_datapoints.OK - spiral
%%% kai.rest.delete_datapoints.NOK - spiral
%%% kai.rest.delete_datapoints.OK.latency - histogram
%%% kai.rest.delete_datapoints.NOK.latency - histogram
%%% kai.rest.delete_datapoints.OK.size - spiral
%%% kai.rest.delete_metric.OK - spiral
%%% kai.rest.delete_metric.NOK - spiral
%%% kai.rest.delete_metric.OK.latency - histogram
%%% kai.rest.delete_metric.NOK.latency - histogram
%%% kai.rest.delete_metric.OK.size - spiral
%%% kai.rest.list_metric_names.OK - spiral
%%% kai.rest.list_metric_names.NOK - spiral
%%% kai.rest.list_metric_names.OK.latency - histogram
%%% kai.rest.list_metric_names.NOK.latency - histogram
%%% kai.rest.list_metric_names.OK.size - spiral
%%% kai.rest.list_tag_names.OK - spiral
%%% kai.rest.list_tag_names.NOK - spiral
%%% kai.rest.list_tag_names.OK.latency - histogram
%%% kai.rest.list_tag_names.NOK.latency - histogram
%%% kai.rest.list_tag_names.OK.size - spiral
%%% kai.rest.list_tag_values.OK - spiral
%%% kai.rest.list_tag_values.NOK - spiral
%%% kai.rest.list_tag_values.OK.latency - histogram
%%% kai.rest.list_tag_values.NOK.latency - histogram
%%% kai.rest.list_tag_values.OK.size - spiral

-export([init_static_metrics/0]).

-export([name_rest_ok/1,
         name_rest_nok/1,
         name_rest_ok_lat/1,
         name_rest_nok_lat/1]).

-export([begin_rest_ok_lat/1,
         begin_rest_nok_lat/1]).

-export([notify_lat/1,
         notify_spiral/1]).

-define(REST, "kai.rest.").

-define(REST_ENDPOINTS, [query_metrics,
                         query_metrics_tags,
                         delete_datapoints,
                         delete_metric,
                         list_metric_names,
                         list_tag_values]).

init_static_metrics() ->
    Xs = [begin
         N1 = name_rest_ok(Call),
         N2 = name_rest_nok(Call),
         N3 = name_rest_ok_lat(Call),
         N4 = name_rest_nok_lat(Call),
         R1 = folsom_metrics:new_spiral(N1),
         lager:debug("Initializing spiral ~s (~p)", [N1, R1]),
         R2 = folsom_metrics:new_spiral(N2),
         lager:debug("Initializing spiral ~s (~p)", [N2, R2]),
         R3 = folsom_metrics:new_histogram(N3),
         lager:debug("Initializing histogram ~s (~p)", [N3, R3]),
         R4 = folsom_metrics:new_histogram(N4),
         lager:debug("Initializing histogram ~s (~p)", [N4, R4])
     end || Call <- ?REST_ENDPOINTS],
    true = lists:all(fun(ok) -> true end, Xs),
    ok.


begin_rest_ok_lat(Call) ->
    Name = name_rest_ok_lat(Call),
    folsom_metrics:histogram_timed_begin(Name).

begin_rest_nok_lat(Call) ->
    Name = name_rest_nok_lat(Call),
    folsom_metrics:histogram_timed_begin(Name).

notify_lat(LatMetric) ->
    ok = folsom_metrics:histogram_timed_notify(LatMetric).

notify_spiral(Name) ->
    ok = folsom_metrics:notify({Name, 1}).

name_rest_ok(Call) when is_atom(Call) ->
    <<?REST, (bin(Call))/binary, "OK">>.

name_rest_nok(Call) when is_atom(Call) ->
    <<?REST, (bin(Call))/binary, "NOK">>.

name_rest_ok_lat(Call) when is_atom(Call) ->
    <<?REST, (bin(Call))/binary, "OK.latency">>.

name_rest_nok_lat(Call) when is_atom(Call) ->
    <<?REST, (bin(Call))/binary, "NOK.latency">>.

bin(A) when is_atom(A) ->
    erlang:atom_to_binary(A, latin1).
