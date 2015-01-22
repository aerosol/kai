-module(kai_rest).
-author('Adam Rutkowski <hq@mtod.org>').

-export([delete_datapoints/1,
         delete_metric/1,
         list_metric_names/0,
         list_tag_names/0,
         list_tag_values/0,
         query_metrics/1,
         query_metric_tags/1
        ]).

-define(base         , "/api/v1/").
-define(HTTP_TIMEOUT , timer:seconds(15)).

%% Rest API

query_metrics(Q) ->
    call(query_metrics, "POST", Q).

query_metric_tags(Q) ->
    call(query_metric_tags, "POST", Q).

delete_datapoints(Q) ->
    call(delete_datapoints, "POST", Q).

delete_metric(Name) ->
    call(delete_metric, "DELETE", Name, []).

list_metric_names() ->
    call(list_metric_names, "GET").

list_tag_names() ->
    call(list_tag_names, "GET").

list_tag_values() ->
    call(list_tag_values, "GET").

call(ApiMethod, HttpMethod) ->
    call(ApiMethod, HttpMethod, []).

call(ApiMethod, HttpMethod, Payload)
  when is_list(Payload) ->
    call(ApiMethod, HttpMethod, jsx:encode(Payload));
call(ApiMethod, HttpMethod, Payload)
  when is_binary(Payload) ->
    call(ApiMethod, HttpMethod, [], Payload).

call(ApiMethod, HttpMethod, Suffix, Payload) ->
    URI = uri(ApiMethod, Suffix),
    HandleReply = reply_handler(ApiMethod, HttpMethod),

    LatOK  = kai_folsom:begin_rest_ok_lat(ApiMethod),
    LatNOK = kai_folsom:begin_rest_nok_lat(ApiMethod),

    SpiOK  = kai_folsom:name_rest_ok(ApiMethod),
    SpiNOK = kai_folsom:name_rest_nok(ApiMethod),

    Result = case request(HttpMethod, URI, Payload) of
                 {error, R} ->
                     kai_folsom:notify_lat(LatNOK),
                     kai_folsom:notify_spiral(SpiNOK),
                     {error, {kairosdb, R}};
                 {ok, {{Status, _}, _, P}} ->
                     case lists:member(Status, [200, 201, 202, 204]) of
                         true ->
                             PSize = byte_size(P),
                             Name = kai_folsom:name_rest_ok_size(ApiMethod),
                             kai_folsom:notify_hist(Name, PSize),
                             RespPayload = decode(P),
                             Reply = HandleReply(RespPayload),
                             kai_folsom:notify_lat(LatOK),
                             kai_folsom:notify_spiral(SpiOK),
                             Reply;
                         false ->
                             kai_folsom:notify_lat(LatNOK),
                             kai_folsom:notify_spiral(SpiNOK),
                             {error, {kairosdb, {Status, P}}}
                     end
             end,
    Result.


%% Internals

endpoint() ->
    H = kai:env(rest_api_host, "localhost"),
    P = kai:env(rest_api_port, 8080),
    S = kai:env(rest_api_scheme, "http"),
    S ++ "://" ++ H ++ ":" ++ integer_to_list(P).

uri(Method, Suffix)
  when is_binary(Suffix) ->
    uri(Method, binary_to_list(Suffix));
uri(Method, Suffix) ->
    endpoint() ++ ?base ++ method_uri(Method) ++ "/" ++ Suffix.

reply_handler(query_metrics, _) -> fun results_per_query/1;
reply_handler(_, _)             -> fun noop/1.

noop(X) -> X.

results_per_query(B) ->
    B.

method_uri(delete_datapoints) -> "datapoints/delete";
method_uri(delete_metric)     -> "metric";
method_uri(list_metric_names) -> "metricnames";
method_uri(list_tag_names)    -> "tagnames";
method_uri(list_tag_values)   -> "tagvalues";
method_uri(query_metrics)     -> "datapoints/query";
method_uri(query_metric_tags) -> "datapoints/query/tags".

request(Method, URI, []) ->
    request(Method, URI, [], []);
request(Method, URI, Payload)
  when is_binary(Payload) ->
    request(Method, URI, Payload, [{"Content-Type", "application/json"}]).

request(Method, URI, Payload, Headers) ->
    Opts = kai:env(dlhttpc_req_opts),
    dlhttpc:request(URI, Method, Headers, Payload, ?HTTP_TIMEOUT, Opts).

decode(<<>>) ->
    {ok, no_content};
decode(B) when is_binary(B) ->
    {ok, jsx:decode(B)}.
