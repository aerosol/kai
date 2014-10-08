-module(rest_SUITE).
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         t_simple_query_metric/1,
         t_put_and_get_metric/1,
         t_put_and_get_latest_metric/1]).

-define(q, kai_q).
-define(th, test_helpers).

suite() ->
    [{timetrap,{seconds,30}}].

all() -> ?th:all_tests(?MODULE).

init_per_suite(Config) ->
    Config2 = ?th:set_common_data_dir(?MODULE, Config),
    {ok, _} = kai:start(),
    case kai:kairosdb_version() of
        {ok, {kairosdb, _}} ->
            Config2;
        {error, Reason} ->
            {skip, Reason}
    end.

end_per_suite(_Config) ->
    ok.

t_simple_query_metric(_) ->
    Q1 = ?q:new({5, days}),
    M0 = ?q:metric(<<"metric_up_in_dis">>, 666),
    Q2 = ?q:compose(Q1, [M0]),
    case kai_rest:query_metrics(Q2) of
        {ok,[{<<"queries">>,
              [[{<<"sample_size">>,0},
                {<<"results">>,
                 [[{<<"name">>,<<"metric_up_in_dis">>},
                   {<<"tags">>,[{}]},
                   {<<"values">>,[]}]]}]]}]}  ->
            {ok, no_content} = kai_rest:delete_datapoints(Q2),
            ok;
        {error, {kairosdb, R}} ->
            {skip, R}
    end.

t_put_and_get_metric(_) ->
    Q1 = ?q:new({5, days}),
    M0 = ?q:metric(<<"you_da_metric">>, 666),
    Q2 = ?q:compose(Q1, [M0]),
    case kai:put_metric(<<"you_da_metric">>, 666) of
        ok ->
            {ok, Results} = kai_rest:query_metrics(Q2),
            {ok, no_content} = kai_rest:delete_datapoints(Q2),
            [{<<"queries">>,
              [[{<<"sample_size">>,1},
                {<<"results">>,
                 [[{<<"name">>,<<"you_da_metric">>},
                   {<<"group_by">>,
                    [[{<<"name">>,<<"type">>},{<<"type">>,<<"number">>}]]},
                   {<<"tags">>,[{<<"add">>,[<<"tag">>]}]},
                   {<<"values">>,[[_Timestamp,666]]}]]}]]}] = Results;
        {error, {kairosdb, R}} ->
            {skip, R}
    end.

t_put_and_get_latest_metric(_) ->
    Q1 = ?q:new({5, days}),
    M0 = ?q:metric(<<"latest_metric">>, 1000),
    M1 = ?q:order(M0, desc),
    Q2 = ?q:compose(Q1, [M1]),
    Ts1 = kai:now_to_epoch_msecs(),
    Ts2 = Ts1+1,
    case kai:put_metric(<<"latest_metric">>, Ts1, 1, []) of
        ok ->
            case kai:put_metric(<<"latest_metric">>, Ts2, 2, []) of
                ok ->
                    timer:sleep(1000),  % write delay
                    {ok, Results} = kai_rest:query_metrics(Q2),
                    {ok, no_content} = kai_rest:delete_datapoints(Q2),
                    [{<<"queries">>,
                      [[{<<"sample_size">>,2},
                        {<<"results">>,
                         [[{<<"name">>,<<"latest_metric">>},
                           {<<"tags">>,[{<<"add">>,[<<"tag">>]}]},
                           {<<"values">>,
                            [[Ts2,2],[Ts1,1]]}]]}]]}] = Results;
                {error, {kairosdb, R}} ->
                    {skip, R}
            end;
        {error, {kairosdb, R}} ->
            {skip, R}
    end.
