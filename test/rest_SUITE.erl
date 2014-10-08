-module(rest_SUITE).
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         t_simple_query_metric/1,
         t_put_and_get_metric/1,
         t_put_and_get_latest_metric/1]).

-define(q, kai_q).
-define(th, test_helpers).

-include_lib("common_test/include/ct.hrl").

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

init_per_testcase(TestCase, Config) ->
    random:seed(erlang:now()),
    MRand      = [ $z-random:uniform(24) || _ <- lists:seq(1, 10) ],
    TestName   = atom_to_binary(TestCase, utf8),
    MetricName = iolist_to_binary([TestName, "_", MRand]),
    {ok, _} = kai_rest:delete_metric(MetricName),
    ct:pal("Setting metric name ~s", [MetricName]),
    kvlists:set_value(metric_name, MetricName, Config).

end_per_testcase(_TestCase, Config) ->
    {ok, _} = kai_rest:delete_metric(mname(Config)),
    ok.

mname(Config) ->
    ?config(metric_name, Config).

t_simple_query_metric(Config) ->
    MN = mname(Config),
    Q1 = ?q:new({5, days}),
    M0 = ?q:metric(MN),
    Q2 = ?q:compose(Q1, [M0]),
    case kai_rest:query_metrics(Q2) of
        {ok,[{<<"queries">>,
              [[{<<"sample_size">>,0},
                {<<"results">>,
                 [[{<<"name">>,MN},
                   {<<"tags">>,[{}]},
                   {<<"values">>,[]}]]}]]}]}  ->
            ok;
        {error, {kairosdb, R}} ->
            {skip, R}
    end.

t_put_and_get_metric(Config) ->
    MN = mname(Config),
    Q1 = ?q:new({5, days}),
    M0 = ?q:metric(mname(Config)),
    Q2 = ?q:compose(Q1, [M0]),
    ok = kai:put_metric(MN, 666),
    timer:sleep(1000), % kairosdb buffer
    {ok, Results} = kai_rest:query_metrics(Q2),
    [{<<"queries">>,
      [[{<<"sample_size">>,1},
        {<<"results">>,
         [[{<<"name">>,MN},
           {<<"group_by">>,
            [[{<<"name">>,<<"type">>},{<<"type">>,<<"number">>}]]},
           {<<"tags">>,[{<<"add">>,[<<"tag">>]}]},
           {<<"values">>,[[_Timestamp,666]]}]]}]]}] = Results.

t_put_and_get_latest_metric(Config) ->
    Q1  = ?q:new({5, days}),
    MN  = mname(Config),
    M0  = ?q:metric(MN, 1000),
    M1  = ?q:order(M0, desc),
    Q2  = ?q:compose(Q1, [M1]),
    Ts1 = kai:now_to_epoch_msecs(),
    Ts2 = Ts1+1,
    ok  = kai:put_metric(MN, Ts1, 1, []),
    ok  = kai:put_metric(MN, Ts2, 2, []),
    timer:sleep(1000), % kairosdb buffer
    {ok, Results} = kai_rest:query_metrics(Q2),
    [{<<"queries">>,
      [[{<<"sample_size">>,2},
        {<<"results">>,
         [[{<<"name">>,MN},
           {<<"group_by">>,
            [[{<<"name">>,<<"type">>},{<<"type">>,<<"number">>}]]},
           {<<"tags">>,[{<<"add">>,[<<"tag">>]}]},
           {<<"values">>,
            [ [Ts2, 2], [Ts1, 1] ] }]]}]]}] = Results.
