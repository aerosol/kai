-module(kai_units_SUITE).
-export([init_per_suite/1,
         end_per_suite/1,
         suite/0,
         all/0,
         t_build_query_from_kairos_docs/1,
         t_p_query_is_always_valid_json/1,
         t_kai_proto_typespecs/1,
         t_kai_q_typespecs/1,
         t_encode_put/1,
         t_encode_put_no_tags/1,
         t_encode_put_int/1,
         t_encode_version/1]).
         %% FIXME t_fetch_version/1]).

-define(q, kai_q).
-define(th, test_helpers).

-include_lib("proper/include/proper.hrl").

-define(NUMTESTS, 100).
-define(PROPTEST(A), true = proper:quickcheck(A(),
                                              [{numtests, ?NUMTESTS},
                                               {constraint_tries, 1000}])).

init_per_suite(Config) ->
    Config2 = ?th:set_common_data_dir(?MODULE, Config),
    {ok, _} = kai:start(),
    Config2.

end_per_suite(_Config) ->
    ok = kai:stop().

suite() ->
    [{timetrap,{seconds,30}}].

all() -> ?th:all_tests(?MODULE).

t_build_query_from_kairos_docs(Config) ->
    Fixt   = ?th:load_fixture(Config, "sample_query.json"),
    Expect = jsx:decode(Fixt),

    Q1     = ?q:new(1357023600000, {5, days}),

    Mabc0  = ?q:metric(<<"abc.123">>, 1000),
    Mabc1  = ?q:tag(Mabc0, host, [foo, foo2]),
    Mabc2  = ?q:tag(Mabc1, customer, <<"bar">>),
    Mabc3  = ?q:sum(Mabc2, {10, minutes}),

    Mxyz0  = ?q:metric(<<"xyz.123">>),
    Mxyz1  = ?q:tag(Mxyz0, host, [foo, foo2]),
    Mxyz2  = ?q:tag(Mxyz1, customer, <<"bar">>),
    Mxyz3  = ?q:avg(Mxyz2, {10, minutes}),

    Q2     = ?q:compose(Q1, [Mabc3, Mxyz3]),

    ct:pal("Q2 ~p", [Q2]),

    QData  = jsx:decode(jsx:encode(Q2)),

    ct:pal("Expect ~p", [Expect]),
    ct:pal("Got ~p", [QData]),

    Expect = QData.

t_p_query_is_always_valid_json(_) ->
    ?PROPTEST(prop_cannot_encode_with_no_metrics).

prop_cannot_encode_with_no_metrics() ->
    ?FORALL(Query,
            kai_q:q(),
            begin
                {error, no_metrics} == kai_q:compose(Query, [])
            end).

t_kai_q_typespecs(_) ->
    [] = proper:check_specs(kai_q).

t_kai_proto_typespecs(_) ->
    [] = proper:check_specs(kai_proto).

t_encode_put(_) ->
    R = kai_proto:put_metric(<<"hello_metric">>, 1234, 10.0,
                             [{<<"tag1">>, <<"v1">>},
                              {<<"tag2">>, <<"v2">>}]),
    <<"put hello_metric 1234 10.0 tag1=v1 tag2=v2\n">> = R.

t_encode_put_no_tags(_) ->
    R = kai_proto:put_metric(<<"hello_metric">>, 1234, 10.0),
    <<"put hello_metric 1234 10.0\n">> = R.

t_encode_put_int(_) ->
    R = kai_proto:put_metric(<<"hello_metric">>, 1234, 10),
    <<"put hello_metric 1234 10\n">> = R.

t_encode_version(_) ->
    <<"version\n">> = kai_proto:version().

t_fetch_version(_) ->
    case kai:kairosdb_version() of
        {ok, {kairosdb, _V}} ->
            ok;
        {error, {kairosdb, R}} ->
            {skip, R}
    end.
