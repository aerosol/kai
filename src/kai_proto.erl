-module(kai_proto).
-author('Adam Rutkowski <hq@mtod.org>').

-export([put_metric/3, put_metric/4]).
-export([version/0]).

-type raw_cmd()   :: binary().
-type conn()      :: pid().

-export_type([raw_cmd/0, conn/0]).

-define(FLOAT_OPTS_DEFAULT , [{decimals , 8} , compact]).
-define(NL                 , <<"\n">>).

-spec version() -> raw_cmd().
version() ->
    encode(version).

-spec put_metric(kai:metric_name(), kai:timestamp(), kai:metric_value()) ->
    raw_cmd().
put_metric(M, TS, V) ->
    encode(put, M, TS, V).

-spec put_metric(kai:metric_name(), kai:timestamp(),
                 kai:metric_value(), kai:metric_tags()) ->
    raw_cmd().
put_metric(M, TS, V, Tags) ->
    encode(put, M, TS, V, Tags).

encode(version) ->
    <<"version\n">>.

encode(put, M, TS, V) ->
    encode(put, M, TS, V, []).

encode(put, M, TS, V, Tags) ->
    encode(put, M, TS, V, Tags, ?FLOAT_OPTS_DEFAULT).

encode(put, M, TS, V, Tags, _FloatOpts)
  when is_integer(V) ->
    encode_put(M, TS, integer_to_binary(V), Tags);
encode(put, M, TS, V, Tags, FloatOpts)
  when is_float(V) ->
    encode_put(M, TS, float_to_binary(V, FloatOpts), Tags).

encode_put(M, TS, V, Tags)
  when is_binary(M)
      andalso is_integer(TS)
      andalso is_binary(V)
      andalso is_list(Tags) ->
    TSBin = integer_to_binary(TS),
    TagsBin = encode_tags(Tags),
    <<"put ", M/binary, " ", TSBin/binary,
      " ", V/binary, TagsBin/binary, ?NL/binary>>.

encode_tags(Tags) ->
    << <<" ", K/binary, "=", V/binary>> || {K,V} <- Tags >>.
