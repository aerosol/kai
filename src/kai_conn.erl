-module(kai_conn).
-author('Adam Rutkowski <hq@mtod.org>').

-behaviour(gen_fsm).

%% API
-export([start_link/0, start_link/1, start_link/3]).
-export([stop/1]).
-export([put_metric/5]).
-export([version/1]).

%% mainly for mocking
-export([send/2]).

%% gen_fsm callbacks
-export([init/1,
         connecting/2,
         connecting/3,
         connected/2,
         connected/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, { host          :: string(),
                 port          :: non_neg_integer(),
                 socket        :: port(),
                 ping          :: non_neg_integer() }).

-define(TCP_OPTS, [binary,{active, false},
                   {packet, line},{keepalive, true}]).

-define(RECONNECT_TIME_MSECS , 5000).
-define(TCP_RECV_LEN         , 0).
-define(TCP_RECV_TIMEOUT     , 5000).

start_link() ->
    {H, P} = endpoint(),
    start_link(H, P, []).

start_link(Opts) when is_list(Opts) ->
    {H, P} = endpoint(),
    start_link(H, P, Opts).

start_link(Host, Port, Opts) ->
    gen_fsm:start_link(?MODULE, [Host, Port, Opts], []).

stop(Conn) ->
    gen_fsm:sync_send_all_state_event(Conn, shutdown).

put_metric(Conn, M, TS, V, Tags) when is_pid(Conn) ->
    gen_fsm:sync_send_event(Conn, {put_metric, {M, TS, V, Tags}}).

version(Conn) ->
    gen_fsm:sync_send_event(Conn, version).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([H, P, Opts]) ->
    PI = case kai:env(telnet_api_ping_interval_secs) of
             I when is_integer(I) ->
                 lager:info("KairosDB scheduling ping in ~w secs", [I]),
                 I * 1000;
             undefined ->
                 undefined
         end,
    S = #state{host = H, port = P, ping = PI},
    try_connect(S, ?TCP_OPTS).

try_connect(S = #state{host=H, port=P}, Opts) ->
    case kairos_connect(S, Opts) of
        {ok, S2} ->
            {ok, {kairosdb, <<"KairosDB ", Ver/binary>>}} = kairos_version(S2),
            lager:info("KairosDB version: ~s", [Ver]),
            schedule_ping(S#state.ping),
            ok = kai_pool:join(),
            {ok, connected, S2};
        {error, R} ->
            lager:warning("Could not connect to KairoDB at ~s:~w due to ~p",
                         [H, P, R]),
            schedule_reconnect(Opts),
            {ok, connecting, S}
    end.

connecting({reconnect, Opts}, S1) ->
    case try_connect(S1, Opts) of
        {ok, connected, S2} ->
            {next_state, connected, S2};
        {ok, connecting, S1} ->
            schedule_reconnect(Opts),
            {next_state, connecting, S1}
    end.

connecting(_Event, _From, State) ->
    Reply = {error, connecting},
    {reply, Reply, connecting, State}.

connected(ping, S1) ->
    lager:debug("KairosDB: PING"),
    kai:put_metric(<<"dummy">>, 1, 1, []),
    {ok, {kairosdb, _}} = kairos_version(S1),
    lager:debug("KairosDB: PONG"),
    schedule_ping(S1#state.ping),
    {next_state, connected, S1};
connected(_, S1) ->
    {next_state, connected, S1}.

connected(version, _From, S1) ->
    Reply = kairos_version(S1),
    {reply, Reply, connected, S1};
connected({put_metric = Cmd, {M, TS, V, Tags}}, _From, S1) ->
    Raw = kai_proto:Cmd(M, TS, V, Tags),
    Reply = send(Raw, S1),
    {reply, Reply, connected, S1};
connected(_Event, _From, State) ->
    Reply = {error, unknown_call},
    {reply, Reply, connected, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(shutdown, _From, _, #state{socket=undefined}=S1) ->
    {stop, normal, ok, S1};
handle_sync_event(shutdown, _From, _, #state{socket=Socket}=S1) ->
    _ = gen_tcp:close(Socket),
    {stop, normal, ok, S1};
handle_sync_event(Event, _From, StateName, State) ->
    Reply = {error, {unknown_event, Event}},
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule_reconnect(Opts) ->
    gen_fsm:send_event_after(?RECONNECT_TIME_MSECS, {reconnect, Opts}).

schedule_ping(undefined) ->
    ok;
schedule_ping(Interval) ->
    gen_fsm:send_event_after(Interval, ping).

kairos_wait_reply(#state{socket = Socket}) ->
    case gen_tcp:recv(Socket, ?TCP_RECV_LEN, ?TCP_RECV_TIMEOUT) of
        {ok, Data} ->
            Data;
        {error, _}=E ->
            E
    end.

kairos_connect(#state{host=H, port=P} = S, Opts) ->
    lager:info("Connecting to KairosDB at ~s:~p", [H, P]),
    case gen_tcp:connect(H, P, Opts) of
        {ok, Socket} ->
            lager:info("Connected to KairosDB at ~s:~p", [H, P]),
            {ok, S#state{socket = Socket}};
        {error, _} = E ->
            E
    end.

kairos_version(S1) ->
    Raw = kai_proto:version(),
    ok = send(Raw, S1),
    case kairos_wait_reply(S1) of
        Data when is_binary(Data) ->
            {ok, {kairosdb, Data}};
        {error, _}=E ->
            E
    end.

send(_Raw, #state{socket=undefined}) ->
    {error, not_connected};
send(Raw, #state{socket=Socket}) ->
    gen_tcp:send(Socket, Raw).

endpoint() ->
    H = kai:env(telnet_api_host, "localhost"),
    P = kai:env(telnet_api_port, 4242),
    {H,P}.
