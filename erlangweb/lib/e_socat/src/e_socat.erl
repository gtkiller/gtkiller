-module(e_socat).

-behaviour(gen_server).
-behaviour(e_component).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% ecomponent callbacks
-export([install/1, uninstall/1, dependencies/0]).

%% api
-export([command/1]).

-record(state, {port_p}).

%%====================================================================
%% ecomponent callbacks
%%====================================================================
install(Conf) ->
    Spec = {?MODULE, {?MODULE, start_link, [Conf]}, 
	    permanent, 2000, worker, [?MODULE]},

    case supervisor:start_child(wpart, Spec) of
	{ok, _}    -> ok;
	{ok, _, _} -> ok;
        Else       -> Else
    end.

uninstall(_Conf) ->
    case supervisor:terminate_child(wpart, ?MODULE) of
	ok   -> supervisor:delete_child(wpart, ?MODULE);
	Else -> Else
    end.

dependencies() ->
    [].

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link(Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Conf, []).

command(Command) ->
    gen_server:call(?MODULE, {command, Command}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
    
init(_Conf) ->
    %ExtPrg = "socat UNIX-CLIENT:/var/run/b2bua.sock -",
    {ok, #state{port_p = init_port()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({command, Command}, _Caller,
        #state{port_p = Port} = State) ->

    Port ! {self(), {command, Command ++ [10]}},
    receive
	{Port, {data, Data}} -> {reply, re:replace(Data,"b2bua\s+\\$", "", [{return,list}]), State};
	{Port, Other} -> {reply, Other, State}
    after
	10000 -> {reply, "port timeout", State#state{port_p = reinit_port(Port)}}
    end;
handle_call(_Msg, _Caller, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    io:format("unknown cast ~p ~n", [_Msg]),
    {noreply, State}. 

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    io:format("unknown info ~p ~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

init_port() ->
    ExtPrg = e_conf:get_conf(socat_program),
    io:format("SOCAT: ~p ~n", [ExtPrg]),
    open_port({spawn, ExtPrg}, []).


reinit_port(Port) ->
    Port ! {self(), close},
    ExtPrg = e_conf:get_conf(socat_program),
    io:format("SOCAT: ~p ~n", [ExtPrg]),
    open_port({spawn, ExtPrg}, []).
