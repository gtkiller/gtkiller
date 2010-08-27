-module(e_gtk_logger).

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
-export([log/3, ttyon/0, ttyoff/0]).


-record(state, {
    loglevel = "DEBUG",
    tty = false
    }).

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

log(Mode, Message, Args) ->
    gen_server:cast(?MODULE, {log, Mode, Message, Args}).
    
ttyon() ->
    gen_server:cast(?MODULE, {ttyon}).
    
ttyoff() ->
    gen_server:cast(?MODULE, {ttyoff}).
    
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
    copy_old_log(),
    error_logger:logfile(close),
    error_logger:logfile({open, "log/gtkiller.log"}),
    error_logger:tty(false),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Msg, _Caller, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast({log, 'DEBUG', Message, Args}, State) ->
    error_logger:info_msg(Message, Args),
    {noreply, State};
handle_cast({log, 'WARNING', Message, Args}, State) ->
    error_logger:warning_msg(Message, Args),
    {noreply, State};
handle_cast({log, 'ERROR', Message, Args}, State) ->
    error_logger:error_msg(Message, Args),
    {noreply, State};  
handle_cast({log, _, Message, Args}, State) ->
    error_logger:error_msg(Message, Args),
    {noreply, State};
handle_cast({ttyon}, State) ->
    error_logger:tty(true),
    {noreply, State#state{tty=true}};
handle_cast({ttyoff}, State) ->
    error_logger:tty(false),
    {noreply, State#state{tty=false}};
handle_cast(_Msg, State) ->
    {noreply, State}. 

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    copy_old_log(),
    error_logger:logfile(close),
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

copy_old_log() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
    file:copy("log/gtkiller.log",
        lists:concat(["log/gtkiller",Year,"-",Month,"-",Day," ",Hour,":",Min,":",Sec,".log"])).

