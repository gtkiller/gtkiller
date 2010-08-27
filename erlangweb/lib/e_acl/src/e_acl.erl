-module(e_acl).

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
-export([is_allow/3, renew_config/0]).
-export([enable_renew_config/0, disable_renew_config/0]).
-include_lib("kernel/include/file.hrl").

-record(state, {etsid,
                check_acl_config = yes,
                configtime }).

%%====================================================================
%% ecomponent callbacks
%%====================================================================
install(Conf) ->
    console:log(["EUMSACLServer start!!!"]),
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

is_allow(Url, Group, User) ->
    gen_server:call(?MODULE, {is_allow, Url, Group, User}).

renew_config() ->
    gen_server:cast(?MODULE, {renew_config}).

disable_renew_config() ->
    gen_server:cast(?MODULE, {disable_renew_config}).

enable_renew_config() ->
    gen_server:cast(?MODULE, {enable_renew_config}).
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
    TableId = init_acl_ets(),
    {ok, #state{etsid = TableId, configtime = erlang:localtime()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({is_allow, Url, Group, User}, _Caller,
        #state{etsid = TableId,
            check_acl_config = Check,
            configtime = LastTime} = State) ->
    Url1 = modify_url(Url),
    console:log(["EUMSAclServer is_allow !!!!", Url1, Group, User]),
    case check_acl_config(Check, LastTime) of
        modify ->
            console:log(["acl.conf was modified - renew right now!"]),
            NewTableId = init_acl_ets(),
            ets:delete(TableId),
            Permission = checkPermissions(NewTableId, Url1, Group, User),
            {reply, {is_allow, Permission},
                State#state{etsid = NewTableId, configtime = erlang:localtime()}};
        _ ->
            Permission = checkPermissions(TableId, Url1, Group, User),
            {reply, {is_allow, Permission}, State}
    end;
handle_call(Msg, _Caller, State) ->
    console:log(["EUMSACLServer unknown call !!!!", Msg, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({enable_renew_config}, State) ->
    console:log(["UMSEACLserver enable_renew_config!!!!"]),
    {noreply, State#state{check_acl_config = yes}};
handle_cast({disable_renew_config}, State) ->
    console:log(["UMSEACLserver disable_renew_config!!!!"]),
    {noreply, State#state{check_acl_config = no}};
handle_cast({renew_config}, #state{etsid = TableId} = State) ->
    console:log(["UMSEACLserver renew config!!!!"]),
    NewTableId = init_acl_ets(),
    ets:delete(TableId),
    {noreply, State#state{etsid = NewTableId, configtime = erlang:localtime()}};
handle_cast(Msg, State) ->
    console:log(["EUMSACLServer unknown cast !!!!", Msg, State]),
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
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    console:log(["EUMSACLServer code_change callback inited !!!!"]),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

init_acl_ets() ->
    NewTableId = ets:new(eumsacl, []),
    List = try file:consult("config/acl.conf") of
        {ok, L} -> L;
        E -> console:log(["acl.conf error: ", E]),[]
    catch
        Type:Error -> console:log(["acl.conf error: ", Type, Error]), []
    end,
    console:log(["ACL Lines: ", List]),
    F = fun({Url, Groups, Users, Perm}) ->
        ets:insert(NewTableId,{Url, {Groups, Users, Perm}})
    end,
    lists:foreach(F, List),
    NewTableId.

modify_url([]) -> "/";
modify_url(Url) -> Url.

checkPermissions(TableId, Url, Group, User) ->
    case ets:lookup(TableId, Url) of
        [] -> true;
        [{_, {Groups, Users, Method}} | _T] ->
            GroupExist = xlists:is_nth(Group, Groups),
            UserExist  = xlists:is_nth(User, Users),
            get_permissions(GroupExist, UserExist, Method)
    end.

get_permissions(_, _, allow_access) -> true;
get_permissions(_, _, deny_access)  -> false;
get_permissions(G, U, allow_access_if) when G =/= false; U =/= false -> true;
get_permissions(false, false, allow_access_if) -> false;
get_permissions(G, U, allow_access_if_any) when G =/= false; U =/= false -> true;
get_permissions(false, false, allow_access_if_any) -> false;
get_permissions(false, false, deny_access_unless) -> true;
get_permissions(G, U, deny_access_unless) when G =/= false; U =/= false -> true;
get_permissions(false, false, deny_access_unless_any) -> true;
get_permissions(G, U, deny_access_unless_any) when G =/= false; U =/= false -> true;
get_permissions(_, _, _) -> true.

check_acl_config(Check, _) when Check =/= yes -> nonmodify;
check_acl_config(Check, LastTime) when Check =:= yes ->
    case file:read_file_info("config/acl.conf") of
        {ok, #file_info{mtime = NewTime}} when NewTime > LastTime -> modify;
        _ -> nonmodify
    end.
