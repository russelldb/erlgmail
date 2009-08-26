%%%-------------------------------------------------------------------
%%% File    : erlgmail.erl
%%% Author  : Russell Brown
%%% Description : Q n D email sender using gmail (cribbed from Benjamin Nortier's 21st C code works blog post http://21ccw.blogspot.com/2009/05/how-to-send-email-via-gmail-using.html)
%%%
%%% Created :  can't 'mber
%%%-------------------------------------------------------------------
-module(erlgmail).

-behaviour(gen_server).

%% API
-export([start_link/0, send/2, send/3, send/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("config.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{filename, filename:join(code:priv_dir(?MODULE), "erlgmail.cfg")}], []).


%%--------------------------------------------------------------------
%% Function: send  
%% Description: sends the  mail to recepients configured
%%--------------------------------------------------------------------
send(Subject, Body) ->
    gen_server:cast(?SERVER, {mail, {email, {subject, Subject}, {body, Body}, {to, []}, {header_to, []}}, 0}).

%%--------------------------------------------------------------------
%% Function: send  
%% Description: sends the  mail to recepients listed in To (a List)
%%--------------------------------------------------------------------
send(Subject, Body, To) ->
	gen_server:cast(?SERVER, {mail, {email, {subject, Subject}, {body, Body}, {to, To}, {header_to, []}}, 0}).

%%--------------------------------------------------------------------
%% Function: send  
%% Description: sends the  mail to recepients listed in To (a List) with the header_to addresses in HeaderTo (a List)
%%--------------------------------------------------------------------
send(Subject, Body,To, HeaderTo) ->
	gen_server:cast(?SERVER, {mail, {email, {subject, Subject}, {body, Body}, {to, To}, {header_to, HeaderTo}}, 0}).

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
init(L) ->
    Filename = proplists:get_value(filename, L),
    Config = config_reader:get_config(Filename),
    Socket = new_smtp:connect({config, Config#config.host, Config#config.port, Config#config.username, Config#config.password}),
    {ok, {Socket, Config}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({mail, {email, {subject, Subject}, {body, Body}, {to, To}, {header_to, HeaderTo}}=Message, Times}, {Socket, Config}=State) ->
    S = try ssl:connection_info(Socket) of
	    {ok, _} ->  
		Socket;
	    {error, _} ->
		new_smtp:connect({config, Config#config.host, Config#config.port, Config#config.username, Config#config.password})
	catch
	    _:_ ->
		new_smtp:connect({config, Config#config.host, Config#config.port, Config#config.username, Config#config.password})
	end,
	%%Set the who to
	Recipient = case To of
		[] -> Config#config.to;
		_ -> To
	end,
	%%And the who to to show
	HeaderRecipient = case HeaderTo of
		[] -> Config#config.header_to;
		_ -> HeaderTo
	end,
	
    try	new_smtp:send(S,  {message, Recipient, HeaderRecipient, Config#config.from, Subject, Body}) of
	S ->
	    {noreply, {S, Config}}
    catch
	exit:_ ->
	    handle_cast({mail, Message, Times+1}, State)
    end;
handle_cast({mail, Message, 3}, State) ->
    %%Log it, move on
    error_logger:error_msg("Failed to send message ~p~n", [Message]),
    {noreply, State};
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
terminate(_Reason, {Socket,_}) ->
    new_smtp:disconnect(Socket).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


