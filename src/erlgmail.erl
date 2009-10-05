%%%-------------------------------------------------------------------
%%% File    : erlgmail.erl
%%% @author  Russell Brown <russell@ossme.net>
%%% @doc  Q n D gen_server email sender using gmail. See the send/n functions below. See the configuration section of the overview for profiles and configuration information.
%%% @reference <a href="http://21ccw.blogspot.com/2009/05/how-to-send-email-via-gmail-using.html">Benjamin Nortier's 21st C code works blog post </a>
%%% @version 1
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(erlgmail).

-behaviour(gen_server).

%% API
-export([start_link/0, send/2, send/3, psend/3, send/4, psend/4, psend/5]).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, application:get_all_env(), []).


%%--------------------------------------------------------------------  
%% @doc Sends an email with subject Subject and message body Body to the recepients configured in the default profile. Uses the default profile.
%% @spec send(Subject::string(), Body::string()) -> ok
%% @equiv erlgmail:send("Subject", "Body", default)
%% @end
%%--------------------------------------------------------------------
send(Subject, Body) ->
    psend(Subject, Body, default).

%%--------------------------------------------------------------------
%% @doc Sends an email with subject Subject and message body Body to the recepients configured in (and using) Profile profile
%% @spec psend(Subject::string(), Body::string(), Profile::profile()) -> ok
%% @type profile() = atom(). The name of a profile registered with the gen_server through the config file
%% @end
%%--------------------------------------------------------------------
psend(Subject, Body, Profile) when is_atom(Profile) ->
    psend(Subject, Body, [], [], Profile).

%%--------------------------------------------------------------------
%% @doc Sends an email with subject Subject and message body Body to the email address To
%% @spec send(Subject::string(), Body::string(), To::email()) -> ok
%% @type email() = string(). An email address (there is no parameter validation)
%% @end
%%--------------------------------------------------------------------
send(Subject, Body, To) when is_list(To) ->
    psend(Subject, Body, To, [], default).

%%--------------------------------------------------------------------
%% @doc Sends an email with subject Subject and message body Body to recipient To using Profile profile
%% @spec psend(Subject::string(), Body::string(), To::email(), Profile::profile()) -> ok
%% @end
%%--------------------------------------------------------------------
psend(Subject, Body, To, Profile) when is_atom(Profile) ->
    psend(Subject, Body, To, [], Profile).

%%--------------------------------------------------------------------
%% Function: send  
%% Description: sends the  mail to recipients in To with the display names in HeaderTo using the default profile
%%--------------------------------------------------------------------
send(Subject, Body, To, HeaderTo) ->
    psend(Subject, Body, To, HeaderTo, default).

%%--------------------------------------------------------------------
%% Function: send  
%% Description: sends the  mail to recipients in To using with display names in HeaderTo using Profile
%%--------------------------------------------------------------------
psend(Subject, Body, To, HeaderTo, Profile) ->
    gen_server:cast(?SERVER, {mail, {email, {subject, Subject}, {body, Body}, {to, To}, {header_to, HeaderTo}, {profile, Profile}}, 0}).


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
    case proplists:get_value(config_file, L) of
	undefined ->
	    {stop, missing_config_file};
	Filename ->
	    IsAbsolute = proplists:get_value(absolute, L, false),
	    ConfigFile = case IsAbsolute of
			     false ->
				 filename:join(code:priv_dir(?MODULE), Filename);
			     _ ->
				 Filename
			 end,
	    %% Get the dictionary of configname -> config records
	    Config = config_reader:get_config2(ConfigFile),

	    %% Create a connection socket for each config item and store them in a dictionary configname -> Socket
	    Sockets = dict:map(fun(_Key, Value) -> new_smtp:connect({config, Value#config.host, Value#config.port, Value#config.username, Value#config.password}) end, Config),
	    %%Put both same name keyed dictionaries into a tuple
	    {ok, {Sockets, Config}}
    end.

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
handle_cast({mail, Message, Times}, State) when Times > 3 ->
    %%Log it, move on
    error_logger:error_msg("Failed to send message ~p~n", [Message]),
    {noreply, State};
handle_cast({mail, {email, {subject, Subject}, {body, Body}, {to, To}, {header_to, HeaderTo}, {profile, Profile}}=Message, Times}, {Sockets, Configs}) ->
    %% Pull the correct socket from the state and use it
    Socket = dict:fetch(Profile, Sockets),
    Config = dict:fetch(Profile, Configs),
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

    try new_smtp:send(Socket,  {message, Recipient, HeaderRecipient, Config#config.from, Subject, Body}) of
        Socket ->
            {noreply, {Sockets, Configs}}
    catch
        exit:_ ->
            S = new_smtp:connect({config, Config#config.host, Config#config.port, Config#config.username, Config#config.password}),
            handle_cast({mail, Message, Times+1},  {dict:store(Profile, S, Sockets), Configs})
    end;
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
%% @spec terminate(Reason::term(), State::state()) -> ok
%% @doc Politely says bye to gmail for each Socket and closes it
%% @type state() = {sockets(), configs()}
%% @type sockets() = dict(profile(), socket()). A dictionary using profile() as key and a socket() as value
%% @type configs() = dict(profile(), config()). A dictionary using profile() as key and a config() as value
%% @type config() = {config, {host, port, username, password, from, to, header_to}}. The config record defined in config.hrl
%% @type socket() = term(). http socket
%% @type dict(). An Erlang dictionary (see dict)
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, {Sockets,_}) ->
    dict:map(fun(_K, V) -> new_smtp:disconnect(V) end, Sockets).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


