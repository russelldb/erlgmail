-module(erlgmail_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_State) ->
  ErlgmailChild = {erlgmail,{erlgmail, start_link, []},
              permanent, 2000, worker, [erlgmail, new_smtp]},
  {ok,{{one_for_one,10,1}, [ErlgmailChild]}}.