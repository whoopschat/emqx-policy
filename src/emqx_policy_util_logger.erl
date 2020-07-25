-module(emqx_policy_util_logger).

-define(LOG(Level, Format, Args), lager:log(Level, self(), Format, Args)).

-export([errorLog/2, infoLog/2]).

errorLog(Format, Args) ->
  log(error, Format, Args).

infoLog(Format, Args) ->
  log(info, Format, Args).

log(Level, Format, Args) ->
  try
    ?LOG(Level, Format, Args)
  catch
    throw:Term ->
      Term;
    exit:Reason ->
      Reason;
    error:Reason ->
      Reason
  end,
  ok.





