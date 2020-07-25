-module(emqx_policy_util_format).

-export([validate_boolean/1, format_from/1, replace_str/3]).

validate_boolean(1) ->
  true;
validate_boolean(<<"1">>) ->
  true;
validate_boolean(<<"true">>) ->
  true;
validate_boolean(true) ->
  true;
validate_boolean(_Value) ->
  false.

format_from({ClientId, Username}) ->
  {ClientId, Username};
format_from(From) when is_atom(From) ->
  {a2b(From), a2b(From)};
format_from(From) when is_binary(From) ->
  {From, From};
format_from(_) ->
  {<<>>, <<>>}.

a2b(A) -> atom_to_binary(A, utf8).

replace_str(Str, Find, New) ->
  try
    Total = string:len(Str),
    Len = string:len(Find),
    Index = string:str(Str, Find),
    First = string:substr(Str, 1, Index - 1),
    End = string:substr(Str, Index + Len, Total - Index - Len + 1),
    string:concat(string:concat(First, New), End)
  catch
    throw:_Term ->
      Str;
    exit:_Reason ->
      Str;
    error:_Reason ->
      Str
  end;
replace_str(Str, _Find, _New) ->
  Str.

