-module(ngram).
-export([incremental_train_from_line/4, generate_from_file/3, generate_text/4]).

incremental_train_from_line(Line, N, Length, Prev) ->
    Dict = get_ngram(Line, N, Prev),
    Dict.
    %string:join(generate_text(Dict, State0, [], Length), "").

generate_from_file(FName, N, Length) ->
    {ok, Co} = file:read_file(FName),
    Text = binary_to_list(Co),
    Dict = get_ngram(Text, N, dict:new()),
    State0 = random_state(Dict),
    string:join(generate_text(Dict, State0, [], Length), "").

random(Max) ->
    random:seed(now()),
    random:uniform(Max).

random_state(Dict) -> 
    L = dict:fetch_keys(Dict),
    random_choice(L).

random_choice(List) ->
    lists:nth(random(length(List)), List).

generate_text(Dict, [_|T]=State, Acc, Length) ->
    case length(Acc) > Length of
        true -> Acc;
        false ->
            Res = dict:find(State, Dict),
            case Res of
                error -> generate_text(Dict, random_state(Dict), Acc, Length);
                _ -> {ok, NC} = Res,
                    NChar = random_choice(NC),
                    generate_text(Dict, T ++ NChar, Acc ++ [NChar], Length)
            end
    end.

get_ngram([], _, Dict) -> Dict;
get_ngram([_|R]=Text, N, Dict) ->
    Len = string:len(Text),
    if
        Len < N -> Dict;
        true ->
            Xp = string:to_lower(string:substr(Text, 1, N-1)),
            Nc = string:to_lower(string:substr(Text, N, 1)),
            NDict = dict:update(Xp, fun(Old) -> [Nc | Old] end, [Nc], Dict),
            get_ngram(R, N, NDict)
    end.
