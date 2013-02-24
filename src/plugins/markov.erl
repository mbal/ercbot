-module(markov).
-export([train/3, generate_text/2]).

train(FileName, Order, Table) ->
    {ok, Co} = file:open(FileName, [read]),
    parse_file(Co, Order, Table, []).

parse_file(FileObj, Order, Table, RemWords) ->
    Line = file:read_line(FileObj),
    case Line of
        eof -> Table;
        {ok, Data} -> 
            {Tab2, RemainingWords} = parse_line(string:to_lower(Data), 
                                                Order, Table, RemWords),
            parse_file(FileObj, Order, Tab2, RemainingWords);
        Error -> Error
    end.

parse_line(Data, Order, Table, RemWords) ->
    case string:tokens(Data, "\r\n") of
        [] -> {Table, RemWords};
        Line ->
            StripLine = lists:nth(1, Line),
            Words = StripLine,
            insert_ngram(RemWords ++ Words, Order, Table)
    end.

%% This version of parse_line works with words, while the previous works 
%% with ngrams made of chars. 
%%parse_line(Data, Order, Table, RemWords) ->
%%    case string:tokens(Data, "\r\n") of
%%        [] -> {Table, RemWords};
%%        Line ->
%%            StripLine = lists:nth(1, Line),
%%            Words = string:tokens(StripLine, " "),
%%            insert_ngram(RemWords ++ Words, Order, Table)
%%    end.


insert_ngram([_|Rest]=Words, Order, Table) ->
    case length(Words) > Order of
        true ->
            Pre = lists:sublist(Words, Order),
            Post = lists:nth(Order+1, Words),
            Table2 = dict:update(Pre, fun(Old) ->
                                              [Post | Old] end, 
                                 [Post], Table),
            insert_ngram(Rest, Order, Table2);
        false ->
            %%the line doesn't always ends with the right number
            %%of words; so we must return the remaining words to the 
            %%caller, which will pass them to the next call to parse_line
            {Table, Words}
    end.

generate_text(Length, Table) ->
    generate_text(Length, Table, random_state(Table)).

generate_text(Length, Table, InitialState) ->
    generate_text_aux(Length, Table, InitialState, []).

generate_text_aux(Length, Table, [_|Rest]=PrevState, Acc) ->
    case length(Acc) >= Length of
        true ->
            lists:reverse(Acc);
        false ->
            case dict:find(PrevState, Table) of
                {ok, ListProbNextEnt} ->
                    NextWord = random_choice(ListProbNextEnt),
                    generate_text_aux(Length, Table, Rest ++ [NextWord], 
                                      [NextWord | Acc]);
                error -> 
                    generate_text_aux(Length, Table, random_state(Table), Acc)
            end
    end.


random_choice(List) ->
    lists:nth(random:uniform(length(List)), List).

random_state(Dictionary) ->
    random_choice(dict:fetch_keys(Dictionary)).

