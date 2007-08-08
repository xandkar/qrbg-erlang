-module(qrbg).

%
% API
%

-export([connect/0, get_response/3, extract_data/1]).
-export([extract_int/1]).

connect() ->
    gen_tcp:connect("random.irb.hr", 1227, [binary, {packet, 0}]).

get_response(Socket, Username, Password) ->
    ContentLength = length(Username) + length(Password) + 6,
    UsernameLength = length(Username),
    PasswordLength = length(Password),
    Data = list_to_binary([<<0:8,ContentLength:16,UsernameLength:8>>, Username, <<PasswordLength:8>>, Password, <<4096:32>>]),
    ok = gen_tcp:send(Socket, Data),
    % TODO: handle error conditions
    process_data(Socket, []).

extract_data(Bin) ->
    <<Response:8, Reason:8, Length:32, Data:Length/binary, _Rest/binary>> = Bin,
    % io:format("Response: ~w Reason: ~w Length: ~w Data: ~w~n", [Response, Reason, Length, Data]),
    {ok, Response, Reason, Length, Data}.

% Various data extraction bits

extract_int(Bin) ->
    Len = 8 * 4,
    <<Int:Len/integer-signed, Rest/binary>> = Bin,
    {Int, Rest}.

%
% crypto compatability API
%

%
% Internal
%

process_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            process_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(SoFar))
    end.
