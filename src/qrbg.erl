-module(qrbg).


%% API
-export([connect/0
        ,get_response/3
        ,get_response/4
        ,extract_data/1
        ,extract_int/1
        ,extract_unsigned_int/1
        ,extract_short_int/1
        ,extract_unsigned_short_int/1
        ,extract_long_int/1
        ,extract_unsigned_long_int/1
        ,extract_byte/1
        ,extract_char/1
        ,extract_float/1
        ,extract_unsigned_float/1
        ,extract_double/1
        ,extract_unsigned_double/1
        ,extract_int/2
        ,extract_bytes/2
        ,rand_bytes/3
        ]).


-define(DEFAULT_REQUEST_SIZE, 4096).


%% ============================================================================
%% API
%% ============================================================================

connect() ->
    gen_tcp:connect("random.irb.hr", 1227, [binary, {packet, 0}]).


get_response(Socket, Username, Password) ->
    get_response(Socket, ?DEFAULT_REQUEST_SIZE, Username, Password).


get_response(Socket, RequestSize, Username, Password) ->
    ContentLength = length(Username) + length(Password) + 6,
    UsernameLength = length(Username),
    PasswordLength = length(Password),
    Data = list_to_binary(
        [<<0:8,ContentLength:16,UsernameLength:8>>
        ,Username
        ,<<PasswordLength:8>>
        ,Password
        ,<<RequestSize:32>>
        ]
    ),
    ok = gen_tcp:send(Socket, Data),
    % TODO: handle error conditions
    process_data(Socket, []).


extract_data(Bin) ->
    <<Response:8, Reason:8, Length:32, Data:Length/binary, _Rest/binary>> = Bin,
    % io:format("Response: ~w Reason: ~w Length: ~w Data: ~w~n", [Response, Reason, Length, Data]),
    {ok, Response, Reason, Length, Data}.


%%
%% Various data extraction bits
%%

extract_int(Bin) ->
    <<Int:32/integer-signed, Rest/binary>> = Bin,
    {Int, Rest}.


extract_unsigned_int(Bin) ->
    <<UnsignedInt:32/integer-unsigned, Rest/binary>> = Bin,
    {UnsignedInt, Rest}.


extract_short_int(Bin) ->
    <<ShortInt:16/integer-signed, Rest/binary>> = Bin,
    {ShortInt, Rest}.


extract_unsigned_short_int(Bin) ->
    <<UnsignedShortInt:16/integer-unsigned, Rest/binary>> = Bin,
    {UnsignedShortInt, Rest}.


extract_long_int(Bin) ->
    <<LongInt:64/integer-signed, Rest/binary>> = Bin,
        {LongInt, Rest}.


extract_unsigned_long_int(Bin) ->
    <<UnsignedLongInt:64/integer-unsigned, Rest/binary>> = Bin,
    {UnsignedLongInt, Rest}.


extract_byte(Bin) ->
    <<Byte:8/integer-unsigned, Rest/binary>> = Bin,
    {Byte, Rest}.


extract_char(Bin) ->
    extract_byte(Bin).


%%
%% TODO: Fix float/double functions
%%

extract_float(Bin) ->
    % Python: data = 0x3F800000 | (self.getInt() & 0x00FFFFFF)
    <<Float:32/float-signed, Rest/binary>> = Bin,
    {Float, Rest}.


extract_unsigned_float(Bin) ->
    <<UnsignedFloat:32/float-unsigned, Rest/binary>> = Bin,
    {UnsignedFloat, Rest}.


extract_double(Bin) ->
    % Python: data = 0x3FF0000000000000l | (self.getLong() & 0x000FFFFFFFFFFFFFl);
    <<Double:64/float-signed, Rest/binary>> = Bin,
    {Double, Rest}.


extract_unsigned_double(Bin) ->
    <<UnsignedDouble:64/float-unsigned, Rest/binary>> = Bin,
    {UnsignedDouble, Rest}.


%%
%% Not included in Python/C libs
%%

extract_int(Bin, Length) ->
    <<Int:Length/integer-signed, Rest/binary>> = Bin,
    {Int, Rest}.


extract_bytes(Bin, Length) ->
    <<Bytes:Length/binary, Rest/binary>> = Bin,
    {Bytes, Rest}.


%%
%% crypto compatability API
%%

rand_bytes(Number, Username, Password) ->
    {ok, Socket} = connect(),
    Response = get_response(Socket, Username, Password),
    {ok, _Response, _Reason, _Length, Data} = extract_data(Response),
    {Bytes, _Rest} = extract_bytes(Data, Number),
    Bytes.


%% ============================================================================
%% Internal
%% ============================================================================

process_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            process_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(SoFar))
    end.
