-module(qrbg).


%% API
-export([connect/0
        ,connect/2
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


-define(DEFAULT_ADDRESS, "random.irb.hr").
-define(DEFAULT_PORT, 1227).
-define(DEFAULT_REQUEST_SIZE, 4096).


%% ============================================================================
%% API
%% ============================================================================

connect() ->
    connect(?DEFAULT_ADDRESS, ?DEFAULT_PORT).


connect(Address, Port) ->
    gen_tcp:connect(Address, Port, [binary, {packet, 0}]).


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
    <<Response:8
     ,Reason:8
     ,Length:32
     ,Data:Length/binary
     ,_Rest/binary
     >> = Bin,

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
%% Floats in range 0-1
%%

extract_float(Bin) ->
    <<Float:32/float-signed, Rest/binary>> = Bin,
    {normalized(Float), Rest}.


extract_unsigned_float(Bin) ->
    <<UnsignedFloat:32/float-unsigned, Rest/binary>> = Bin,
    {normalized(UnsignedFloat), Rest}.


extract_double(Bin) ->
    <<Double:64/float-signed, Rest/binary>> = Bin,
    {normalized(Double), Rest}.


extract_unsigned_double(Bin) ->
    <<UnsignedDouble:64/float-unsigned, Rest/binary>> = Bin,
    {normalized(UnsignedDouble), Rest}.


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

%%
%% Float in range 0-1
%%
normalized(Float) ->
    abs(Float - trunc(Float)).


process_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            process_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(SoFar))
    end.
