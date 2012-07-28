qrbg-erlang
===========


Description
-----------
An Erlang API to the Quantum Random Bit Generator service at
http://random.irb.hr/

Derived from the original work of Matt Croydon, found at
http://code.google.com/p/qrbgerl/


Example
-------
```erlang
1> c(qrbg).
{ok,qrbg}
2> {ok, Socket} = qrbg:connect().
{ok,#Port<0.127>}
3> Response = qrbg:get_response(Socket, "username", "password").
<<0,0,0,0,16,0,230,132,193,235,0,254,163,8,239,180,51,164,169,160,170,248,94,
  132,220,79,234,4,117,...>>
4> {ok, _Response, _Reason, _Length, Data} = qrbg:extract_data(Response).
{ok,0,
    0,
    4096,
    <<230,132,193,235,0,254,163,8,239,180,51,164,169,160,170,248,94,132,220,
      79,234,4,117,248,...>>}
5> {Int, RestData} = qrbg:extract_int(Data).
{-427507221,
 <<0,254,163,8,239,180,51,164,169,160,170,248,94,132,220,79,234,4,117,248,174,
   59,167,49,165,170,154,...>>}
6> Int.
-427507221
7> {Int2, MoreRestData} = qrbg:extract_int(RestData).
{16687880,
 <<239,180,51,164,169,160,170,248,94,132,220,79,234,4,117,248,174,59,167,49,
   165,170,154,146,102,164,89,...>>}
8> Int2.
16687880
```
