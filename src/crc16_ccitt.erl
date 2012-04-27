%%%----------------------------------------------------------------------
%%% File    : crc16_ccitt.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : crc16 ccitt.
%%% Created : 29 Oct 2009
%%% License : http://www.opengoss.com
%%% 
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------

%%% CRC16/CCITT
%%%
%%% 1 + x + x^5 + x^12 + x^16 = 0x1021 is irreducible polynomial.
%%%
%%% "123456789" CRC16-CCITT = 29b1
-module(crc16_ccitt).

-define(POLYNOMIAL, 16#1021).

-export([calc/1]).

calc(Bin) ->
   calc(Bin, 16#0000).

calc(<<>>, CRC) ->
   CRC band 16#FFFF;

calc(<<Value:8, Rest/binary>>, CRC) when Value =< 255->
    NewCRC = calc_bit(8, Value, CRC),
    calc(Rest, NewCRC).

calc_bit(0, _Byte, NewCRC) ->
    NewCRC;

calc_bit(N, Byte, NewCRC) ->
    Bit = (Byte bsr (N-1)) band 1,
    C15 = (NewCRC bsr 15) band 1,
    NewCRC1 = NewCRC bsl 1,
    NewCRC2 = case Bit bxor C15 of
        1 -> NewCRC1 bxor ?POLYNOMIAL;
        0 -> NewCRC1
    end,
    calc_bit(N -1, Byte, NewCRC2).

