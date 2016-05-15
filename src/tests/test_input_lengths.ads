-- Test_Input_Lengths

-- Ensure that headers and trailers of different lengths are accepted
-- and messages of different lengths correctly decrypted (to check padding)

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with NORX;

with System.Storage_Elements;
use System.Storage_Elements;

generic
   with package NORX_Package is new NORX(<>);
   Max_Size : System.Storage_Elements.Storage_Offset := 2000;
   Other_Size : System.Storage_Elements.Storage_Offset := 73;
procedure Test_Input_Lengths;
