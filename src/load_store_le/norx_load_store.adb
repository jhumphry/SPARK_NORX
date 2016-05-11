-- NORX_Load_Store
-- A collection of functions to load and store words of different sizes from
-- Storage_Array in Little Endian format. This version assumes that the machine
-- is Little Endian and does unchecked conversions to convert the types.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

-- Note that all the Unsigned_xx types count as Implementation_Identifiers
pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

with Ada.Unchecked_Conversion;

package body NORX_Load_Store
with SPARK_Mode => Off
is

   function Storage_Array_To_Unsigned_8
     (S : in Storage_Array)
      return Unsigned_8
   is
      function SA_To_U8_Unchecked is
        new Ada.Unchecked_Conversion(Source => Storage_Array,
                                     Target => Unsigned_8);
   begin
      return SA_To_U8_Unchecked(S);
   end Storage_Array_To_Unsigned_8;

   function Unsigned_8_To_Storage_Array
     (W : in Unsigned_8)
      return Storage_Array
   is
      subtype SA1 is Storage_Array(0..0);

      function U8_To_SA_Unchecked is
        new Ada.Unchecked_Conversion(Source => Unsigned_8,
                                     Target => SA1);
   begin
      return U8_To_SA_Unchecked(W);
   end Unsigned_8_To_Storage_Array;

   function Storage_Array_To_Unsigned_16
     (S : in Storage_Array)
      return Unsigned_16
   is
      function SA_To_U16_Unchecked is
        new Ada.Unchecked_Conversion(Source => Storage_Array,
                                     Target => Unsigned_16);
   begin
      return SA_To_U16_Unchecked(S);
   end Storage_Array_To_Unsigned_16;

   function Unsigned_16_To_Storage_Array
     (W : in Unsigned_16)
      return Storage_Array
   is
      subtype SA2 is Storage_Array(0..1);

      function U16_To_SA_Unchecked is
        new Ada.Unchecked_Conversion(Source => Unsigned_16,
                                     Target => SA2);
   begin
      return U16_To_SA_Unchecked(W);
   end Unsigned_16_To_Storage_Array;

   function Storage_Array_To_Unsigned_32
     (S : in Storage_Array)
      return Unsigned_32
   is
      function SA_To_U32_Unchecked is
        new Ada.Unchecked_Conversion(Source => Storage_Array,
                                     Target => Unsigned_32);
   begin
      return SA_To_U32_Unchecked(S);
   end Storage_Array_To_Unsigned_32;

   function Unsigned_32_To_Storage_Array
     (W : in Unsigned_32)
      return Storage_Array
   is
      subtype SA4 is Storage_Array(0..3);

      function U32_To_SA_Unchecked is
        new Ada.Unchecked_Conversion(Source => Unsigned_32,
                                     Target => SA4);
   begin
      return U32_To_SA_Unchecked(W);
   end Unsigned_32_To_Storage_Array;

   function Storage_Array_To_Unsigned_64
     (S : in Storage_Array)
      return Unsigned_64
   is
      function SA_To_U64_Unchecked is
        new Ada.Unchecked_Conversion(Source => Storage_Array,
                                     Target => Unsigned_64);
   begin
      return SA_To_U64_Unchecked(S);
   end Storage_Array_To_Unsigned_64;

   function Unsigned_64_To_Storage_Array
     (W : in Unsigned_64)
      return Storage_Array
   is
      subtype SA8 is Storage_Array(0..7);

      function U64_To_SA_Unchecked is
        new Ada.Unchecked_Conversion(Source => Unsigned_64,
                                     Target => SA8);
   begin
      return U64_To_SA_Unchecked(W);
   end Unsigned_64_To_Storage_Array;

end NORX_Load_Store;
