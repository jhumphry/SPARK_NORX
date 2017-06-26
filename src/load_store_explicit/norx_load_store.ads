-- NORX_Load_Store
-- A collection of functions to load and store words of different sizes from
-- Storage_Array in Little Endian format. Currently these are not optimised
-- for the case where the machine itself is LE or has dedicated assembly
-- instructions that can perform the conversion.

-- Copyright (c) 2016-2017, James Humphry - see LICENSE file for details

-- Note that all the Unsigned_xx types count as Implementation_Identifiers
pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

with System.Storage_Elements;
use System.Storage_Elements;

with Interfaces;
use Interfaces;

package NORX_Load_Store
  with Pure, SPARK_Mode => On is

   subtype E is Storage_Element;
   subtype Storage_Array_Single is Storage_Array(1..1);

   function Check_Storage_Array_Length (X : in Storage_Array;
                                        L : in Positive) return Boolean
   is
      (
       if X'Last < X'First then
            False

         elsif X'First < 0 then
           (
                (Long_Long_Integer (X'Last) < Long_Long_Integer'Last +
                       Long_Long_Integer (X'First))
            and then
            X'Last - X'First = Storage_Offset(L) - 1)

         else
          X'Last - X'First = Storage_Offset(L) - 1
      )
   with Ghost;

   function Storage_Array_To_Unsigned_8 (S : in Storage_Array)
                                         return Unsigned_8 is
     (Unsigned_8(S(S'First)))
   with Inline, Pre => (Check_Storage_Array_Length(S, 1));

   function Unsigned_8_To_Storage_Array (W : in Unsigned_8)
                                         return Storage_Array is
     (Storage_Array_Single'(Storage_Array_Single'First => E(W)))
   with Inline, Post => (Unsigned_8_To_Storage_Array'Result'Length = 1);

   function Storage_Array_To_Unsigned_16 (S : in Storage_Array)
                                          return Unsigned_16 is
     (Unsigned_16(S(S'First)) or
        Shift_Left(Unsigned_16(S(S'First + 1)), 8))
   with Inline, Pre => (Check_Storage_Array_Length(S, 2));

   function Unsigned_16_To_Storage_Array (W : in Unsigned_16)
                                          return Storage_Array is
     (Storage_Array'(E(W mod 16#100#),
                     E(Shift_Right(W, 8) mod 16#100#)))
   with Inline, Post => (Unsigned_16_To_Storage_Array'Result'Length = 2);

   function Storage_Array_To_Unsigned_32 (S : in Storage_Array)
                                          return Unsigned_32 is
     (Unsigned_32(S(S'First)) or
        Shift_Left(Unsigned_32(S(S'First + 1)), 8) or
          Shift_Left(Unsigned_32(S(S'First + 2)), 16) or
          Shift_Left(Unsigned_32(S(S'First + 3)), 24))
   with Inline, Pre => (Check_Storage_Array_Length(S, 4));

   function Unsigned_32_To_Storage_Array (W : in Unsigned_32)
                                          return Storage_Array is
     (Storage_Array'(E(W mod 16#100#),
                     E(Shift_Right(W, 8) mod 16#100#),
                     E(Shift_Right(W, 16) mod 16#100#),
                     E(Shift_Right(W, 24) mod 16#100#)))
   with Inline, Post => (Unsigned_32_To_Storage_Array'Result'Length = 4);

   function Storage_Array_To_Unsigned_64 (S : in Storage_Array)
                                          return Unsigned_64 is
     (Unsigned_64(S(S'First)) or
        Shift_Left(Unsigned_64(S(S'First + 1)), 8) or
          Shift_Left(Unsigned_64(S(S'First + 2)), 16) or
          Shift_Left(Unsigned_64(S(S'First + 3)), 24) or
          Shift_Left(Unsigned_64(S(S'First + 4)), 32) or
          Shift_Left(Unsigned_64(S(S'First + 5)), 40) or
          Shift_Left(Unsigned_64(S(S'First + 6)), 48) or
          Shift_Left(Unsigned_64(S(S'First + 7)), 56))
   with Inline, Pre => (Check_Storage_Array_Length(S, 8));

   function Unsigned_64_To_Storage_Array (W : in Unsigned_64)
                                          return Storage_Array is
     (Storage_Array'(E(W mod 16#100#),
                     E(Shift_Right(W, 8) mod 16#100#),
                     E(Shift_Right(W, 16) mod 16#100#),
                     E(Shift_Right(W, 24) mod 16#100#),
                     E(Shift_Right(W, 32) mod 16#100#),
                     E(Shift_Right(W, 40) mod 16#100#),
                     E(Shift_Right(W, 48) mod 16#100#),
                     E(Shift_Right(W, 56) mod 16#100#)))
     with Inline, Post => (Unsigned_64_To_Storage_Array'Result'Length = 8);

end NORX_Load_Store;
