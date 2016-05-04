-- NORX_Load_Store
-- A collection of functions to load and store words of different sizes from
-- Storage_Array in Little Endian format. Currently these are not optimised
-- for the case where the machine itself is LE or has dedicated assembly
-- instructions that can perform the conversion.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with System.Storage_Elements;
use System.Storage_Elements;

with Interfaces;
use Interfaces;

package NORX_Load_Store is
   pragma Pure;

   subtype E is Storage_Element;

   function Storage_Array_To_Unsigned_32 (S : in Storage_Array)
                                          return Unsigned_32 is
     (Unsigned_32(S(S'First)) or
        Shift_Left(Unsigned_32(S(S'First + 1)), 8) or
          Shift_Left(Unsigned_32(S(S'First + 2)), 16) or
          Shift_Left(Unsigned_32(S(S'First + 3)), 24))
   with Inline, Pre => (S'Length = 4);

   function Unsigned_32_To_Storage_Array (W : in Unsigned_32)
                                          return Storage_Array is
     (Storage_Array'(E(W mod 16#100#),
                     E(Shift_Right(W, 8) mod 16#100#),
                     E(Shift_Right(W, 16) mod 16#100#),
                     E(Shift_Right(W, 24) mod 16#100#)))
     with Inline;

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
   with Inline, Pre => (S'Length = 8);

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
     with Inline;

end NORX_Load_Store;
