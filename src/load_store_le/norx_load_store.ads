-- NORX_Load_Store
-- A collection of functions to load and store words of different sizes from
-- Storage_Array in Little Endian format. This version assumes that the machine
-- is Little Endian and does unchecked conversions to convert the types.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

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

   function Storage_Array_To_Unsigned_8 (S : in Storage_Array)
                                         return Unsigned_8
   with Inline, Pre => (S'Length = 1);

   function Unsigned_8_To_Storage_Array (W : in Unsigned_8)
                                         return Storage_Array
   with Inline, Post => (Unsigned_8_To_Storage_Array'Result'Length = 1);

   function Storage_Array_To_Unsigned_16 (S : in Storage_Array)
                                          return Unsigned_16
   with Inline, Pre => (S'Length = 2);

   function Unsigned_16_To_Storage_Array (W : in Unsigned_16)
                                          return Storage_Array
   with Inline, Post => (Unsigned_16_To_Storage_Array'Result'Length = 2);

   function Storage_Array_To_Unsigned_32 (S : in Storage_Array)
                                          return Unsigned_32
   with Inline, Pre => (S'Length = 4);

   function Unsigned_32_To_Storage_Array (W : in Unsigned_32)
                                          return Storage_Array
   with Inline, Post => (Unsigned_32_To_Storage_Array'Result'Length = 4);

   function Storage_Array_To_Unsigned_64 (S : in Storage_Array)
                                          return Unsigned_64
   with Inline, Pre => (S'Length = 8);

   function Unsigned_64_To_Storage_Array (W : in Unsigned_64)
                                          return Storage_Array
     with Inline, Post => (Unsigned_64_To_Storage_Array'Result'Length = 8);

end NORX_Load_Store;
