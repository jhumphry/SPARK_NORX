-- NORX
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with System.Storage_Elements;
use type System.Storage_Elements.Storage_Offset;

with NORX_Definitions;
use NORX_Definitions;

generic
   w : Word_Size; -- Word size w ∈ { 32, 64 }
   type Word is mod <>; -- Word'Modulus must equal w**2
--     with function Storage_Array_To_Word
--       (S : in System.Storage_Elements.Storage_Array) return Word;
   with function Rotate_Right
     (Value  : Word;
      Amount : Natural) return Word is <>;
   with function Shift_Left
     (Value  : Word;
      Amount : Natural) return Word is <>;
   l : Round_Number; -- Round number 1 ≤ l ≤ 63
   t : Positive; -- Tag size t ≤ 4w
   r : Rotation_Offsets;
package NORX is

private

   -- These compile-time checks test requirements that cannot be expressed
   -- in the generic formal parameters. Currently compile-time checks are
   -- not supported in GNATprove so the related warnings are suppressed.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error (2**w /= Word'Modulus,
                              "The specified type Word must have the same " &
                                "'Modulus as the word width parameter w**2");
   pragma Compile_Time_Error (t > 4*w,
                              "The specified tag size t must be less than or " &
                                "equal to 4*w, the word size");
   pragma Compile_Time_Error (System.Storage_Elements.Storage_Element'Size /= 8,
                              "This implementation of NORX cannot work " &
                                "with Storage_Element'Size /= 8.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

   type State is array (Integer range 0..15) of Word;

   function Get_Initialisation_Constants return State;

end NORX;
