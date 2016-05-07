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
   with function Storage_Array_To_Word
     (S : in System.Storage_Elements.Storage_Array) return Word;
   with function Word_To_Storage_Array
     (W : in Word) return System.Storage_Elements.Storage_Array;
   with function Rotate_Right
     (Value  : Word;
      Amount : Natural) return Word is <>;
   with function Shift_Left
     (Value  : Word;
      Amount : Natural) return Word is <>;
   l : Round_Number; -- Round number 1 ≤ l ≤ 63
   t : Positive; -- Tag size t ≤ 4w
   rot : Rotation_Offsets;
   r : Positive; -- Rate
   c : Positive; --Capacity
package NORX is

   use System.Storage_Elements;

   subtype Key_Type is Storage_Array(0..Storage_Offset(w/2)-1);
   subtype Nonce_Type is Storage_Array(0..Storage_Offset(w/4)-1);
   subtype Tag_Type is Storage_Array(0..Storage_Offset(t/8)-1);

   -- High-level API for NORX

   procedure AEADEnc(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     M : in Storage_Array;
                     Z : in Storage_Array;
                     C : out Storage_Array;
                     T : out Tag_Type)
     with Pre => (C'Length = M'Length);

   procedure AEADDec(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     C : in Storage_Array;
                     Z : in Storage_Array;
                     T : in Tag_Type;
                     M : out Storage_Array;
                     Valid : out Boolean)
     with Pre => (M'Length = C'Length);

   -- This type declaration makes the NORX.Access_Internals package easier to
   -- write. It is not intended for normal use.
   type State(<>) is private;

private

   type State is array (Integer range 0..15) of Word;

   -- Low-level API for NORX. These routines can be accessed by instantiating
   -- the NORX.Access_Internals child package

   function Make_State return State is (State'(others => 0));

   function Get_Initialisation_Constants return State;

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type) return State;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word);

   procedure Encrypt (S : in out State;
                      M : in Storage_Array;
                      C : out Storage_Array;
                      v : in Word)
     with Pre => (C'Length = M'Length);

   procedure Decrypt (S : in out State;
                      C : in Storage_Array;
                      M : out Storage_Array;
                      v : in Word)
     with Pre => (C'Length = M'Length);

   procedure Finalise (S : in out State; Tag : out Tag_Type; v : in Word);

   -- These compile-time checks test requirements that cannot be expressed
   -- in the generic formal parameters. Currently compile-time checks are
   -- not supported in GNATprove so the related warnings are suppressed.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error (2**w /= Word'Modulus,
                              "The specified type Word must have the same " &
                                "'Modulus as the word width parameter w**2");
   pragma Compile_Time_Error (t > 4 * w,
                              "The specified tag size t must be less than or " &
                                "equal to 4*w, the word size");
   pragma Compile_Time_Error (t mod w /= 0,
                              "The specified tag size t is not a multiple of " &
                                "w, the word size");
   pragma Compile_Time_Error (r + c /= 16 * w,
                              "The total of the rate (r) and capacity (c) do " &
                                "not equal 16*w, the word size");
   pragma Compile_Time_Error (r mod w /= 0 or c mod w /= 0,
                              "The rate (r) and capacity (c) are not " &
                                "multiples of the word size");
   pragma Compile_Time_Error (System.Storage_Elements.Storage_Element'Size /= 8,
                              "This implementation of NORX cannot work " &
                                "with Storage_Element'Size /= 8");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end NORX;
