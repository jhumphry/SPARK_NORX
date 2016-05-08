-- NORX6441_Static
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- This is a "static" version of NORX6641.ads for use with SPARK as using
-- the SPARK tools with generic units is not always very convenient.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with System.Storage_Elements;
use type System.Storage_Elements.Storage_Offset;

with Interfaces;

with NORX_Load_Store;

with NORX_Definitions;
use NORX_Definitions;

package NORX6441_Static
with SPARK_Mode => On
 is

   w : constant Word_Size := 64;

   subtype Word is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;

   function Storage_Array_To_Word (S : in System.Storage_Elements.Storage_Array)
                                   return Word
                                   renames NORX_Load_Store.Storage_Array_To_Unsigned_64;

   function Word_To_Storage_Array (W : in Word)
                                   return System.Storage_Elements.Storage_Array
                                   renames NORX_Load_Store.Unsigned_64_To_Storage_Array;

   function Rotate_Right (Value  : Word;
                          Amount : Natural) return Word
                          renames Interfaces.Rotate_Right;

   function Shift_Left (Value  : Word;
                        Amount : Natural) return Word
                        renames Interfaces.Shift_Left;

   l : constant Round_Number := 4; -- Round number 1 ≤ l ≤ 63
   k : constant Positive := 256; -- Key size
   t : constant Positive := 256; -- Tag size (t ≤ 4w for NORX32 and NORX64)
   n : constant Positive := 128; -- Nonce size
   rot : constant Rotation_Offsets := (8, 19, 40, 63);
   r : constant Positive := 768; -- Rate
   c : constant Positive := 256; --Capacity

   use System.Storage_Elements;

   subtype Key_Type is Storage_Array(0..Storage_Offset(k/8)-1);
   subtype Nonce_Type is Storage_Array(0..Storage_Offset(n/8)-1);
   subtype Tag_Type is Storage_Array(0..Storage_Offset(t/8)-1);

   -- High-level API for NORX

   procedure AEADEnc(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     M : in Storage_Array;
                     Z : in Storage_Array;
                     C : out Storage_Array;
                     T : out Tag_Type)
     with Pre => (C'Length = M'Length and
                    A'Length < Storage_Offset'Last and
                      A'Last < Storage_Offset'Last - Storage_Offset(r/8) and
                      Z'Length < Storage_Offset'Last and
                        Z'Last < Storage_Offset'Last - Storage_Offset(r/8));

   procedure AEADDec(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     C : in Storage_Array;
                     Z : in Storage_Array;
                     T : in Tag_Type;
                     M : out Storage_Array;
                     Valid : out Boolean)
     with Pre => (M'Length = C'Length and
                    A'Length < Storage_Offset'Last and
                      A'Last < Storage_Offset'Last - Storage_Offset(r/8) and
                      Z'Length < Storage_Offset'Last and
                        Z'Last < Storage_Offset'Last - Storage_Offset(r/8));

   -- This type declaration makes the NORX.Access_Internals package easier to
   -- write. It is not intended for normal use.
   type State(<>) is private;

private

   type State is array (Integer range 0..15) of Word;

   -- Low-level API for NORX. These routines can be accessed by instantiating
   -- the NORX.Access_Internals child package

   function Make_State return State;

   function Get_Initialisation_Constants return State;

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type) return State;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word)
     with Pre=> (X'Length < Storage_Offset'Last and
                   X'Last < Storage_Offset'Last - Storage_Offset(r/8));

   procedure Encrypt (S : in out State;
                      M : in Storage_Array;
                      C : out Storage_Array;
                      v : in Word)
     with Pre => (C'Length = M'Length and
                    M'Length < Storage_Offset'Last and
                      M'Last < Storage_Offset'Last - Storage_Offset(r/8) and
                      C'Last < Storage_Offset'Last - Storage_Offset(r/8));

   procedure Decrypt (S : in out State;
                      C : in Storage_Array;
                      M : out Storage_Array;
                      v : in Word)
     with Pre => (C'Length = M'Length and
                    C'Length < Storage_Offset'Last and
                      C'Last < Storage_Offset'Last - Storage_Offset(r/8) and
                      M'Last < Storage_Offset'Last - Storage_Offset(r/8));

   procedure Finalise (S : in out State; Tag : out Tag_Type; v : in Word);

   -- These compile-time checks test requirements that cannot be expressed
   -- in the generic formal parameters. Currently compile-time checks are
   -- not supported in GNATprove so the related warnings are suppressed.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error (2**w /= Word'Modulus,
                              "The specified type Word must have the same " &
                                "'Modulus as the word width parameter w**2");
   pragma Compile_Time_Error (k mod w /= 0,
                              "The specified key size k is not a multiple of " &
                                "w, the word size");
   pragma Compile_Time_Error (t mod w /= 0,
                              "The specified tag size t is not a multiple of " &
                                "w, the word size");
   pragma Compile_Time_Error (t > 2*r,
                              "The specified tag size t is more than 2*r, " &
                                "the rate");
   pragma Compile_Time_Error (n mod w /= 0,
                              "The specified nonce size n is not a multiple " &
                                "of w, the word size");
   pragma Compile_Time_Error (n > 4*w,
                              "The specified nonce size n is greater than " &
                                "4*w, the word size");
   pragma Compile_Time_Error (r + c /= 16 * w,
                              "The total of the rate (r) and capacity (c) do " &
                                "not equal 16*w, the word size");
   pragma Compile_Time_Error (r mod w /= 0 or c mod w /= 0,
                              "The rate r and capacity c are not " &
                                "multiples of the word size");
   pragma Compile_Time_Error (System.Storage_Elements.Storage_Element'Size /= 8,
                              "This implementation of NORX cannot work " &
                                "with Storage_Element'Size /= 8");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end NORX6441_Static;
