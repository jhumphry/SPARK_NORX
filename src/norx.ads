-- NORX
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

with System.Storage_Elements;
use type System.Storage_Elements.Storage_Offset;

with NORX_Definitions;
use NORX_Definitions;

generic
   w : Word_Size; -- Word size w ∈ { 8, 16, 32, 64 } bits
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
   l : Round_Count; -- Rounds of the permutation to perform 1 ≤ l ≤ 63
   k : Positive; -- Key size in bits
   Key_Position : Key_Material_Position := 4; -- Position in the NORX state at
                                              -- which the key is added
   t : Positive; -- Tag size in bits (t ≤ 4w for NORX32 and NORX64)
   n : Positive; -- Nonce size in bits
   rot : Rotation_Offsets; -- Rotation offsets for permutation function
   r : Positive; -- Rate in bits
   c : Positive; -- Capacity in bits
package NORX is

   use System.Storage_Elements;

   subtype Key_Type is Storage_Array(0..Storage_Offset(k/8)-1);
   -- A Storage_Array subtype containing key material.

   subtype Nonce_Type is Storage_Array(0..Storage_Offset(n/8)-1);
   -- A Storage_Array subtype containing the nonce material. This must be unique
   -- per-message.

   subtype Tag_Type is Storage_Array(0..Storage_Offset(t/8)-1);
   -- A Storage_Array subtype containing an authentication tag.

   Null_Storage_Array : constant Storage_Array(1..0) := (others => 0);
   -- A null Storage_Array that can be passed to AEADEnc and AEADDec if one
   -- of the header, message or trailer parameters is not required.

   -- High-level API for NORX

   procedure AEADEnc(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     M : in Storage_Array;
                     Z : in Storage_Array;
                     C : out Storage_Array;
                     T : out Tag_Type)
     with Pre => (C'Length = M'Length and
                    Valid_Storage_Array_Parameter(A'Length, A'Last) and
                      Valid_Storage_Array_Parameter(M'Length, M'Last) and
                      Valid_Storage_Array_Parameter(Z'Length, Z'Last) and
                      Valid_Storage_Array_Parameter(C'Length, C'Last));
   -- AEADEnc carries out an authenticated encryption
   -- K : key data
   -- N : nonce
   -- A : optional (unencrypted) header
   -- M : optional message to be encrypted
   -- Z : optional (unencrypted) trailer
   -- C : encrypted version of M
   -- T : authentication tag for (A,M,Z)

   procedure AEADDec(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     C : in Storage_Array;
                     Z : in Storage_Array;
                     T : in Tag_Type;
                     M : out Storage_Array;
                     Valid : out Boolean)
     with Pre => (M'Length = C'Length and
                    Valid_Storage_Array_Parameter(A'Length, A'Last) and
                      Valid_Storage_Array_Parameter(C'Length, C'Last) and
                      Valid_Storage_Array_Parameter(Z'Length, Z'Last) and
                      Valid_Storage_Array_Parameter(M'Length, M'Last)),
     Post => (Valid or (for all I in M'Range => M(I) = 0));
   -- AEADEnc carries out an authenticated decryption
   -- K : key data
   -- N : nonce
   -- A : optional (unencrypted) header
   -- C : optional ciphertext to be decrypted
   -- Z : optional (unencrypted) trailer
   -- T : authentication tag
   -- M : contains the decrypted C or zero if the input does not authenticate
   -- Valid : indicates if the input authenticates correctly

   type State(<>) is private;
   -- This type declaration makes the NORX.Access_Internals package easier to
   -- write. It is not intended for normal use.

   function Valid_Storage_Array_Parameter(Length : in Storage_Offset;
                                          Last : in Storage_Offset)
                                          return Boolean;
   -- This function simplifies the preconditions

private

   function Valid_Storage_Array_Parameter(Length : in Storage_Offset;
                                          Last : in Storage_Offset)
                                          return Boolean is
      (Length < Storage_Offset'Last and
         Last < Storage_Offset'Last - Storage_Offset(r/8));

   type State is array (Integer range 0..15) of Word;

   -- Low-level API for NORX. These routines can be accessed by instantiating
   -- the NORX.Access_Internals child package

   function Make_State return State;

   function Get_Initialisation_Constants return State;

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type) return State;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word)
     with Pre=> (Valid_Storage_Array_Parameter(X'Length, X'Last));

   procedure Encrypt (S : in out State;
                      M : in Storage_Array;
                      C : out Storage_Array;
                      v : in Word)
     with Pre => (C'Length = M'Length and
                    Valid_Storage_Array_Parameter(M'Length, M'Last) and
                      Valid_Storage_Array_Parameter(C'Length, C'Last));

   procedure Decrypt (S : in out State;
                      C : in Storage_Array;
                      M : out Storage_Array;
                      v : in Word)
     with Pre => (C'Length = M'Length and
                    Valid_Storage_Array_Parameter(C'Length, C'Last) and
                      Valid_Storage_Array_Parameter(M'Length, M'Last));

   procedure Finalise (S : in out State;
                       Key : in Key_Type;
                       Tag : out Tag_Type;
                       v : in Word);

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
   pragma Compile_Time_Error (k > 12*w,
                              "The specified nonce size n is greater than " &
                                "12*w, the word size");
   pragma Compile_Time_Error (t mod w /= 0,
                              "The specified tag size t is not a multiple of " &
                                "w, the word size");
   pragma Compile_Time_Error (t > 16*w,
                              "The specified tag size t is greater than " &
                                "16*w, the word size");
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

end NORX;
