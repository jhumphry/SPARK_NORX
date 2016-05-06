-- NORX.Access_Internals
-- Allow access to some internal parts of NORX for testing and verification
-- purposes. Not part of the standard API

-- Copyright (c) 2016, James Humphry - see LICENSE file for details


generic
package NORX.Access_Internals is

   subtype State is NORX.State;

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

private

   function Get_Initialisation_Constants return State
     renames NORX.Get_Initialisation_Constants;

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type) return State
                        renames NORX.Initialise;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word)
                     renames NORX.Absorb;

   procedure Encrypt (S : in out State;
                      M : in Storage_Array;
                      C : out Storage_Array;
                      v : in Word) renames NORX.Encrypt;

   procedure Decrypt (S : in out State;
                      C : in Storage_Array;
                      M : out Storage_Array;
                      v : in Word) renames NORX.Decrypt;

   procedure Finalise (S : in out State; Tag : out Tag_Type; v : in Word)
     renames NORX.Finalise;

end NORX.Access_internals;
