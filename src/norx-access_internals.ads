-- NORX.Access_Internals
-- Allow access to some internal parts of NORX for testing and verification
-- purposes. Not part of the standard API

-- Copyright (c) 2016, James Humphry - see LICENSE file for details


generic
package NORX.Access_Internals is

   subtype State is NORX.State;

   procedure Put_State(S : in State);

   function Get_Initialisation_Constants return State;

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type) return State;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word);

private

   function Get_Initialisation_Constants return State
     renames NORX.Get_Initialisation_Constants;

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type) return State
                        renames NORX.Initialise;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word)
                     renames NORX.Absorb;

end NORX.Access_internals;
