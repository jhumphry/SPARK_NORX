-- NORX.Access_Internals
-- Allow access to some internal parts of NORX for testing and verification
-- purposes. Not part of the standard API

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

package body NORX.Access_Internals is

   package Unsigned_64_IO is new
     Ada.Text_IO.Modular_IO(Num => Word);
   use Unsigned_64_IO;

   procedure Put_State(S : in State) is
   begin
      for I in S'Range loop
         Put(S(I), Base => 16);
         if I mod 4 = 3 then
            New_Line;
         else
            Put(" ");
         end if;
      end loop;
   end Put_State;

   function Get_Initialisation_Constants return State is
      (State(NORX.Get_Initialisation_Constants));

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type)
                        return State is
      (State(NORX.Initialise(Key, Nonce)));

end NORX.Access_Internals;
