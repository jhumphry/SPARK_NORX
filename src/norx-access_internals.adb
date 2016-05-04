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

   package Storage_Text_IO is new
     Ada.Text_IO.Modular_IO(Num => Storage_Element);
   use Storage_Text_IO;

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

   procedure Put_Storage_Array(X : in Storage_Array) is
   begin
      for I in X'Range loop
         Put(X(I), Base => 16, Width => 7);
         if I mod 16 = 15 then
            New_Line;
         end if;
      end loop;
   end Put_Storage_Array;

end NORX.Access_Internals;
