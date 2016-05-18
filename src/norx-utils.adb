-- NORX.Utils
-- Some utility routines useful when writing the examples.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

package body NORX.Utils is

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

   procedure Put_Storage_Array(X : in Storage_Array) is

      function To_Hex_String(X : Storage_Element) return String is
         Hex : constant String(1..16) := "0123456789ABCDEF";
         begin
            return Hex(Integer(X/16) + 1) & Hex(Integer(X mod 16) + 1);
         end To_Hex_String;

   begin
      for I in X'Range loop
         Put(To_Hex_String(X(I)));
         if I mod 16 = 15 then
            New_Line;
         else
            Put(" ");
         end if;
      end loop;
   end Put_Storage_Array;

end NORX.Utils;
