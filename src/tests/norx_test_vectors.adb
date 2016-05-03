-- NORX_Test_Vectors

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
with Interfaces;

with NORX6441;
with NORX.Access_Internals;

procedure NORX_Test_Vectors is

   package Unsigned_64_IO is new
     Ada.Text_IO.Modular_IO(Num => Interfaces.Unsigned_64);
   use Unsigned_64_IO;

   package NORX6441_Internals is new NORX6441.Access_Internals;

   Init_Constants : constant NORX6441_Internals.State
     := NORX6441_Internals.Get_Initialisation_Constants;

begin
   Put_Line("NORX Test Vectors");
   New_Line;

   Put_Line("NORX6441: 64-bit words, 4 rounds, no parallelisation");
   Put_Line("Check initialisation constants:");

   for I in Init_Constants'Range loop
      Put("u"); Put(I, Width => 2); Put(" : ");
      Put(Init_Constants(I), Base => 16);
      New_Line;
   end loop;
   New_Line;

end NORX_Test_Vectors;
