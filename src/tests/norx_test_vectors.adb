-- NORX_Test_Vectors

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;
with Interfaces;

with NORX6441;
with NORX.Access_Internals;

procedure NORX_Test_Vectors is

   package NORX6441_Internals is new NORX6441.Access_Internals;
   use NORX6441_Internals;

   Init_Constants : constant State := Get_Initialisation_Constants;


begin
   Put_Line("NORX Test Vectors");
   New_Line;

   Put_Line("NORX6441: 64-bit words, 4 rounds, no parallelisation");

   Put_Line("Check initialisation constants:");
   Put_State(Init_Constants);
   New_Line;

   New_Line;

end NORX_Test_Vectors;
