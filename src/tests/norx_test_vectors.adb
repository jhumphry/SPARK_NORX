-- NORX_Test_Vectors

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
use System.Storage_Elements;

with NORX6441;
with NORX.Access_Internals;

procedure NORX_Test_Vectors is

   package NORX6441_Internals is new NORX6441.Access_Internals;
   use NORX6441_Internals;

   Init_Constants : constant State := Get_Initialisation_Constants;

   State_Trace : State;

   K : Storage_Array(0..31);
   N : Storage_Array(0..15);
   A, M, Z : Storage_Array(0..127);

begin
   Put_Line("NORX Test Vectors");
   New_Line;

   Put_Line("NORX6441: 64-bit words, 4 rounds, no parallelisation");

   Put_Line("Initialising input data as per A.2 of the NORX specification");
   for I in K'Range loop
      K(I) := Storage_Element(I);
   end loop;

   for I in N'Range loop
      N(I) := (15 - Storage_Element(I)) * 16;
   end loop;

   for I in A'Range loop
      A(I) := Storage_Element(I);
      M(I) := Storage_Element(I);
      Z(I) := Storage_Element(I);
   end loop;

   New_Line;

   Put_Line("Check initialisation constants:");
   Put_State(Init_Constants);
   New_Line;

   Put_Line("Initialise state with key and nonce:");
   State_Trace := Initialise(K, N);
   Put_State(State_Trace);
   New_Line;

end NORX_Test_Vectors;
