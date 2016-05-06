-- NORX32_Test_Vectors

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;
with System.Storage_Elements;
use System.Storage_Elements;

with NORX3241;
with NORX.Access_Internals;
with NORX.Utils;

procedure NORX32_Test_Vectors is

   package NORX3241_Internals is new NORX3241.Access_Internals;
   use NORX3241_Internals;

   package NORX3241_Utils is new NORX3241.Utils;
   use NORX3241_Utils;

   Init_Constants : constant State := Get_Initialisation_Constants;

   State_Trace : State;

   K : Storage_Array(0..15);
   N : Storage_Array(0..7);
   A, M, Z : Storage_Array(0..127);
   C : Storage_Array(0..127);
   T : NORX3241.Tag_Type;

begin
   Put_Line("NORX 32-bit Test Vectors");
   New_Line;

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

   Put_Line("Plaintext:");
   Put_Storage_Array(M);
   New_Line;

   Put_Line("NORX3241: 32-bit words, 4 rounds, no parallelisation");

   Put_Line("Check initialisation constants:");
   Put_State(Init_Constants);
   New_Line;

   State_Trace := Initialise(K, N);
   Put_Line("Initialise state with key and nonce:");
   Put_State(State_Trace);
   New_Line;

   Absorb(State_Trace, A, 16#01#);
   Put_Line("Absorb header into state:");
   Put_State(State_Trace);
   New_Line;

   Encrypt(State_Trace, M, C, 16#02#);
   Put_Line("Encrypt test message. State:");
   Put_State(State_Trace);
   New_Line;

   Absorb(State_Trace, Z, 16#04#);
   Put_Line("Absorb trailer into state:");
   Put_State(State_Trace);
   New_Line;

   Finalise(State_Trace, T, 16#08#);
   Put_Line("Finalise state:");
   Put_State(State_Trace);
   New_Line;

   Put_Line("Ciphertext:");
   Put_Storage_Array(C);
   Put_Line("Tag:");
   Put_Storage_Array(T);
   New_Line;

end NORX32_Test_Vectors;
