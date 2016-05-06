-- Display_NORX_Traces
-- A utility to display traces of the encryption process for the test vectors
-- suggested in Appendix A of the NORX specification

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;
with System.Storage_Elements;
use System.Storage_Elements;

with NORX.Access_Internals;
with NORX.Utils;

procedure Display_NORX_Traces is

   package NORX_Internals is new NORX_Package.Access_Internals;
   use NORX_Internals;

   package NORX_Utils is new NORX_Package.Utils;
   use NORX_Utils;

   Init_Constants : constant State := Get_Initialisation_Constants;

   State_Trace : State;

   K : NORX_Package.Key_Type;
   N : NORX_Package.Nonce_Type;
   A, M, Z : Storage_Array(0..127);
   C : Storage_Array(0..127);
   T : NORX_Package.Tag_Type;

   M2 : Storage_Array(0..127);
   T2 : NORX_Package.Tag_Type;

begin
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

   Put_Line("ENCRYPTION");
   New_Line;

   State_Trace := Initialise(K, N);
   Put_Line("State after initialisation (with key and nonce):");
   Put_State(State_Trace);
   New_Line;

   Absorb(State_Trace, A, 16#01#);
   Put_Line("State after header processing:");
   Put_State(State_Trace);
   New_Line;

   Encrypt(State_Trace, M, C, 16#02#);
   Put_Line("State after message encryption:");
   Put_State(State_Trace);
   New_Line;

   Absorb(State_Trace, Z, 16#04#);
   Put_Line("State after trailer processing:");
   Put_State(State_Trace);
   New_Line;

   Finalise(State_Trace, T, 16#08#);
   Put_Line("State after finalisation:");
   Put_State(State_Trace);
   New_Line;

   Put_Line("Ciphertext:");
   Put_Storage_Array(C);
   Put_Line("Tag:");
   Put_Storage_Array(T);
   New_Line;

   Put_Line("DECRYPTION");
   New_Line;

   State_Trace := Initialise(K, N);
   Put_Line("Initialise state with key and nonce");

   Absorb(State_Trace, A, 16#01#);
   Put_Line("Absorbing header into state");

   Decrypt(State_Trace, C, M2, 16#02#);
   Put_Line("Decrypting message");

   Absorb(State_Trace, Z, 16#04#);
   Put_Line("Absorbing trailer into state");

   Finalise(State_Trace, T2, 16#08#);
   Put_Line("Finalising state");
   New_Line;

   Put_Line("Recovered plaintext:");
   Put_Storage_Array(M2);
   Put_Line("Recovered Tag:");
   Put_Storage_Array(T2);
   New_Line;

   Put_Line((if M /= M2 then "Plaintexts don't match" else "Plaintexts match"));
   Put_Line((if T /= T2 then "Tags don't match" else "Tags match"));
   New_Line;

end Display_NORX_Traces;
