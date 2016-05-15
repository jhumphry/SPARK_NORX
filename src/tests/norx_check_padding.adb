-- NORX_Check_Padding

-- Ensure that headers and trailers of different lengths are accepted
-- and messages of different lengths correctly decrypted (to check padding)

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

with Test_Input_Lengths;

with NORX0841;
with NORX1641;
with NORX3241;
with NORX3261;
with NORX6441;
with NORX6461;

procedure NORX_Check_Padding is

   procedure NORX0841_Test is new Test_Input_Lengths(Norx_Package => NORX0841);
   procedure NORX1641_Test is new Test_Input_Lengths(Norx_Package => NORX1641);
   procedure NORX3241_Test is new Test_Input_Lengths(Norx_Package => NORX3241);
   procedure NORX3261_Test is new Test_Input_Lengths(Norx_Package => NORX3261);
   procedure NORX6441_Test is new Test_Input_Lengths(Norx_Package => NORX6441);
   procedure NORX6461_Test is new Test_Input_Lengths(Norx_Package => NORX6461);

begin

   Put_Line("Checking padding and input-length flexibility for NORX routines");
   New_Line;

   Put("Header, message and trailer lengths from 0-2000 are checked for " &
         "correct authenticated encryption and decryption. While the length " &
         "of one input is being varied, the lengths of the others are held " &
         "constant.");
   New_Line;

   Put_Line("----------");
   Put_Line("NORX08-4-1");
   Put_Line("----------");
   NORX0841_Test;

   Put_Line("----------");
   Put_Line("NORX16-4-1");
   Put_Line("----------");
   NORX1641_Test;

   Put_Line("----------");
   Put_Line("NORX32-4-1");
   Put_Line("----------");
   NORX3241_Test;

   Put_Line("----------");
   Put_Line("NORX32-6-1");
   Put_Line("----------");
   NORX3261_Test;

   Put_Line("----------");
   Put_Line("NORX64-4-1");
   Put_Line("----------");
   NORX6441_Test;

   Put_Line("----------");
   Put_Line("NORX64-6-1");
   Put_Line("----------");
   NORX6461_Test;

end NORX_Check_Padding;
