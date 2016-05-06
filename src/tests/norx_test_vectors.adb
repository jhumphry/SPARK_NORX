-- NORX_Test_Vectors

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

with Display_NORX_Traces;

with NORX3241;
with NORX3261;
with NORX6441;
with NORX6461;

procedure NORX_Test_Vectors is

   procedure NORX3241_Display is
     new Display_NORX_Traces(NORX_Package => NORX3241);

   procedure NORX3261_Display is
     new Display_NORX_Traces(NORX_Package => NORX3261);

   procedure NORX6441_Display is
     new Display_NORX_Traces(NORX_Package => NORX6441);

   procedure NORX6461_Display is
     new Display_NORX_Traces(NORX_Package => NORX6461);

begin
   Put_Line("NORX Test Vectors");
   New_Line;

   Put_Line("----------");
   Put_Line("NORX32-4-1");
   Put_Line("----------");
   NORX3241_Display;

   Put_Line("----------");
   Put_Line("NORX32-6-1");
   Put_Line("----------");
   NORX3261_Display;

   Put_Line("----------");
   Put_Line("NORX64-4-1");
   Put_Line("----------");
   NORX6441_Display;

   Put_Line("----------");
   Put_Line("NORX64-6-1");
   Put_Line("----------");
   NORX6461_Display;

end NORX_Test_Vectors;
