-- NORX_Test_Vectors

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

with Display_NORX_Traces;

with NORX6441;
with NORX3241;

procedure NORX_Test_Vectors is

   procedure NORX3241_Display is
     new Display_NORX_Traces(NORX_Package => NORX3241);

   procedure NORX6441_Display is
     new Display_NORX_Traces(NORX_Package => NORX6441);

begin
   Put_Line("NORX Test Vectors");
   New_Line;

   Put_Line("NORX32-4-1");
   NORX3241_Display;

   Put_Line("NORX64-4-1");
   NORX6441_Display;

end NORX_Test_Vectors;
