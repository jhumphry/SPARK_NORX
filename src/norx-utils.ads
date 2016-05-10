-- NORX.Utils
-- Some utility routines useful when writing the examples.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

generic
package NORX.Utils is

   procedure Put_State(S : in NORX.State);
   -- Print out a hexadecimal representation of the state to the console

   procedure Put_Storage_Array(X : in Storage_Array);
   -- Print out a hexadecimal representation of the storage array to the console

end NORX.Utils;
