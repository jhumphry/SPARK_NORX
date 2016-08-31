-- NORX_C_Definitions

-- Some definitions required by the C interface in NORX.C_Interface.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

with System.Storage_Elements;
with Interfaces.C.Pointers;

package NORX_C_Definitions
with SPARK_Mode => Off is

   use System.Storage_Elements;

   package Storage_Element_Pointers is new
     Interfaces.C.Pointers(Index              => Storage_Offset,
                           Element            => Storage_Element,
                           Element_Array      => Storage_Array,
                           Default_Terminator => 0);

end NORX_C_Definitions;
