-- NORX_C_Definitions

-- Some definitions required by the C interface in NORX.C_Interface.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

with System.Storage_Elements;
with Interfaces.C;
with Interfaces.C.Pointers;

package NORX_C_Definitions
with SPARK_Mode => Off is

   use System.Storage_Elements;

   subtype size_t is Interfaces.C.size_t;
   subtype ptrdiff_t is Interfaces.C.ptrdiff_t;

   package Storage_Element_Pointers is new
     Interfaces.C.Pointers(Index              => Storage_Offset,
                           Element            => Storage_Element,
                           Element_Array      => Storage_Array,
                           Default_Terminator => 0);

   subtype uchar_ptr is NORX_C_Definitions.Storage_Element_Pointers.Pointer;

   -- These compile-time checks test requirements that cannot be expressed
   -- in the generic formal parameters. Currently compile-time checks are
   -- not supported in GNATprove so the related warnings are suppressed.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error (System.Storage_Elements.Storage_Element'Size /=
                                Interfaces.C.unsigned_char'Size,
                              "Interfacing between C and the Ada library " &
                                "work if Ada Storage_Element are different " &
                                "from C unsigned chars.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end NORX_C_Definitions;
