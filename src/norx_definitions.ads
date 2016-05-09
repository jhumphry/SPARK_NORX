-- NORX_Definitions
-- Some type / subtype definitions in common use in the NORX code.
-- As some uses of these types are in generic parameters, it is not possible
-- to hide them.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

package NORX_Definitions
with Pure, SPARK_Mode => On is

   subtype Word_Size is Integer
     with Static_Predicate => Word_Size in 8 | 16 | 32 | 64;

   subtype Round_Number is Integer range 1..63;

   type Rotation_Offsets is array (Integer range 0..3) of Natural;

end NORX_Definitions;
