-- NORX.Compare_Tags

-- Compare two Tag_Type values using a constant-time approach. This is split out
-- into a separate child unit from NORX so that different compilation flags can
-- be used. It is advisable to prevent optimisation of this function or else
-- a clever compiler might use a short-circuit approach. That might produce
-- logically identical results but it would leak information to attackers if
-- the timing of the decoding program can be observed.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

function NORX.Compare_Tags (L, R : Tag_Type) return Boolean is
   Result : Storage_Element := 0;
begin
   for I in L'Range loop
      Result := Result or (L(I) xor (R(I)));
   end loop;
   return (Result = 0);
end NORX.Compare_Tags;
