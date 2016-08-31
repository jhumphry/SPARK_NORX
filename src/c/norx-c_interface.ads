-- NORX.C_Interface

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

with Interfaces.C;

with NORX_C_Definitions;

generic
package NORX.C_Interface
with SPARK_Mode => Off
is
   subtype uchar_ptr is NORX_C_Definitions.Storage_Element_Pointers.Pointer;
   subtype size_t is Interfaces.C.size_t;

--     int norx_aead_decrypt(
--             unsigned char *m, size_t *mlen,
--             const unsigned char *a, size_t alen,
--             const unsigned char *c, size_t clen,
--             const unsigned char *z, size_t zlen,
--             const unsigned char *nonce, const unsigned char *key);

private

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

end NORX.C_Interface;
