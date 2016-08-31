-- NORX.C_AEAD_Encrypt

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

function NORX.C_AEAD_Decrypt (m : in uchar_ptr; mlen : access size_t;
                              a : in uchar_ptr; alen : size_t;
                              c : in uchar_ptr; clen : size_t;
                              z : in uchar_ptr; zlen : size_t;
                              nonce : in uchar_ptr;
                              key : in uchar_ptr) return int is

   use type int;
   use type size_t;

   tag_bytes : constant size_t := size_t(t/8);

   use NORX_C_Definitions.Storage_Element_Pointers;

   -- The NORX C API uses size_t rather than ptrdiff_t so some annoying
   -- conversions are required.
   A_SA : constant Storage_Array := Value (Ref => a, Length => ptrdiff_t(alen));
   C_SA : constant Storage_Array :=
     Value (Ref => c, Length => ptrdiff_t(clen-tag_bytes));
   Tag_SA : constant Tag_Type :=
     Value (Ref => c + ptrdiff_t(clen - tag_bytes),
            Length => ptrdiff_t(tag_bytes));
   Z_SA : constant Storage_Array := Value (Ref => z, Length => ptrdiff_t(zlen));
   Nonce_SA : constant Nonce_Type :=
     Value (Ref => nonce, Length => ptrdiff_t(n/8));
   Key_SA : constant Key_Type := Value (Ref => key, Length => ptrdiff_t(k/8));
   M_SA : aliased Storage_Array(C_SA'Range);
   Valid : Boolean;
begin
   AEADDec(K     => Key_SA,
           N     => Nonce_SA,
           A     => A_SA,
           C     => C_SA,
           Z     => Z_SA,
           T     => Tag_SA,
           M     => M_SA,
           Valid => Valid);

   Copy_Array(Source => M_SA(M_SA'First)'Unchecked_Access,
              Target => m,
              Length => ptrdiff_t(clen - tag_bytes));

   mlen.all := clen - size_t(t/8);

   if Valid then
      return 0;
   else
      return -1;
   end if;
end;
