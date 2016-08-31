-- NORX.C_AEAD_Encrypt

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

procedure NORX.C_AEAD_Encrypt (c : in uchar_ptr; clen : access size_t;
                               a : in uchar_ptr; alen : size_t;
                               m : in uchar_ptr; mlen : size_t;
                               z : in uchar_ptr; zlen : size_t;
                               nonce : in uchar_ptr;
                               key : in uchar_ptr) is

   use type size_t;

   use NORX_C_Definitions.Storage_Element_Pointers;

   -- The NORX C API uses size_t rather than ptrdiff_t so some annoying
   -- conversions are required.
   A_SA : constant Storage_Array := Value (Ref => a, Length => ptrdiff_t(alen));
   M_SA : constant Storage_Array := Value (Ref => m, Length => ptrdiff_t(mlen));
   Z_SA : constant Storage_Array := Value (Ref => z, Length => ptrdiff_t(zlen));
   Nonce_SA : constant Nonce_Type :=
     Value (Ref => nonce, Length => ptrdiff_t(n/8));
   Key_SA : constant Key_Type := Value (Ref => key, Length => ptrdiff_t(k/8));
   Tag_SA : Tag_Type;
   C_SA : aliased Storage_Array(M_SA'Range);
begin
   AEADEnc(K => Key_SA,
           N => Nonce_SA,
           A => A_SA,
           M => M_SA,
           Z => Z_SA,
           C => C_SA,
           T => Tag_SA);
   Copy_Array(Source => C_SA(C_SA'First)'Unchecked_Access,
              Target => c,
              Length => ptrdiff_t(mlen));
   Copy_Array(Source => Tag_SA(Tag_SA'First)'Unchecked_Access,
              Target => c + ptrdiff_t(mlen),
              Length => ptrdiff_t(t/8));

   clen.all := mlen + size_t(t/8);
end;
