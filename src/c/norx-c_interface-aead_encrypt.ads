-- NORX.C_Interface.AEAD_Encrypt

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

-- Reference C API is as follows:

--     void norx_aead_encrypt(
--             unsigned char *c, size_t *clen,
--             const unsigned char *a, size_t alen,
--             const unsigned char *m, size_t mlen,
--             const unsigned char *z, size_t zlen,
--             const unsigned char *nonce, const unsigned char *key);

generic
procedure NORX.C_Interface.AEAD_Encrypt (c : in uchar_ptr; clen : access size_t;
                                         a : in uchar_ptr; alen : size_t;
                                         m : in uchar_ptr; mlen : size_t;
                                         z : in uchar_ptr; zlen : size_t;
                                         nonce : in uchar_ptr;
                                         key : in uchar_ptr);
