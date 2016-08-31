-- NORX.C_AEAD_Decrypt

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma SPARK_Mode (Off);

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

-- Reference C API is as follows:

--     int norx_aead_decrypt(
--             unsigned char *m, size_t *mlen,
--             const unsigned char *a, size_t alen,
--             const unsigned char *c, size_t clen,
--             const unsigned char *z, size_t zlen,
--             const unsigned char *nonce, const unsigned char *key);

-- Returns 0 on success, -1 on failure

with NORX_C_Definitions;
use NORX_C_Definitions;

generic
function NORX.C_AEAD_Decrypt (m : in uchar_ptr; mlen : access size_t;
                              a : in uchar_ptr; alen : size_t;
                              c : in uchar_ptr; clen : size_t;
                              z : in uchar_ptr; zlen : size_t;
                              nonce : in uchar_ptr;
                              key : in uchar_ptr) return int;
