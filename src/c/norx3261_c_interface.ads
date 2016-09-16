-- NORX6441_C_Interface

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- This instantiation words on 32-bit words, with 6 rounds and a parallelism
-- degree of 1

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma SPARK_Mode (Off);

with NORX3261;
with NORX.C_AEAD_Encrypt;
with NORX.C_AEAD_Decrypt;

package NORX3261_C_Interface is

   procedure NORX3261_AEAD_Encrypt is new NORX3261.C_AEAD_Encrypt
     with Export => True,
     Convention => C,
     External_Name => "norx3261_aead_encrypt";

   function NORX3261_AEAD_Decrypt is new NORX3261.C_AEAD_Decrypt
     with Export => True,
     Convention => C,
     External_Name => "norx3261_aead_decrypt";

end NORX3261_C_Interface;
