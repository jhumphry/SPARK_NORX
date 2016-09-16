-- NORX6441_C_Interface

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- This instantiation words on 64-bit words, with 6 rounds and a parallelism
-- degree of 1

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma SPARK_Mode (Off);

with NORX6461;
with NORX.C_AEAD_Encrypt;
with NORX.C_AEAD_Decrypt;

package NORX6461_C_Interface is

   procedure NORX6461_AEAD_Encrypt is new NORX6461.C_AEAD_Encrypt
     with Export => True,
     Convention => C,
     External_Name => "norx6461_aead_encrypt";

   function NORX6461_AEAD_Decrypt is new NORX6461.C_AEAD_Decrypt
     with Export => True,
     Convention => C,
     External_Name => "norx6461_aead_decrypt";

end NORX6461_C_Interface;
