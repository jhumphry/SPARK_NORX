-- NORX6441_C_Interface

-- a C API for an Ada implementation of the NORX Authenticated Encryption
-- Algorithm created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel
-- Neves

-- This instantiation words on 64-bit words, with 4 rounds and a parallelism
-- degree of 1

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma SPARK_Mode (Off);

with NORX6441;
with NORX.C_Interface;
with NORX.C_Interface.AEAD_Encrypt;

package NORX6441_C_Interface is

   package NORX6641_C is new NORX6441.C_Interface;

   procedure NORX6441_AEAD_Encrypt is new NORX6641_C.AEAD_Encrypt
     with Export => True,
     Convention => C,
     External_Name => "norx6441_aead_encrypt";

end NORX6441_C_Interface;
