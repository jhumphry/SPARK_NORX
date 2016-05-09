-- NORX6441
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- This instantiation words on 64-bit words, with 4 rounds and a parallelism
-- degree of 1

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma SPARK_Mode (On);

with Interfaces;
with NORX;
with NORX_Load_Store;

pragma Elaborate_All(NORX);

use all type Interfaces.Unsigned_64;

package NORX6441 is new NORX(w                     => 64,
                             Word                  => Interfaces.Unsigned_64,
                             Storage_Array_To_Word => NORX_Load_Store.Storage_Array_To_Unsigned_64,
                             Word_To_Storage_Array => NORX_Load_Store.Unsigned_64_To_Storage_Array,
                             l                     => 4,
                             k                     => 256,
                             t                     => 256,
                             n                     => 128,
                             rot                   => (8, 19, 40, 63),
                             r                     => 768,
                             c                     => 256);
