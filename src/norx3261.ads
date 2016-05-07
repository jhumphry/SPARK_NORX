-- NORX3261
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- This instantiation words on 32-bit words, with 6 rounds and a parallelism
-- degree of 1

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Interfaces;
with NORX;
with NORX_Load_Store;

pragma Elaborate_All(NORX);

use all type Interfaces.Unsigned_32;

package NORX3261 is new NORX(w                     => 32,
                             Word                  => Interfaces.Unsigned_32,
                             Storage_Array_To_Word => NORX_Load_Store.Storage_Array_To_Unsigned_32,
                             Word_To_Storage_Array => NORX_Load_Store.Unsigned_32_To_Storage_Array,
                             l                     => 6,
                             k                     => 128,
                             t                     => 128,
                             n                     => 64,
                             rot                   => (8, 11, 16, 31),
                             r                     => 384,
                             c                     => 128);
