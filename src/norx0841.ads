-- NORX0841
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- This instantiation words on 8-bit words, with 4 rounds and a parallelism
-- degree of 1

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with Interfaces;
with NORX;
with NORX_Load_Store;

pragma Elaborate_All(NORX);

use all type Interfaces.Unsigned_8;

package NORX0841 is new NORX(w                     => 8,
                             Word                  => Interfaces.Unsigned_8,
                             Storage_Array_To_Word => NORX_Load_Store.Storage_Array_To_Unsigned_8,
                             Word_To_Storage_Array => NORX_Load_Store.Unsigned_8_To_Storage_Array,
                             l                     => 4,
                             k                     => 80,
                             t                     => 80,
                             n                     => 32,
                             rot                   => (1, 3, 5, 7),
                             r                     => 40,
                             c                     => 88);
