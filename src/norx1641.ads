-- NORX1641
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- This instantiation words on 16-bit words, with 4 rounds and a parallelism
-- degree of 1. Note that the Key_Position is 2 here, not 4 as it is in all
-- the other instantiations. This was a deliberate decision by the designers in
-- order to keep the lower half of the state initialised in a constant way, as
-- it may be slightly more exposed to differential attacks.

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

pragma SPARK_Mode (On);

with Interfaces;
with NORX;
with NORX_Load_Store;

pragma Elaborate_All(NORX);

use all type Interfaces.Unsigned_16;

package NORX1641 is new NORX(w                     => 16,
                             Word                  => Interfaces.Unsigned_16,
                             Storage_Array_To_Word => NORX_Load_Store.Storage_Array_To_Unsigned_16,
                             Word_To_Storage_Array => NORX_Load_Store.Unsigned_16_To_Storage_Array,
                             l                     => 4,
                             k                     => 96,
                             Key_Position          => 2,
                             t                     => 96,
                             n                     => 32,
                             rot                   => (8, 11, 12, 15),
                             r                     => 128,
                             c                     => 128);
