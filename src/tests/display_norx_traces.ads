-- Display_NORX_Traces
-- A utility to display traces of the encryption process for the test vectors
-- suggested in Appendix A of the NORX specification

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

with NORX;

generic
   with package NORX_Package is new NORX(<>);
procedure Display_NORX_Traces;
