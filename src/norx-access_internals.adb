-- NORX.Access_Internals
-- Allow access to some internal parts of NORX for testing and verification
-- purposes. Not part of the standard API

-- Copyright (c) 2016, James Humphry - see LICENSE file for details


package body NORX.Access_Internals is

   function Get_Initialisation_Constants return State is
      (State(NORX.Get_Initialisation_Constants));

end NORX.Access_Internals;
