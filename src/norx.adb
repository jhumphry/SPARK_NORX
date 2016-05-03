-- NORX
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

package body NORX is

   u : State; -- The initialisation constants

   function Get_Initialisation_Constants return State is (u);

   -- Quarter-round
   procedure G (a, b, c, d : in out Word)
     with Inline is

      -- The nonlinear primitive
      function H (x, y : in Word) return Word is
         ((x xor y) xor (Shift_Left((x and y), 1))) with Inline;

   begin
      a := H(a, b);
      d := Rotate_Right(a xor d, r(0));
      c := H(c, d);
      b := Rotate_Right(b xor c, r(1));
      a := H(a, b);
      d := Rotate_Right(a xor d, r(2));
      c := H(c, d);
      b := Rotate_Right(b xor c, r(3));
   end G;

   procedure F (S : in out State; Rounds : in Round_Number) is
   begin
      for I in 1..Rounds loop
         -- Column
         G(S(0), S(4),  S(8), S(12));
         G(S(1), S(5),  S(9), S(13));
         G(S(2), S(6), S(10), S(14));
         G(S(3), S(7), S(11), S(15));

         -- Diagonal
         G(S(0), S(5), S(10), S(15));
         G(S(1), S(6), S(11), S(12));
         G(S(2), S(7),  S(8), S(13));
         G(S(3), S(4),  S(9), S(14));
      end loop;
   end F;

begin

   -- Initialisation constants set up as in 2.5.2 of the specification
   u := ( 0,  1,  2,  3,
          4,  5,  6,  7,
          8,  9, 10, 11,
         12, 13, 14, 15);

   F(u, 2);

end NORX;
