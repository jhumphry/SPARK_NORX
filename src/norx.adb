-- NORX
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

package body NORX is

   -- Constants used internally

   p : constant Positive := 1; -- This implementation only supports parallelism
                               --  of degree 1 i.e. only serial NORX

   Bytes : constant Storage_Offset := Storage_Offset(w / 8);

   type Domains is (Header, Payload, Trailer, Tag, Branching, Merging);
   Domain_Separation : constant array (Domains range <>) of Word :=
     (Header    => 16#01#,
      Payload   => 16#02#,
      Trailer   => 16#03#,
      Tag       => 16#04#,
      Branching => 16#10#,
      Merging   => 16#20#);

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
      d := Rotate_Right(a xor d, rot(0));
      c := H(c, d);
      b := Rotate_Right(b xor c, rot(1));
      a := H(a, b);
      d := Rotate_Right(a xor d, rot(2));
      c := H(c, d);
      b := Rotate_Right(b xor c, rot(3));
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

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type)
                        return State is

      N : constant array (Integer range 0..1) of Word :=
        (Storage_Array_To_Word(Nonce(0..Bytes-1)),
         Storage_Array_To_Word(Nonce(Bytes..2*Bytes-1)));

      K : constant array (Integer range 0..3) of Word :=
        (Storage_Array_To_Word(Key(0..Bytes-1)),
         Storage_Array_To_Word(Key(Bytes..2*Bytes-1)),
         Storage_Array_To_Word(Key(2*Bytes..3*Bytes-1)),
         Storage_Array_To_Word(Key(3*Bytes..4*Bytes-1)));

      S : State := (N( 0), N( 1), u( 2), u( 3),
                    K( 0), K( 1), K( 2), K( 3),
                    u( 8), u( 9), u(10), u(11),
                    u(12), u(13), u(14), u(15));

   begin
      S(12) := S(12) xor Word(w);
      S(13) := S(13) xor Word(l);
      S(14) := S(14) xor Word(p);
      S(15) := S(15) xor Word(t);
      F(S, l);
      return S;
   end Initialise;

begin

   -- Initialisation constants set up as in 2.5.2 of the specification
   u := ( 0,  1,  2,  3,
          4,  5,  6,  7,
          8,  9, 10, 11,
         12, 13, 14, 15);

   F(u, 2);

end NORX;
