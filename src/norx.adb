-- NORX
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- Copyright (c) 2016, James Humphry - see LICENSE file for details

package body NORX is

   -- ***
   -- Constants and types used internally
   -- ***

   p : constant Positive := 1; -- This implementation only supports parallelism
                               --  of degree 1 i.e. only serial NORX

   Bytes : constant Storage_Offset := Storage_Offset(w / 8);
   Rate_Bytes : constant Integer := (r / 8);
   Rate_Words : constant Integer := r / w;
   Tag_Words : constant Integer := t / w;

   type Domains is (Header, Payload, Trailer, Tag, Branching, Merging);
   Domain_Separation : constant array (Domains range <>) of Word :=
     (Header    => 16#01#,
      Payload   => 16#02#,
      Trailer   => 16#03#,
      Tag       => 16#04#,
      Branching => 16#10#,
      Merging   => 16#20#);

   u : State; -- The initialisation constants

   -- ***
   -- Implementation of the the permutation F^{l} as described in Figure 2.4
   -- of the NORX specification
   -- ***

   procedure G (a, b, c, d : in out Word)
     with Inline is

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

   procedure F_l (S : in out State)
   with Inline is
   begin
      for I in 1..l loop
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
   end F_l;

   procedure F_2 (S : in out State) is
      -- The initialisation constants are set up by two rounds of the diffusion
      -- regardless of the number of rounds l

   begin
      for I in 1..2 loop
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
   end F_2;

   -- ***
   -- Internal use routines
   -- ***

   function Pad_r (X : in Storage_Array) return Storage_Array
     with Inline, Pre=> (X'Length < Rate_Bytes) is
      Result : Storage_Array(1..Storage_Offset(Rate_Bytes));
   begin
      Result(1..X'Length) := X;
      Result(X'Length + 1) := 16#01#;
      Result(X'Length + 2 .. Storage_Offset(Rate_Bytes) - 1) := (others => 0);
      Result(Storage_Offset(Rate_Bytes)) := 16#80#;
      return Result;
   end Pad_r;

   function Compare_Tags (L, R : Tag_Type) return Boolean is
      -- This function compares two tags and returns a Boolean to indicate
      -- if they are equal. It aims to perform the comparison in constant
      -- time regardless of the inputs, as required by section 2.5 of the
      -- specification

      Result : Storage_Element := 0;
   begin
      for I in L'Range loop
         Result := Result or (L(I) xor (R(I)));
      end loop;
      return (Result = 0);
   end Compare_Tags;

   -- ***
   -- Low-level API (mainly) as described in Figure 2.6 of the NORX
   -- specification
   -- ***

   function Get_Initialisation_Constants return State is (u);

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
      F_l(S);
      return S;
   end Initialise;


   procedure Absorb_Block (S : in out State;
                           X : in Storage_Array;
                           v : in Word)
     with Inline, Pre => (X'Length = Rate_Bytes) is
      X_Index : Storage_Offset := X'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);
      for I in 0..Rate_Words - 1 loop
         S(I) := S(I) xor
           Storage_Array_To_Word(X(X_Index .. X_Index + Bytes - 1));
         X_Index := X_Index + Bytes;
      end loop;
   end Absorb_Block;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word) is
      Number_Full_Blocks : constant Natural := X'Length / Rate_Bytes;
      X_Index : Storage_Offset := X'First;
   begin
      if X'Length > 0 then

         for I in 1..Number_Full_Blocks loop
            Absorb_Block(S,
                         X(X_Index .. X_Index + Storage_Offset(Rate_Bytes)-1),
                         v);
            X_Index := X_Index + Storage_Offset(Rate_Bytes);
         end loop;

         Absorb_Block(S, Pad_r(X(X_Index..X'Last)), v);

      end if;
   end Absorb;

   procedure Encrypt_Block (S : in out State;
                            M : in Storage_Array;
                            C : out Storage_Array;
                            v : in Word)
     with Inline, Pre => (M'Length = C'Length and M'Length = Rate_Bytes) is
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);
      for I in 0..Rate_Words - 1 loop
         S(I) := S(I) xor
           Storage_Array_To_Word(M(M_Index .. M_Index + Bytes - 1));
         C(C_Index .. C_Index + Bytes - 1) := Word_To_Storage_Array(S(I));
         M_Index := M_Index + Bytes;
         C_Index := C_Index + Bytes;
      end loop;
   end Encrypt_Block;

   procedure Encrypt (S : in out State;
                      M : in Storage_Array;
                      C : out Storage_Array;
                      v : in Word) is
      Number_Full_Blocks : constant Natural := M'Length / Rate_Bytes;
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      if M'Length > 0 then
         for I in 1..Number_Full_Blocks loop
            Encrypt_Block(S => S,
                          M => M(M_Index..M_Index+Storage_Offset(Rate_Bytes)-1),
                          C => C(C_Index..C_Index+Storage_Offset(Rate_Bytes)-1),
                          v => v);
            M_Index := M_Index + Storage_Offset(Rate_Bytes);
            C_Index := C_Index + Storage_Offset(Rate_Bytes);
         end loop;

         declare
            Last_M: constant Storage_Array := Pad_r(M(M_Index..M'Last));
            Last_C : Storage_Array(1..Storage_Offset(Rate_Bytes));
         begin
            Encrypt_Block(S => S,
                          M => Last_M,
                          C => Last_C,
                          v => v);
            C(C_Index..C'Last) := Last_C(1..(C'Last - C_Index)+1);
         end;

      end if;
   end Encrypt;

   procedure Decrypt_Block (S : in out State;
                            C : in Storage_Array;
                            M : out Storage_Array;
                            v : in Word)
     with Inline, Pre => (M'Length = C'Length and M'Length = Rate_Bytes) is
      C_i : Word;
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);
      for I in 0..Rate_Words - 1 loop
         C_i := Storage_Array_To_Word(C(C_Index .. C_Index + Bytes - 1));
         M(M_Index .. M_Index + Bytes - 1) := Word_To_Storage_Array(S(I) xor C_i);
         S(I) := C_i;

         M_Index := M_Index + Bytes;
         C_Index := C_Index + Bytes;
      end loop;
   end Decrypt_Block;

   procedure Decrypt_Last_Block (S : in out State;
                                 C : in Storage_Array;
                                 M : out Storage_Array;
                                 v : in Word)
     with Inline, Pre => (M'Length = C'Length) is

      Last_Block : Storage_Array(1..Storage_Offset(Rate_Bytes));
      C_i : Word;
      Index : Storage_Offset := Last_Block'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);

      for I in 0..Rate_Words-1 loop
         Last_Block(Index .. Index + Bytes-1) := Word_To_Storage_Array(S(I));
         Index := Index + Bytes;
      end loop;

      Last_Block(1..C'Length) := C;
      Last_Block(C'Length+1) := Last_Block(C'Length+1) xor 16#01#;
      Last_Block(Last_Block'Last) := Last_Block(Last_Block'Last) xor 16#80#;

      Index := Last_Block'First;
      for I in 0..Rate_Words - 1 loop
         C_i := Storage_Array_To_Word(Last_Block(Index .. Index + Bytes - 1));
         Last_Block(Index .. Index + Bytes - 1)
                    := Word_To_Storage_Array(S(I) xor C_i);
         S(I) := C_i;
         Index := Index + Bytes;
      end loop;

      M := Last_Block(1..C'Length);
   end Decrypt_Last_Block;

   procedure Decrypt (S : in out State;
                      C : in Storage_Array;
                      M : out Storage_Array;
                      v : in Word) is
      Number_Full_Blocks : constant Natural := M'Length / Rate_Bytes;
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      if M'Length > 0 then
         for I in 1..Number_Full_Blocks loop
            Decrypt_Block(S => S,
                          C => C(C_Index..C_Index+Storage_Offset(Rate_Bytes)-1),
                          M => M(M_Index..M_Index+Storage_Offset(Rate_Bytes)-1),
                          v => v);
            M_Index := M_Index + Storage_Offset(Rate_Bytes);
            C_Index := C_Index + Storage_Offset(Rate_Bytes);
         end loop;

         Decrypt_Last_Block(S => S,
                            C => C(C_Index..C'Last),
                            M => M(M_Index..M'Last),
                            v => v);

      end if;
   end Decrypt;

   procedure Finalise (S : in out State; Tag : out Tag_Type; v : in Word) is
      Tag_Index : Storage_Offset := Tag'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);
      F_l(S);
      for I in 0 .. Tag_Words-1 loop
         Tag(Tag_Index .. Tag_Index + Bytes - 1) := Word_To_Storage_Array(S(I));
         Tag_Index := Tag_Index + Bytes;
      end loop;
   end Finalise;

   -- ***
   -- High-level API as described in Figure 2.5 of the NORX specification
   -- ***

   procedure AEADEnc(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     M : in Storage_Array;
                     Z : in Storage_Array;
                     C : out Storage_Array;
                     T : out Tag_Type) is
      S : State := Initialise(K, N);
   begin
      Absorb(S, A, Domain_Separation(Header));
      Encrypt(S, M, C, Domain_Separation(Payload));
      Absorb(S, Z, Domain_Separation(Trailer));
      Finalise(S, T, Domain_Separation(Tag));
   end AEADEnc;

   procedure AEADDec(K : in Key_Type;
                     N : in Nonce_Type;
                     A : in Storage_Array;
                     C : in Storage_Array;
                     Z : in Storage_Array;
                     T : in Tag_Type;
                     M : out Storage_Array;
                     Valid : out Boolean) is
      S : State := Initialise(K, N);
      T2 : Tag_Type;
   begin
      Absorb(S, A, Domain_Separation(Header));
      Decrypt(S, C, M, Domain_Separation(Payload));
      Absorb(S, Z, Domain_Separation(Trailer));
      Finalise(S, T2, Domain_Separation(Tag));
      if Compare_Tags(T, T2) then
         Valid := True;
      else
         -- Section 2.5 of the specification requires that the decrypted data
         -- not be returned to the caller if verification fails, to try to
         -- prevent callers from forgetting to check the validity of the result.
         M := (others => 0);
         Valid := False;
      end if;
   end AEADDec;

begin

   -- Initialisation constants set up as in 2.5.2 of the specification
   u := ( 0,  1,  2,  3,
          4,  5,  6,  7,
          8,  9, 10, 11,
          12, 13, 14, 15);

   F_2(u);

end NORX;
