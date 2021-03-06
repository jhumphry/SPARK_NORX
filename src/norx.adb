-- NORX
-- an Ada implementation of the NORX Authenticated Encryption Algorithm
-- created by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves

-- Copyright (c) 2016-2017, James Humphry - see LICENSE file for details

pragma Restrictions(No_Implementation_Attributes,
                    No_Implementation_Units,
                    No_Obsolescent_Features);

with NORX.Compare_Tags;

package body NORX is

   -- ***
   -- Constants and types used internally
   -- ***

   p : constant Positive := 1; -- This implementation only supports parallelism
                               --  of degree 1 i.e. only serial NORX

   Bytes : constant Storage_Offset := Storage_Offset(w / 8);
   Rate_Bytes_SO : constant Storage_Offset := Storage_Offset(r / 8);
   Rate_Words : constant Integer := r / w;
   Key_Words : constant Integer := k / w;
   Tag_Words : constant Integer := t / w;
   Nonce_Words : constant Integer := n / w;

   type Domains is (Header, Payload, Trailer, Tag, Branching, Merging);
   Domain_Separation : constant array (Domains range <>) of Word :=
     (Header    => 16#01#,
      Payload   => 16#02#,
      Trailer   => 16#03#,
      Tag       => 16#04#,
      Branching => 16#10#,
      Merging   => 16#20#);

   subtype Rate_Storage_Array is Storage_Array(1..Rate_Bytes_SO);

   -- The initialisation constants
   u : State
     with Constant_After_Elaboration;

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

   function Valid_Paddable (X : in Storage_Array) return Boolean is
     (
      -- First check that the array can be padded without exceeding the range
      -- of a Storage_Array
      X'Last < Storage_Offset'Last - Rate_Bytes_SO and
        (
         -- now check X'Length < Rate_Bytes_I else padding is not needed...
         -- but in a way that doesn't overflow given Storage_Offset can hold
         -- a range greater than Long_Long_Integer
         if X'Last < X'First then
            True

         elsif X'First < 0 then
           (
                (Long_Long_Integer (X'Last) < Long_Long_Integer'Last +
                       Long_Long_Integer (X'First))
            and then
            X'Last - X'First < Rate_Bytes_SO - 1)

         else
            X'Last - X'First < Rate_Bytes_SO - 1
        )
     )
   with Ghost;

   function Pad_r (X : in Storage_Array) return Rate_Storage_Array
     with Inline, Pre=> Valid_Paddable(X) is
      Result : Rate_Storage_Array;
      Padding : constant Storage_Array(1 .. Rate_Bytes_SO - Storage_Offset(X'Length + 1))
        := (others => 0);
   begin
      Result := X & 16#01# & Padding;
      Result(Result'Last) := Result(Result'Last) or 16#80#;
      return Result;
   end Pad_r;

   function Compare_Tags_Constant_Time is new NORX.Compare_Tags;

   -- ***
   -- Low-level API (mainly) as described in Figure 2.6 of the NORX
   -- specification
   -- ***

   function Make_State return State is (State'(others => 0));

   function Get_Initialisation_Constants return State is (u);

   function Initialise (Key : in Key_Type; Nonce : in Nonce_Type)
                        return State is
      S : State := u;
   begin
      for I in 0..Nonce_Words-1 loop
         S(I) :=
           Storage_Array_To_Word(Nonce(Storage_Offset(I)*Bytes .. Storage_Offset(I+1)*Bytes-1));
      end loop;

      for I in 0..Key_Words-1 loop
         S(I + Key_Position) :=
           Storage_Array_To_Word(Key(Storage_Offset(I)*Bytes .. Storage_Offset(I+1)*Bytes-1));
      end loop;

      S(12) := S(12) xor Word(w);
      S(13) := S(13) xor Word(l);
      S(14) := S(14) xor Word(p);
      S(15) := S(15) xor Word(t);

      F_l(S);

      for I in 0..Key_Words-1 loop
         S(16-Key_Words+I) := S(16-Key_Words+I) xor
           Storage_Array_To_Word(Key(Storage_Offset(I)*Bytes .. Storage_Offset(I+1)*Bytes-1));
      end loop;

      return S;
   end Initialise;

   procedure Absorb_Block (S : in out State;
                           X : in Rate_Storage_Array;
                           v : in Word)
     with Inline is
      X_Index : Storage_Offset := X'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);
      for I in 0..Rate_Words - 1 loop
         pragma Loop_Invariant (X_Index = X'First + Storage_Offset(I) * Bytes);
         S(I) := S(I) xor
           Storage_Array_To_Word(X(X_Index .. X_Index + Bytes - 1));
         X_Index := X_Index + Bytes;
      end loop;

      pragma Assert (X_Index = X'Last + 1);
   end Absorb_Block;

   procedure Absorb (S : in out State; X : in Storage_Array; v : in Word) is
      Number_Full_Blocks : constant Storage_Offset
        := X'Length / Rate_Bytes_SO;
      X_Index : Storage_Offset := X'First;
   begin

      pragma Assert (X'Last < Storage_Offset'Last - Storage_Offset(r/8));

      if X'Length > 0 then

         for I in 1..Number_Full_Blocks loop
            pragma Loop_Invariant (X_Index = X'First + (I-1) * Rate_Bytes_SO);
            Absorb_Block(S,
                         X(X_Index .. X_Index + Rate_Bytes_SO-1),
                         v);
            X_Index := X_Index + Rate_Bytes_SO;
         end loop;

         Absorb_Block(S, Pad_r(X(X_Index..X'Last)), v);

      end if;
   end Absorb;

   procedure Encrypt_Block (S : in out State;
                            M : in Rate_Storage_Array;
                            C : out Rate_Storage_Array;
                            v : in Word)
     with Inline is
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);
      for I in 0..Rate_Words - 1 loop
         pragma Loop_Invariant(M_Index = M'First + Storage_Offset(I) * Bytes);
         pragma Loop_Invariant(C_Index = C'First + Storage_Offset(I) * Bytes);
         S(I) := S(I) xor
           Storage_Array_To_Word(M(M_Index .. M_Index + Bytes - 1));
         C(C_Index .. C_Index + Bytes - 1) := Word_To_Storage_Array(S(I));
         M_Index := M_Index + Bytes;
         C_Index := C_Index + Bytes;
      end loop;

      pragma Assert (M_Index = M'Last + 1);
      pragma Assert (C_Index = C'Last + 1);
   end Encrypt_Block;

   procedure Encrypt (S : in out State;
                      M : in Storage_Array;
                      C : out Storage_Array;
                      v : in Word) is
      Number_Full_Blocks : constant Storage_Offset := M'Length / Rate_Bytes_SO;
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      if M'Length > 0 then
         for I in 1..Number_Full_Blocks loop
            pragma Loop_Invariant(M_Index = M'First + (I-1) * Rate_Bytes_SO);
            pragma Loop_Invariant(C_Index = C'First + (I-1) * Rate_Bytes_SO);
            Encrypt_Block(S => S,
                          M => M(M_Index..M_Index+Rate_Bytes_SO-1),
                          C => C(C_Index..C_Index+Rate_Bytes_SO-1),
                          v => v);
            M_Index := M_Index + Rate_Bytes_SO;
            C_Index := C_Index + Rate_Bytes_SO;
         end loop;

         declare
            Last_M: constant Storage_Array := Pad_r(M(M_Index..M'Last));
            Last_C : Storage_Array(1..Rate_Bytes_SO);
         begin
            Encrypt_Block(S => S,
                          M => Last_M,
                          C => Last_C,
                          v => v);
            C(C_Index..C'Last) := Last_C(1..(C'Last - C_Index)+1);
         end;

      end if;
   end Encrypt;

   pragma Annotate (GNATprove, False_Positive,
                    """C"" might not be initialized",
                    "The loop initialises C from C'First to C_Index-1 and the second block of code initialises C_Index to C'Last");

   procedure Decrypt_Block (S : in out State;
                            C : in Rate_Storage_Array;
                            M : out Rate_Storage_Array;
                            v : in Word)
     with Inline is
      C_i : Word;
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);
      for I in 0..Rate_Words - 1 loop
         pragma Loop_Invariant(M_Index = M'First + Storage_Offset(I) * Bytes);
         pragma Loop_Invariant(C_Index = C'First + Storage_Offset(I) * Bytes);
         C_i := Storage_Array_To_Word(C(C_Index .. C_Index + Bytes - 1));
         M(M_Index .. M_Index + Bytes - 1) := Word_To_Storage_Array(S(I) xor C_i);
         S(I) := C_i;

         M_Index := M_Index + Bytes;
         C_Index := C_Index + Bytes;
      end loop;

      pragma Assert (M_Index = M'Last + 1);
      pragma Assert (C_Index = C'Last + 1);
   end Decrypt_Block;

   function Valid_Last_Ciphertext_Block (X : in Storage_Array) return Boolean is
     (
      if X'Last < X'First then
         True

      elsif X'First < 0 then
        (
             (Long_Long_Integer (X'Last) < Long_Long_Integer'Last +
                    Long_Long_Integer (X'First))
         and then
         X'Last - X'First < Rate_Bytes_SO - 1)

      else
         X'Last - X'First < Rate_Bytes_SO - 1
     )
   with Ghost;

   procedure Decrypt_Last_Block (S : in out State;
                                 C : in Storage_Array;
                                 M : out Storage_Array;
                                 v : in Word)
     with Inline, Pre => ( (Valid_Storage_Array_Parameter(M'First, M'Last) and
                             Valid_Last_Ciphertext_Block(C) )
                           and then
                             M'Length = C'Length
                          ) is

      Last_Block : Storage_Array(1..Rate_Bytes_SO);
      C_i : Word;
      Index : Storage_Offset := Last_Block'First;
   begin
      S(15) := S(15) xor v;
      F_l(S);

      for I in 0..Rate_Words-1 loop
         pragma Loop_Invariant (Index = Last_Block'First + Storage_Offset(I) * Bytes);
         Last_Block(Index .. Index + Bytes-1) := Word_To_Storage_Array(S(I));
         Index := Index + Bytes;
      end loop;

      Last_Block(1..C'Length) := C;
      Last_Block(C'Length+1) := Last_Block(C'Length+1) xor 16#01#;
      Last_Block(Last_Block'Last) := Last_Block(Last_Block'Last) xor 16#80#;

      Index := Last_Block'First;
      for I in 0..Rate_Words - 1 loop
         pragma Loop_Invariant (Index = Last_Block'First + Storage_Offset(I) * Bytes);
         C_i := Storage_Array_To_Word(Last_Block(Index .. Index + Bytes - 1));
         Last_Block(Index .. Index + Bytes - 1)
                    := Word_To_Storage_Array(S(I) xor C_i);
         S(I) := C_i;
         Index := Index + Bytes;
      end loop;

      pragma Assert (Index = Last_Block'Last + 1);

      M := Last_Block(1..C'Length);
   end Decrypt_Last_Block;

   pragma Annotate (GNATprove, False_Positive,
                    """Last_Block"" might not be initialized",
                    "Initialisation and assertion demonstrate that Index is incremented over every element of Last_Block");

   procedure Decrypt (S : in out State;
                      C : in Storage_Array;
                      M : out Storage_Array;
                      v : in Word) is
      Number_Full_Blocks : constant Storage_Offset := C'Length / Rate_Bytes_SO;
      M_Index : Storage_Offset := M'First;
      C_Index : Storage_Offset := C'First;
   begin
      if M'Length > 0 then
         for I in 1..Number_Full_Blocks loop
            pragma Loop_Invariant(M_Index = M'First + (I-1) * Rate_Bytes_SO);
            pragma Loop_Invariant(C_Index = C'First + (I-1) * Rate_Bytes_SO);
            Decrypt_Block(S => S,
                          C => C(C_Index..C_Index+Rate_Bytes_SO-1),
                          M => M(M_Index..M_Index+Rate_Bytes_SO-1),
                          v => v);
            M_Index := M_Index + Rate_Bytes_SO;
            C_Index := C_Index + Rate_Bytes_SO;
         end loop;

         Decrypt_Last_Block(S => S,
                            C => C(C_Index..C'Last),
                            M => M(M_Index..M'Last),
                            v => v);

      end if;
   end Decrypt;

   pragma Annotate (GNATprove, False_Positive,
                    """M"" might not be initialized",
                    "The loop initialises M from M'First to M_Index-1 and the call to Decrypt_Last_Block initialises M_Index to M'Last");

   procedure Finalise (S : in out State;
                       Key : in Key_Type;
                       Tag : out Tag_Type;
                       v : in Word) is
      Tag_Index : Storage_Offset := Tag'First;
   begin
      S(15) := S(15) xor v;

      F_l(S);

      for I in 0..Key_Words-1 loop
         S(16-Key_Words+I) := S(16-Key_Words+I) xor
           Storage_Array_To_Word(Key(Storage_Offset(I)*Bytes .. Storage_Offset(I+1)*Bytes-1));
      end loop;

      F_l(S);

      for I in 0..Key_Words-1 loop
         S(16-Key_Words+I) := S(16-Key_Words+I) xor
           Storage_Array_To_Word(Key(Storage_Offset(I)*Bytes .. Storage_Offset(I+1)*Bytes-1));
      end loop;

      for I in 0..Tag_Words-1 loop
         pragma Loop_Invariant (Tag_Index = Tag'First + Storage_Offset(I) * Bytes);
         Tag(Tag_Index..Tag_Index+Bytes-1) :=
           Word_To_Storage_Array(S(16-Tag_Words+I));
         Tag_Index := Tag_Index + Bytes;
      end loop;

      pragma Assert (Tag_Index = Tag'Last + 1);

   end Finalise;

   pragma Annotate (GNATprove, False_Positive,
                    """Tag"" might not be initialized",
                    "Initialisation and assertion demonstrate that Tag_Index is incremented over every element of Tag");

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
      Finalise(S, K, T, Domain_Separation(Tag));
      pragma Unreferenced (S);
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
      Finalise(S, K, T2, Domain_Separation(Tag));
      pragma Unreferenced (S);
      if Compare_Tags_Constant_Time(T, T2) then
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
