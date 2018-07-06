package body Aida.UTF8 with
   SPARK_Mode
 is

   procedure Get (Source :     String; Pointer : in out Int32;
      Value              : out Aida.UTF8_Code_Point.T)
   is
      Accum : Aida.UTF8_Code_Point.T'Base;
      Code  : Aida.UTF8_Code_Point.T'Base;
   begin
      Code := Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer)));

      case Code is
         when 0 .. 16#7F# => -- 1 byte (ASCII)
            Value   := Code;
            Pointer := Pointer + 1;
         when 16#C2# .. 16#DF# => -- 2 bytes
            Accum   := (Code and 16#1F#) * 2**6;
            Code := UTF8_Code_Point.T (Character'Pos (Source (Pointer + 1)));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 2;
         when 16#E0# => -- 3 bytes
            Code :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 1)));
            Accum := (Code and 16#3F#) * 2**6;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 2)));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 3;
         when 16#E1# .. 16#EF# => -- 3 bytes
            Accum := (Code and 16#0F#) * 2**12;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 1)));
            Accum := Accum or (Code and 16#3F#) * 2**6;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 2)));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 3;
         when 16#F0# => -- 4 bytes
            Code :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 1)));
            Accum := (Code and 16#3F#) * 2**12;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 2)));
            Accum := Accum or (Code and 16#3F#) * 2**6;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 3)));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 4;
         when 16#F1# .. 16#F3# => -- 4 bytes
            Accum := (Code and 16#07#) * 2**18;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 1)));
            Accum := Accum or (Code and 16#3F#) * 2**12;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 2)));
            Accum := Accum or (Code and 16#3F#) * 2**6;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 3)));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 4;
         when 16#F4# => -- 4 bytes
            Accum := (Code and 16#07#) * 2**18;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 1)));
            Accum := Accum or (Code and 16#3F#) * 2**12;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 2)));
            Accum := Accum or (Code and 16#3F#) * 2**6;
            Code  :=
              Aida.UTF8_Code_Point.T (Character'Pos (Source (Pointer + 3)));
            Value   := Accum or (Code and 16#3F#);
            Pointer := Pointer + 4;
         when others =>
            raise Constraint_Error;
            --  This exception will never be raised if pre-conditions are met.
      end case;
   end Get;

   function Is_Valid_UTF8 (Source : String) return Boolean is
      Accum : Aida.UTF8_Code_Point.T;
      pragma Unreferenced (Accum);

      Index : Int32 := Source'First;
   begin
      while Index <= Source'Last loop
         if Is_Valid_UTF8_Code_Point (Source, Index) then
            Get (Source, Index, Accum);
         else
            exit;
         end if;
         pragma Loop_Variant (Increases => Index);
      end loop;
      return Index = Source'Last + 1;
   end Is_Valid_UTF8;

   function Length (Source : String) return Nat32 is
      Count : Nat32 := 0;
      Accum : Aida.UTF8_Code_Point.T;
      pragma Unreferenced (Accum);

      Index : Int32 := Source'First;
   begin
      while Index <= Source'Last loop
         if Is_Valid_UTF8_Code_Point (Source, Index) then
            Get (Source, Index, Accum);
            Count := Count + 1;
         else
            exit;
         end if;
         pragma Loop_Invariant (Count <= Index - Source'First);
      end loop;
      return Count;
   end Length;

   procedure Put (Destination : in out String; Pointer : in out Int32;
      Value                   :        Aida.UTF8_Code_Point.T)
   is
   begin
      if Value <= 16#7F# then
         Destination (Pointer) := Character'Val (Value);
         Pointer               := Pointer + 1;
      elsif Value <= 16#7FF# then
         Destination (Pointer)     := Character'Val (16#C0# or Value / 2**6);
         Destination (Pointer + 1) :=
           Character'Val (16#80# or (Value and 16#3F#));
         Pointer := Pointer + 2;
      elsif Value <= 16#FFFF# then
         Destination (Pointer)     := Character'Val (16#E0# or Value / 2**12);
         Destination (Pointer + 1) :=
           Character'Val (16#80# or (Value / 2**6 and 16#3F#));
         Destination (Pointer + 2) :=
           Character'Val (16#80# or (Value and 16#3F#));
         Pointer := Pointer + 3;
      else
         Destination (Pointer)     := Character'Val (16#F0# or Value / 2**18);
         Destination (Pointer + 1) :=
           Character'Val (16#80# or (Value / 2**12 and 16#3F#));
         Destination (Pointer + 2) :=
           Character'Val (16#80# or (Value / 2**6 and 16#3F#));
         Destination (Pointer + 3) :=
           Character'Val (16#80# or (Value and 16#3F#));
         Pointer := Pointer + 4;
      end if;
   end Put;

   function To_Lowercase (Value : String) return String is
      Result : String (1 .. Value'Length);
      From   : Int32 := Value'First;
      To     : Int32 := 1;
      Code   : Aida.UTF8_Code_Point.T;
   begin
      while From <= Value'Last loop
         pragma Assume (Is_Valid_UTF8_Code_Point (Value, From));

         Aida.UTF8.Get (Value, From, Code);
         Code := To_Lowercase (Code);
         Aida.UTF8.Put (Result, To, Code);
      end loop;
      return Result (1 .. To - 1);
   end To_Lowercase;

   function To_Uppercase (Value : String) return String is
      Result : String (1 .. Value'Length);
      From   : Int32 := Value'First;
      To     : Int32 := 1;
      Code   : Aida.UTF8_Code_Point.T;
   begin
      while From <= Value'Last loop
         pragma Assume (Is_Valid_UTF8_Code_Point (Value, From));

         Aida.UTF8.Get (Value, From, Code);
         Code := To_Uppercase (Code);
         Aida.UTF8.Put (Result, To, Code);
      end loop;
      return Result (1 .. To - 1);
   end To_Uppercase;

end Aida.UTF8;
