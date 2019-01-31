with Interfaces;
with Ada.Characters.Handling;

package body Aida with SPARK_Mode is

   use type Interfaces.Unsigned_32;
   use type Ada.Containers.Hash_Type;

--     function Is_Digit (C : Character) return Boolean is
--     begin
--        return C in '0' .. '9';
--     end Is_Digit;

   function To_Integer (Source : in Digit_Character) return Integer is
   begin
      return Character'Pos (Source) - Character'Pos ('0');
   end To_Integer;

   procedure To_Integer (Source : in     Character;
                       Target :    out Integer) is
   begin
      Target := Character'Pos (Source) - Character'Pos ('0');
   end To_Integer;

   function To_Char (This : Integer) return Character is
     (
      case This is
         when Integer'First .. 0 => '0',
         when 1 => '1',
         when 2 => '2',
         when 3 => '3',
         when 4 => '4',
         when 5 => '5',
         when 6 => '6',
         when 7 => '7',
         when 8 => '8',
         when 9 .. Integer'Last => '9'
     );

   function To_String (This : Integer) return String is

      subtype Index_T is Integer range 1 .. 16;

      subtype Result_T is String (Index_T);

      procedure Make_Result (Temp   : in out Integer;
                             Result : in out Result_T;
                             P      : in out Index_T) with
        Pre  => Temp >= 0 and 300_000_000 > Temp and P = 15,
        Post => P >= 6 and P <= 15,
        Inline_Always => True;

      procedure Make_Result (Temp   : in out Integer;
                             Result : in out Result_T;
                             P      : in out Index_T)
      is
         Digit : Integer;
      begin
         --  1
         if Temp /= 0 then
            Digit := Temp mod 10;
            Result (P) := To_Char (Digit);
            Temp := Temp / 10;
            P := P - 1;

            --  2
            if Temp /= 0 then
               Digit := Temp mod 10;
               Result (P) := To_Char (Digit);
               Temp := Temp / 10;
               P := P - 1;

               --  3
               if Temp /= 0 then
                  Digit := Temp mod 10;
                  Result (P) := To_Char (Digit);
                  Temp := Temp / 10;
                  P := P - 1;

                  --  4
                  if Temp /= 0 then
                     Digit := Temp mod 10;
                     Result (P) := To_Char (Digit);
                     Temp := Temp / 10;
                     P := P - 1;

                     --  5
                     if Temp /= 0 then
                        Digit := Temp mod 10;
                        Result (P) := To_Char (Digit);
                        Temp := Temp / 10;
                        P := P - 1;

                        --  6
                        if Temp /= 0 then
                           Digit := Temp mod 10;
                           Result (P) := To_Char (Digit);
                           Temp := Temp / 10;
                           P := P - 1;

                           --  7
                           if Temp /= 0 then
                              Digit := Temp mod 10;
                              Result (P) := To_Char (Digit);
                              Temp := Temp / 10;
                              P := P - 1;

                              --  8
                              if Temp /= 0 then
                                 Digit := Temp mod 10;
                                 Result (P) := To_Char (Digit);
                                 Temp := Temp / 10;
                                 P := P - 1;

                                 --  9
                                 if Temp /= 0 then
                                    Digit := Temp mod 10;
                                    Result (P) := To_Char (Digit);
                                    Temp := Temp / 10;
                                    P := P - 1;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end Make_Result;

      Result : Result_T := (others => ' ');

      P : Index_T := Index_T'Last;

      Temp : Integer := This;
   begin
      if Temp <= 0 then
         if Temp = -2_147_483_648 then
            Result (Index_T'Last - 10 .. Index_T'Last) := "-2147483648";
            P := 6;
         else
            Temp := -Temp;

            pragma Assert (Temp >= 0 and 2_147_483_647 >= Temp);

            declare
               Digit : Integer := Temp mod 10;
            begin
               Result (P) := To_Char (Digit);
            end;

            Temp := Temp / 10;

            P := P - 1;

            pragma Warnings (Off, "unused assignment to ""Temp""");
            Make_Result (Temp, Result, P);
            pragma Warnings (On, "unused assignment to ""Temp""");
         end if;
      else
         declare
            Digit : Integer;
         begin
            Digit := Temp mod 10;
            Result (P) := To_Char (Digit);

            Temp := Temp / 10;

            P := P - 1;

            pragma Warnings (Off, "unused assignment to ""Temp""");
            Make_Result (Temp, Result, P);
            pragma Warnings (On, "unused assignment to ""Temp""");
         end;
      end if;

      pragma Assert (P >= 6 and P <= 15);

      if This < 0 then
         Result (P) := '-';
         P := P - 1;
      end if;

      return Result (P + 1 .. Index_T'Last);
   end To_String;

   function To_Hash_Type
     (This : Integer) return Ada.Containers.Hash_Type
   is
      X : Interfaces.Unsigned_32 := (if This >= 0 then
                                        Interfaces.Unsigned_32 (This)
                                     elsif This = Integer'First then
                                        1001
                                     else
                                        Interfaces.Unsigned_32 (-This));
   begin
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X);

      return Ada.Containers.Hash_Type (X);
   end To_Hash_Type;

   function To_String (This : Float) return String is
      pragma SPARK_Mode (Off);
   begin
      return Float'Image (This);
   end To_String;

   procedure To_Integer (Source     : in  String;
                       Target     : out Integer;
                       Has_Failed : out Boolean)
   is
      function I (Source : String;
                  Index  : Natural) return Integer is
        (Aida.To_Integer (Source (Source'First + Index))) with
        Pre   => (Index < Source'Length and then
                    Source'First + Index <= Source'Last and then
                      Source (Source'First + Index) in Digit_Character);
   begin
      if Source'Length = 0 then
         Target := 0;
         Has_Failed := True;
      elsif Source (Source'First) = '-' then
         if Source'Length > 11 then
            Target := 0;
            Has_Failed := True;
         elsif Source'Length = 1 then
            Target := 0;
            Has_Failed := True;
         else
            Target := 0;

            if
              (for all J in (Source'First + 1) .. Source'Last =>
                   Source (J) in Digit_Character)
            then
               if Source'Length = 11 then
                  if Source (Source'First + 1) > '2' then
                     Has_Failed := True;
                  elsif Source (Source'First + 1) < '2' then
                     Target := -1_000_000_000 * I (Source, 1)
                       - 100_000_000 * I (Source, 2)
                       - 10_000_000 * I (Source, 3)
                       - 1_000_000 * I (Source, 4)
                       - 100_000 * I (Source, 5)
                       - 10_000 * I (Source, 6)
                       - 1_000 * I (Source, 7)
                       - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 2) > '1' then
                     Has_Failed := True;
                  elsif Source (Source'First + 2) < '1' then
                     Target := -2_000_000_000 -
                       10_000_000 * I (Source, 3)
                       - 1_000_000 * I (Source, 4)
                       - 100_000 * I (Source, 5)
                       - 10_000 * I (Source, 6)
                       - 1_000 * I (Source, 7)
                       - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 3) > '4' then
                     Has_Failed := True;
                  elsif Source (Source'First + 3) < '4' then
                     Target := -2_100_000_000
                       - 10_000_000 * I (Source, 3)
                       - 1_000_000 * I (Source, 4)
                       - 100_000 * I (Source, 5)
                       - 10_000 * I (Source, 6)
                       - 1_000 * I (Source, 7)
                       - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 4) > '7' then
                     Has_Failed := True;
                  elsif Source (Source'First + 4) < '7' then
                     Target := -2_140_000_000
                       - 1_000_000 * I (Source, 4)
                       - 100_000 * I (Source, 5)
                       - 10_000 * I (Source, 6)
                       - 1_000 * I (Source, 7)
                       - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 5) > '4' then
                     Has_Failed := True;
                  elsif Source (Source'First + 5) < '4' then
                     Target := -2_147_000_000
                       - 100_000 * I (Source, 5)
                       - 10_000 * I (Source, 6)
                       - 1_000 * I (Source, 7)
                       - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 6) > '8' then
                     Has_Failed := True;
                  elsif Source (Source'First + 6) < '8' then
                     Target := -2_147_400_000
                       - 10_000 * I (Source, 6)
                       - 1_000 * I (Source, 7)
                       - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 7) > '3' then
                     Has_Failed := True;
                  elsif Source (Source'First + 7) < '3' then
                     Target := -2_147_480_000 - 1_000 * I (Source, 7)
                       - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 8) > '6' then
                     Has_Failed := True;
                  elsif Source (Source'First + 8) < '6' then
                     Target := -2_147_483_000 - 100 * I (Source, 8)
                       - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 9) > '4' then
                     Has_Failed := True;
                  elsif Source (Source'First + 9) < '4' then
                     Target := -2_147_483_600 - 10 * I (Source, 9)
                       - I (Source, 10);
                     Has_Failed := False;
                  elsif Source (Source'First + 10) > '8' then
                     Has_Failed := True;
                  else
                     Target := -2_147_483_640 - I (Source, 10);
                     Has_Failed := False;
                  end if;
               else
                  case Source'Length is
                     when 2 =>
                        Target := -I (Source, 1);
                        Has_Failed := False;
                     when 3 =>
                        Target := -10 * I (Source, 1) - I (Source, 2);
                        Has_Failed := False;
                     when 4 =>
                        Target := -100 * I (Source, 1)
                          - 10 * I (Source, 2)
                          - I (Source, 3);
                        Has_Failed := False;
                     when 5 =>
                        Target := -1_000 * I (Source, 1)
                          - 100 * I (Source, 2)
                          - 10 * I (Source, 3)
                          - I (Source, 4);
                        Has_Failed := False;
                     when 6 =>
                        Target := -10_000 * I (Source, 1)
                          - 1_000 * I (Source, 2)
                          - 100 * I (Source, 3)
                          - 10 * I (Source, 4)
                          - I (Source, 5);
                        Has_Failed := False;
                     when 7 =>
                        Target := -100_000 * I (Source, 1)
                          - 10_000 * I (Source, 2)
                          - 1_000 * I (Source, 3)
                          - 100 * I (Source, 4)
                          - 10 * I (Source, 5)
                          - I (Source, 6);
                        Has_Failed := False;
                     when 8 =>
                        Target := -1_000_000 * I (Source, 1)
                          - 100_000 * I (Source, 2)
                          - 10_000 * I (Source, 3)
                          - 1_000 * I (Source, 4)
                          - 100 * I (Source, 5)
                          - 10 * I (Source, 6)
                          - I (Source, 7);
                        Has_Failed := False;
                     when 9 =>
                        Target := -10_000_000 * I (Source, 1)
                          - 1000_000 * I (Source, 2)
                          - 100_000 * I (Source, 3)
                          - 10_000 * I (Source, 4)
                          - 1_000 * I (Source, 5)
                          - 100 * I (Source, 6)
                          - 10 * I (Source, 7)
                          - I (Source, 8);
                        Has_Failed := False;
                     when 10 =>
                        Target := -100_000_000 * I (Source, 1)
                          - 10_000_000 * I (Source, 2)
                          - 1_000_000 * I (Source, 3)
                          - 100_000 * I (Source, 4)
                          - 10_000 * I (Source, 5)
                          - 1_000 * I (Source, 6)
                          - 100 * I (Source, 7)
                          - 10 * I (Source, 8)
                          - I (Source, 9);
                        Has_Failed := False;
                     when others =>
                        Target := 0;
                        Has_Failed := True;
                  end case;
               end if;
            else
               Has_Failed := True;
            end if;
         end if;
      else
         if Source'Length > 10 then
            Target := 0;
            Has_Failed := True;
         elsif (for all I in Source'Range => Source (I) in Digit_Character) then
            Target := 0;

            if Source'Length = 10 then
               if Source (Source'First) > '2' then
                  Has_Failed := True;
               elsif Source (Source'First) < '2' then
                  Target := 1_000_000_000 * I (Source, 0)
                    + 100_000_000 * I (Source, 1)
                    + 10_000_000 * I (Source, 2)
                    + 1_000_000 * I (Source, 3)
                    + 100_000 * I (Source, 4)
                    + 10_000 * I (Source, 5)
                    + 1_000 * I (Source, 6)
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 1) > '1' then
                  Has_Failed := True;
               elsif Source (Source'First + 1) < '1' then
                  Target := 2_000_000_000
                    + 10_000_000 * I (Source, 2)
                    + 1_000_000 * I (Source, 3)
                    + 100_000 * I (Source, 4)
                    + 10_000 * I (Source, 5)
                    + 1_000 * I (Source, 6)
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 2) > '4' then
                  Has_Failed := True;
               elsif Source (Source'First + 2) < '4' then
                  Target := 2_100_000_000 + 10_000_000 * I (Source, 2)
                    + 1_000_000 * I (Source, 3)
                    + 100_000 * I (Source, 4)
                    + 10_000 * I (Source, 5)
                    + 1_000 * I (Source, 6)
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 3) > '7' then
                  Has_Failed := True;
               elsif Source (Source'First + 3) < '7' then
                  Target := 2_140_000_000
                    + 1_000_000 * I (Source, 3)
                    + 100_000 * I (Source, 4)
                    + 10_000 * I (Source, 5)
                    + 1_000 * I (Source, 6)
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 4) > '4' then
                  Has_Failed := True;
               elsif Source (Source'First + 4) < '4' then
                  Target := 2_147_000_000
                    + 100_000 * I (Source, 4)
                    + 10_000 * I (Source, 5)
                    + 1_000 * I (Source, 6)
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 5) > '8' then
                  Has_Failed := True;
               elsif Source (Source'First + 5) < '8' then
                  Target := 2_147_400_000
                    + 10_000 * I (Source, 5)
                    + 1_000 * I (Source, 6)
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 6) > '3' then
                  Has_Failed := True;
               elsif Source (Source'First + 6) < '3' then
                  Target := 2_147_480_000
                    + 1_000 * I (Source, 6)
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 7) > '6' then
                  Has_Failed := True;
               elsif Source (Source'First + 7) < '6' then
                  Target := 2_147_483_000
                    + 100 * I (Source, 7)
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 8) > '4' then
                  Has_Failed := True;
               elsif Source (Source'First + 8) < '4' then
                  Target := 2_147_483_600
                    + 10 * I (Source, 8)
                    + I (Source, 9);
                  Has_Failed := False;
               elsif Source (Source'First + 9) > '7' then
                  Has_Failed := True;
               else
                  Target := 2_147_483_640 + I (Source, 9);

                  Has_Failed := False;
               end if;
            else
               case Source'Length is
                  when 1 =>
                     Target := I (Source, 0);
                     Has_Failed := False;
                  when 2 =>
                     Target := I (Source, 0) * 10 + I (Source, 1);
                     Has_Failed := False;
                  when 3 =>
                     Target :=
                       I (Source, 0) * 100 +
                       I (Source, 1) * 10 +
                       I (Source, 2) * 1;
                     Has_Failed := False;
                  when 4 =>
                     Target :=
                       I (Source, 0) * 1_000 +
                       I (Source, 1) * 100 +
                       I (Source, 2) * 10 +
                       I (Source, 3) * 1;
                     Has_Failed := False;
                  when 5 =>
                     Target :=
                       I (Source, 0) * 10_000 +
                       I (Source, 1) * 1_000 +
                       I (Source, 2) * 100 +
                       I (Source, 3) * 10 +
                       I (Source, 4) * 1;
                     Has_Failed := False;
                  when 6 =>
                     Target :=
                       I (Source, 0) * 100_000 +
                       I (Source, 1) * 10_000 +
                       I (Source, 2) * 1_000 +
                       I (Source, 3) * 100 +
                       I (Source, 4) * 10 +
                       I (Source, 5) * 1;
                     Has_Failed := False;
                  when 7 =>
                     Target := I (Source, 0) * 1_000_000 +
                       I (Source, 1) * 100_000 +
                       I (Source, 2) * 10_000 +
                       I (Source, 3) * 1_000 +
                       I (Source, 4) * 100 +
                       I (Source, 5) * 10 +
                       I (Source, 6) * 1;
                     Has_Failed := False;
                  when 8 =>
                     Target :=
                       I (Source, 0) * 10_000_000 +
                       I (Source, 1) * 1_000_000 +
                       I (Source, 2) * 100_000 +
                       I (Source, 3) * 10_000 +
                       I (Source, 4) * 1_000 +
                       I (Source, 5) * 100 +
                       I (Source, 6) * 10 +
                       I (Source, 7) * 1;
                     Has_Failed := False;
                  when 9 =>
                     Target := I (Source, 0) * 100_000_000 +
                       I (Source, 1) * 10_000_000 +
                       I (Source, 2) * 1_000_000 +
                       I (Source, 3) * 100_000 +
                       I (Source, 4) * 10_000 +
                       I (Source, 5) * 1_000 +
                       I (Source, 6) * 100 +
                       I (Source, 7) * 10 +
                       I (Source, 8) * 1;
                     Has_Failed := False;
                  when others =>
                     Target := 0;
                     Has_Failed := True;
               end case;
            end if;
         else
            Target := 0;
            Has_Failed := True;
         end if;
      end if;
   end To_Integer;

   procedure To_Float (Source     : in  String;
                       Target     : out Float;
                       Has_Failed : out Boolean)
   is
      pragma SPARK_Mode (Off);
   begin
      Target := Float'Value (Source);
      Has_Failed := False;
   exception
      when Constraint_Error =>
         Has_Failed := True;
   end To_Float;

   function Is_Latin1_Graphic_Characters (Text : String) return Boolean is
      Result : Boolean := True;
   begin
      for I in Text'Range loop
         if not Ada.Characters.Handling.Is_Graphic (Text (I)) then
            Result := False;
            exit;
         end if;
      end loop;

      return Result;
   end Is_Latin1_Graphic_Characters;

   function Starts_With (This         : String;
                         Searched_For : String) return Boolean
   is
      Result : Boolean;
   begin
      if Searched_For'Length > This'Length then
         Result := False;
      else
         Result :=
           (for all Index in Searched_For'Range =>
              This (Index - Searched_For'First + This'First) =
                Searched_For (Index));
      end if;

      return Result;
   end Starts_With;

   function To_Hash_Type (This : String) return Ada.Containers.Hash_Type is
      H : Ada.Containers.Hash_Type := 0;
      A : Ada.Containers.Hash_Type := 31_415;
      B : constant Ada.Containers.Hash_Type := 27_183;
   begin
      for I in Positive range This'First .. This'Last loop
         H := A * H + Character'Pos (This (I));
         A := A * B;
         pragma Loop_Variant (Increases => I);
      end loop;

      return H;
   end To_Hash_Type;

   function Concat (Left, Right : String) return String is
      S : String (1 .. Left'Length + Right'Length) := (others => ' ');
   begin
      S (1 .. Left'Length) := Left (Left'First .. Left'Last);
      S (1 + Left'Length .. Left'Length + Right'Length)
        := Right (Right'First .. Right'Last);
      return S;
   end Concat;

   procedure Initialize (This : in out Bounded_String; Text : String) is
   begin
      for I in Integer range 1 .. Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
         pragma Loop_Invariant
           (for all J in Integer range 1 .. I =>
              This.Text (J) = Text (Text'First - 1 + J));
         pragma Loop_Variant (Increases => I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize;

   procedure Initialize2 (This : out Bounded_String; Text : String) is
   begin
      This.Text := (others => ' ');
      for I in Integer range 1 .. Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
         pragma Loop_Invariant
           (for all J in Integer range 1 .. I =>
              This.Text (J) = Text (Text'First - 1 + J));
         pragma Loop_Variant (Increases => I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize2;

   procedure Append (Target : in out Bounded_String; Source : String) is
   begin
      for I in Integer range Source'First .. Source'Last loop
         Target.Text (Target.Text_Length + 1 + (I - Source'First)) :=
           Source (I);
      end loop;
      Target.Text_Length := Target.Text_Length + Source'Length;
   end Append;

   function To_Hash_Type
     (This : Bounded_String) return Ada.Containers.Hash_Type is
   begin
      return Aida.To_Hash_Type (This.Text (1 .. Length (This)));
   end To_Hash_Type;

   function Equals (This : Bounded_String; Object : String) return Boolean is
      Result : Boolean := True;
   begin
      if Length (This) = Object'Length then
         if Object'Length > 0 then
            Result :=
              This.Text (1 .. This.Text_Length) = Object (Object'Range);
         end if;
      end if;

      return Result;
   end Equals;

   function To_String (This : Bounded_String) return String is
   begin
      return This.Text (1 .. This.Text_Length);
   end To_String;

   procedure Initialize (This    : in out Call_Result;
                         Code_1 : Integer;
                         Code_2 : Integer) is
   begin
      This.My_Code_1 := Code_1;
      This.My_Code_2 := Code_2;
      This.My_Has_Failed := True;
   end Initialize;

   function Message (This : Call_Result) return String is
      subtype Index_T is Positive range 1 .. 24;

      Text : String (Index_T'Range) := (others => '0');

      Max : Index_T;
   begin
      if This.My_Code_1 >= 0 then
         if This.My_Code_2 >= 0 then
            Max := 22;

            declare
               Text2 : String := Aida.To_String (This.My_Code_1);
               L : Integer := 10 - Text2'Length + 1;
            begin
               pragma Assert (L <= 10);
               Text (L .. 10) := Text2 (Text2'First .. Text2'Last);
            end;

            Text (11 .. 12) := ", ";

            declare
               Text2 : String := Aida.To_String (This.My_Code_2);
               L : Integer := 22 - Text2'Length + 1;
            begin
               Text (L .. 22) := Text2 (Text2'First .. Text2'Last);
            end;
         else
            Max := 23;

            declare
               Text2 : String := Aida.To_String (This.My_Code_1);
               L : Integer := 10 - Text2'Length + 1;
            begin
               pragma Assert (L <= 10);
               Text (L .. 10) := Text2 (Text2'First .. Text2'Last);
            end;

            Text (11 .. 12) := ", ";

            declare
               Text2 : String := Aida.To_String (This.My_Code_2);
               L : Integer := 23 - Text2'Length + 1;
            begin
               Text (13) := '-';
               Text (L + 1 .. 23) := Text2 (Text2'First + 1 .. Text2'Last);
            end;
         end if;
      else
         if This.My_Code_2 >= 0 then
            Max := 23;

            declare
               Text2 : String := Aida.To_String (This.My_Code_1);
               L : Integer := 11 - Text2'Length + 1;
            begin
               pragma Assert (L <= 11);
               Text (1) := '-';
               Text (L + 1 .. 11) := Text2 (Text2'First + 1 .. Text2'Last);
            end;

            Text (12 .. 13) := ", ";

            declare
               Text2 : String := Aida.To_String (This.My_Code_2);
               L : Integer := 23 - Text2'Length + 1;
            begin
               Text (L .. 23) := Text2 (Text2'First .. Text2'Last);
            end;
         else
            Max := 24;

            declare
               Text2 : String := Aida.To_String (This.My_Code_1);
               L : Integer := 11 - Text2'Length + 1;
            begin
               pragma Assert (L <= 11);
               Text (1) := '-';
               Text (L + 1 .. 11) := Text2 (Text2'First + 1 .. Text2'Last);
            end;

            Text (12 .. 13) := ", ";

            declare
               Text2 : String := Aida.To_String (This.My_Code_2);
               L : Integer := 24 - Text2'Length + 1;
            begin
               Text (14) := '-';
               Text (L + 1 .. 24) := Text2 (Text2'First + 1 .. Text2'Last);
            end;
         end if;
      end if;

      return Text (1 .. Max);
   end Message;

end Aida;
