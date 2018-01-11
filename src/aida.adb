with Interfaces;
with Ada.Characters.Handling;

package body Aida is
   pragma SPARK_Mode;

   use type Interfaces.Unsigned_32;

   package body Character is

      function Is_Digit (C : Character_T) return Boolean is
      begin
         return C in '0'..'9';
      end Is_Digit;

      function To_Int32 (Source : in Character_T) return Int32_T is
      begin
         return Standard.Character'Pos (Standard.Character (Source)) - Standard.Character'Pos ('0');
      end To_Int32;

      procedure To_Int32 (Source : in     Character_T;
                          Target :    out Int32_T) is
      begin
         Target := Standard.Character'Pos (Standard.Character (Source)) - Standard.Character'Pos ('0');
      end To_Int32;

   end Character;

   package body Hash32 is

      function Hash32 (This : Int32_T) return Hash32_T is (Aida.Int32.Hash32 (This));

      function Hash32 (This : String_T) return Hash32_T is (Aida.String.Hash32 (This));

   end Hash32;

   package body Int32 is

      function To_Char (This : Int32_T) return Character_T is (
                                                               case This is
                                                                  when 0 => '0',
                                                                  when 1 => '1',
                                                                  when 2 => '2',
                                                                  when 3 => '3',
                                                                  when 4 => '4',
                                                                  when 5 => '5',
                                                                  when 6 => '6',
                                                                  when 7 => '7',
                                                                  when 8 => '8',
                                                                  when 9 => '9',
                                                                  when others => '0'
                                                              );

      function To_String (This : Int32_T) return String_T is

         subtype Index_T is Int32_T range 1..16;

         subtype Result_T is String_T (Index_T);

         procedure Make_Result (Temp   : in out Int32_T;
                                Result : in out Result_T;
                                P      : in out Index_T) with
           Pre  => Temp >= 0 and 300_000_000 > Temp and P = 15,
           Post => P >= 6 and P <= 15,
           Inline_Always => True;

         procedure Make_Result (Temp   : in out Int32_T;
                                Result : in out Result_T;
                                P      : in out Index_T)
         is
            Digit : Int32_T;
         begin
            -- 1
            if Temp /= 0 then
               Digit := Temp mod 10;
               Result (P) := To_Char (Digit);
               Temp := Temp / 10;
               P := P - 1;

               -- 2
               if Temp /= 0 then
                  Digit := Temp mod 10;
                  Result (P) := To_Char (Digit);
                  Temp := Temp / 10;
                  P := P - 1;

                  -- 3
                  if Temp /= 0 then
                     Digit := Temp mod 10;
                     Result (P) := To_Char (Digit);
                     Temp := Temp / 10;
                     P := P - 1;

                     -- 4
                     if Temp /= 0 then
                        Digit := Temp mod 10;
                        Result (P) := To_Char (Digit);
                        Temp := Temp / 10;
                        P := P - 1;

                        -- 5
                        if Temp /= 0 then
                           Digit := Temp mod 10;
                           Result (P) := To_Char (Digit);
                           Temp := Temp / 10;
                           P := P - 1;

                           -- 6
                           if Temp /= 0 then
                              Digit := Temp mod 10;
                              Result (P) := To_Char (Digit);
                              Temp := Temp / 10;
                              P := P - 1;

                              -- 7
                              if Temp /= 0 then
                                 Digit := Temp mod 10;
                                 Result (P) := To_Char (Digit);
                                 Temp := Temp / 10;
                                 P := P - 1;

                                 -- 8
                                 if Temp /= 0 then
                                    Digit := Temp mod 10;
                                    Result (P) := To_Char (Digit);
                                    Temp := Temp / 10;
                                    P := P - 1;

                                    -- 9
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

         Temp : Int32_T := This;
      begin
         if Temp <= 0 then
            if Temp = -2_147_483_648 then
               Result (Index_T'Last - 10..Index_T'Last) := "-2147483648";
               P := 6;
            else
               Temp := -Temp;

               pragma Assert (Temp >= 0 and 2_147_483_647 >= Temp);

               declare
                  Digit : Int32_T := Temp mod 10;
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
               Digit : Int32_T;
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

         return Result (P+1..Index_T'Last);
      end To_String;

      function Hash32 (This : Int32_T) return Hash32_T is
         X : Interfaces.Unsigned_32 := (if This >= 0 then
                                           Interfaces.Unsigned_32 (This)
                                        elsif This = Int32_T'First then
                                           1001
                                        else
                                           Interfaces.Unsigned_32 (-This));
      begin
         X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
         X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
         X := (Interfaces.Shift_Right (X, 16) xor X);

         return Hash32_T (X);
      end Hash32;

   end Int32;

   package body Float is

      function To_String (This : Float_T) return String_T is
         pragma SPARK_Mode (Off);
      begin
         return Float_T'Image (This);
      end To_String;

   end Float;

   package body String is

      procedure To_Int32 (Source     : in  String_T;
                          Target     : out Int32_T;
                          Has_Failed : out Boolean) is
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

               if (for all J in (Source'First + 1)..Source'Last => Aida.Character.Is_Digit (Source(J))) then
                  if Source'Length = 11 then
                     if Source (Source'First + 1) > '2' then
                        Has_Failed := True;
                     elsif Source (Source'First + 1) < '2' then
                        Target := -1_000_000_000*I (Source, 1) - 100_000_000*I (Source, 2) -
                          10_000_000*I (Source, 3) -
                          1_000_000*I (Source, 4) -
                          100_000*I (Source, 5) - 10_000*I (Source, 6) -
                          1_000*I (Source, 7) - 100*I (Source, 8) -
                          10*I (Source, 9) - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 2) > '1' then
                        Has_Failed := True;
                     elsif Source (Source'First + 2) < '1' then
                        Target := -2_000_000_000 -
                          10_000_000*I (Source, 3) -
                          1_000_000*I (Source, 4) -
                          100_000*I (Source, 5) - 10_000*I (Source, 6) -
                          1_000*I (Source, 7) - 100*I (Source, 8) -
                          10*I (Source, 9) - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 3) > '4' then
                        Has_Failed := True;
                     elsif Source (Source'First + 3) < '4' then
                        Target := -2_100_000_000
                          - 10_000_000*I (Source, 3)
                          - 1_000_000*I (Source, 4)
                          - 100_000*I (Source, 5)
                          - 10_000*I (Source, 6)
                          - 1_000*I (Source, 7)
                          - 100*I (Source, 8)
                          - 10*I (Source, 9)
                          - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 4) > '7' then
                        Has_Failed := True;
                     elsif Source (Source'First + 4) < '7' then
                        Target := -2_140_000_000 - 1_000_000*I (Source, 4) -
                          100_000*I (Source, 5) - 10_000*I (Source, 6) -
                          1_000*I (Source, 7) - 100*I (Source, 8) -
                          10*I (Source, 9) - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 5) > '4' then
                        Has_Failed := True;
                     elsif Source (Source'First + 5) < '4' then
                        Target := -2_147_000_000 - 100_000*I (Source, 5) -
                          10_000*I (Source, 6) -
                          1_000*I (Source, 7) - 100*I (Source, 8) -
                          10*I (Source, 9) - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 6) > '8' then
                        Has_Failed := True;
                     elsif Source (Source'First + 6) < '8' then
                        Target := -2_147_400_000
                          - 10_000*I (Source, 6)
                          - 1_000*I (Source, 7)
                          - 100*I (Source, 8)
                          - 10*I (Source, 9)
                          - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 7) > '3' then
                        Has_Failed := True;
                     elsif Source (Source'First + 7) < '3' then
                        Target := -2_147_480_000 - 1_000*I (Source, 7) -
                          100*I (Source, 8) -
                          10*I (Source, 9) - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 8) > '6' then
                        Has_Failed := True;
                     elsif Source (Source'First + 8) < '6' then
                        Target := -2_147_483_000 - 100*I (Source, 8) -
                          10*I (Source, 9) - I (Source, 10);
                        Has_Failed := False;
                     elsif Source (Source'First + 9) > '4' then
                        Has_Failed := True;
                     elsif Source (Source'First + 9) < '4' then
                        Target := -2_147_483_600 - 10*I (Source, 9) - I (Source, 10);
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
                        Target := - I (Source, 1);
                        Has_Failed := False;
                     when 3 =>
                        Target := -10*I (Source, 1) - I (Source, 2);
                        Has_Failed := False;
                     when 4 =>
                        Target := -100*I (Source, 1)
                          - 10*I (Source, 2)
                          - I (Source, 3);
                        Has_Failed := False;
                     when 5 =>
                        Target := -1_000*I (Source, 1)
                          - 100*I (Source, 2)
                          - 10*I (Source, 3)
                          - I (Source, 4);
                        Has_Failed := False;
                     when 6 =>
                        Target := -10_000*I (Source, 1)
                          - 1_000*I (Source, 2)
                          - 100*I (Source, 3)
                          - 10*I (Source, 4)
                          - I (Source, 5);
                        Has_Failed := False;
                     when 7 =>
                        Target := -100_000*I (Source, 1)
                          - 10_000*I (Source, 2)
                          - 1_000*I (Source, 3)
                          - 100*I (Source, 4)
                          - 10*I (Source, 5)
                          - I (Source, 6);
                        Has_Failed := False;
                     when 8 =>
                        Target := -1_000_000*I (Source, 1)
                          - 100_000*I (Source, 2)
                          - 10_000*I (Source, 3)
                          - 1_000*I (Source, 4)
                          - 100*I (Source, 5)
                          - 10*I (Source, 6)
                          - I (Source, 7);
                        Has_Failed := False;
                     when 9 =>
                        Target := -10_000_000*I (Source, 1)
                          - 1000_000*I (Source, 2)
                          - 100_000*I (Source, 3)
                          - 10_000*I (Source, 4)
                          - 1_000*I (Source, 5)
                          - 100*I (Source, 6)
                          - 10*I (Source, 7)
                          - I (Source, 8);
                        Has_Failed := False;
                     when 10 =>
                        Target := -100_000_000*I (Source, 1)
                          - 10_000_000*I (Source, 2)
                          - 1_000_000*I (Source, 3)
                          - 100_000*I (Source, 4)
                          - 10_000*I (Source, 5)
                          - 1_000*I (Source, 6)
                          - 100*I (Source, 7)
                          - 10*I (Source, 8)
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
            elsif (for all I in Source'Range => Aida.Character.Is_Digit (Source(I))) then
               Target := 0;

               if Source'Length = 10 then
                  if Source (Source'First) > '2' then
                     Has_Failed := True;
                  elsif Source (Source'First) < '2' then
                     Target := 1_000_000_000*I (Source, 0) + 100_000_000*I (Source, 1) + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 1) > '1' then
                     Has_Failed := True;
                  elsif Source (Source'First + 1) < '1' then
                     Target := 2_000_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 2) > '4' then
                     Has_Failed := True;
                  elsif Source (Source'First + 2) < '4' then
                     Target := 2_100_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 3) > '7' then
                     Has_Failed := True;
                  elsif Source (Source'First + 3) < '7' then
                     Target := 2_140_000_000 + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 4) > '4' then
                     Has_Failed := True;
                  elsif Source (Source'First + 4) < '4' then
                     Target := 2_147_000_000 + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 5) > '8' then
                     Has_Failed := True;
                  elsif Source (Source'First + 5) < '8' then
                     Target := 2_147_400_000 + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 6) > '3' then
                     Has_Failed := True;
                  elsif Source (Source'First + 6) < '3' then
                     Target := 2_147_480_000 + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 7) > '6' then
                     Has_Failed := True;
                  elsif Source (Source'First + 7) < '6' then
                     Target := 2_147_483_000 + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
                     Has_Failed := False;
                  elsif Source (Source'First + 8) > '4' then
                     Has_Failed := True;
                  elsif Source (Source'First + 8) < '4' then
                     Target := 2_147_483_600 + 10*I (Source, 8) + I (Source, 9);
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
      end To_Int32;

      function To_Int32 (Source : String_T) return Int32_T is
         Target : Int32_T;
      begin
         if Source (Source'First) = '-' then

            if Source'Length = 11 then
               if Source (Source'First + 1) < '2' then
                  Target := -1_000_000_000*I (Source, 1) - 100_000_000*I (Source, 2) -
                    10_000_000*I (Source, 3) -
                    1_000_000*I (Source, 4) -
                    100_000*I (Source, 5) - 10_000*I (Source, 6) -
                    1_000*I (Source, 7) - 100*I (Source, 8) -
                    10*I (Source, 9) - I (Source, 10);
               elsif Source (Source'First + 2) < '1' then
                  Target := -2_000_000_000 -
                    10_000_000*I (Source, 3) -
                    1_000_000*I (Source, 4) -
                    100_000*I (Source, 5) - 10_000*I (Source, 6) -
                    1_000*I (Source, 7) - 100*I (Source, 8) -
                    10*I (Source, 9) - I (Source, 10);
               elsif Source (Source'First + 3) < '4' then
                  Target := -2_100_000_000
                    - 10_000_000*I (Source, 3)
                    - 1_000_000*I (Source, 4)
                    - 100_000*I (Source, 5)
                    - 10_000*I (Source, 6)
                    - 1_000*I (Source, 7)
                    - 100*I (Source, 8)
                    - 10*I (Source, 9)
                    - I (Source, 10);
               elsif Source (Source'First + 4) < '7' then
                  Target := -2_140_000_000 - 1_000_000*I (Source, 4) -
                    100_000*I (Source, 5) - 10_000*I (Source, 6) -
                    1_000*I (Source, 7) - 100*I (Source, 8) -
                    10*I (Source, 9) - I (Source, 10);
               elsif Source (Source'First + 5) < '4' then
                  Target := -2_147_000_000 - 100_000*I (Source, 5) -
                    10_000*I (Source, 6) -
                    1_000*I (Source, 7) - 100*I (Source, 8) -
                    10*I (Source, 9) - I (Source, 10);
               elsif Source (Source'First + 6) < '8' then
                  Target := -2_147_400_000
                    - 10_000*I (Source, 6)
                    - 1_000*I (Source, 7)
                    - 100*I (Source, 8)
                    - 10*I (Source, 9)
                    - I (Source, 10);
               elsif Source (Source'First + 7) < '3' then
                  Target := -2_147_480_000 - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10);
               elsif Source (Source'First + 8) < '6' then
                  Target := -2_147_483_000 - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10);
               elsif Source (Source'First + 9) < '4' then
                  Target := -2_147_483_600 - 10*I (Source, 9) - I (Source, 10);
               else
                  Target := -2_147_483_640 - I (Source, 10);
               end if;
            else
               case Source'Length is
               when 2 =>
                  Target := - I (Source, 1);
               when 3 =>
                  Target := -10*I (Source, 1) - I (Source, 2);
               when 4 =>
                  Target := -100*I (Source, 1)
                    - 10*I (Source, 2)
                    - I (Source, 3);
               when 5 =>
                  Target := -1_000*I (Source, 1)
                    - 100*I (Source, 2)
                    - 10*I (Source, 3)
                    - I (Source, 4);
               when 6 =>
                  Target := -10_000*I (Source, 1)
                    - 1_000*I (Source, 2)
                    - 100*I (Source, 3)
                    - 10*I (Source, 4)
                    - I (Source, 5);
               when 7 =>
                  Target := -100_000*I (Source, 1)
                    - 10_000*I (Source, 2)
                    - 1_000*I (Source, 3)
                    - 100*I (Source, 4)
                    - 10*I (Source, 5)
                    - I (Source, 6);
               when 8 =>
                  Target := -1_000_000*I (Source, 1)
                    - 100_000*I (Source, 2)
                    - 10_000*I (Source, 3)
                    - 1_000*I (Source, 4)
                    - 100*I (Source, 5)
                    - 10*I (Source, 6)
                    - I (Source, 7);
               when 9 =>
                  Target := -10_000_000*I (Source, 1)
                    - 1000_000*I (Source, 2)
                    - 100_000*I (Source, 3)
                    - 10_000*I (Source, 4)
                    - 1_000*I (Source, 5)
                    - 100*I (Source, 6)
                    - 10*I (Source, 7)
                    - I (Source, 8);
               when 10 =>
                  Target := -100_000_000*I (Source, 1)
                    - 10_000_000*I (Source, 2)
                    - 1_000_000*I (Source, 3)
                    - 100_000*I (Source, 4)
                    - 10_000*I (Source, 5)
                    - 1_000*I (Source, 6)
                    - 100*I (Source, 7)
                    - 10*I (Source, 8)
                    - I (Source, 9);
               when others =>
                  Target := 0;
               end case;
            end if;
         else
            if Source'Length = 10 then
               if Source (Source'First) < '2' then
                  Target := 1_000_000_000*I (Source, 0) + 100_000_000*I (Source, 1) + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3)
                    + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 1) < '1' then
                  Target := 2_000_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 2) < '4' then
                  Target := 2_100_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 3) < '7' then
                  Target := 2_140_000_000 + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 4) < '4' then
                  Target := 2_147_000_000 + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 5) < '8' then
                  Target := 2_147_400_000 + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 6) < '3' then
                  Target := 2_147_480_000 + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 7) < '6' then
                  Target := 2_147_483_000 + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9);
               elsif Source (Source'First + 8) < '4' then
                  Target := 2_147_483_600 + 10*I (Source, 8) + I (Source, 9);
               else
                  Target := 2_147_483_640 + I (Source, 9);
               end if;
            else
               case Source'Length is
               when 1 =>
                  Target := I (Source, 0);
               when 2 =>
                  Target := I (Source, 0) * 10 + I (Source, 1);
               when 3 =>
                  Target :=
                    I (Source, 0) * 100 +
                    I (Source, 1) * 10 +
                    I (Source, 2) * 1;
               when 4 =>
                  Target :=
                    I (Source, 0) * 1_000 +
                    I (Source, 1) * 100 +
                    I (Source, 2) * 10 +
                    I (Source, 3) * 1;
               when 5 =>
                  Target :=
                    I (Source, 0) * 10_000 +
                    I (Source, 1) * 1_000 +
                    I (Source, 2) * 100 +
                    I (Source, 3) * 10 +
                    I (Source, 4) * 1;
               when 6 =>
                  Target :=
                    I (Source, 0) * 100_000 +
                    I (Source, 1) * 10_000 +
                    I (Source, 2) * 1_000 +
                    I (Source, 3) * 100 +
                    I (Source, 4) * 10 +
                    I (Source, 5) * 1;
               when 7 =>
                  Target := I (Source, 0) * 1_000_000 +
                    I (Source, 1) * 100_000 +
                    I (Source, 2) * 10_000 +
                    I (Source, 3) * 1_000 +
                    I (Source, 4) * 100 +
                    I (Source, 5) * 10 +
                    I (Source, 6) * 1;
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
               when others =>
                  Target := 0;
               end case;
            end if;
         end if;

         return Target;
      end To_Int32;

      procedure To_Float (Source     : in  String_T;
                          Target     : out Float_T;
                          Has_Failed : out Boolean)
      is
         pragma SPARK_Mode (Off);
      begin
         Target := Float_T'Value (Source);
         Has_Failed := False;
      exception
         when Constraint_Error =>
            Has_Failed := True;
      end To_Float;

      function Is_Latin1_Graphic_Characters (Text : String_T) return Boolean is
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

      function Starts_With (This         : String_T;
                            Searched_For : String_T) return Boolean
      is
         Result : Boolean;
      begin
         if Searched_For'Length > This'Length then
            Result := False;
         else
            Result := (for all Index in Searched_For'Range => This (Index - Searched_For'First + This'First) = Searched_For (Index));
         end if;

         return Result;
      end Starts_With;

      function Hash32 (This : String_T) return Hash32_T is
         H : Hash32_T := 0;
         A : Hash32_T := 31_415;
         B : constant Hash32_T := 27_183;
      begin
         for I in Pos32_T range This'First..This'Last loop
            H := A*H + Standard.Character'Pos (This (I));
            A := A*B;
            pragma Loop_Variant (Increases => I);
         end loop;

         return H;
      end Hash32;

      function Concat (Left, Right : String_T) return String_T is
         S : String_T (1..Left'Length + Right'Length) := (others => ' ');
      begin
         S (1..Left'Length) := Left (Left'First..Left'Last);
         S (1 + Left'Length..Left'Length + Right'Length) := Right (Right'First..Right'Last);
         return S;
      end Concat;

   end String;

end Aida;
