with Aida_Z.Character;
with Aida_Z.Int32;
with Aida_Z.Float;
with Aida_Z.String;
with Aida_Z.Hash32;

package Aida with SPARK_Mode, Pure is

   type Character_T is new Aida_Z.Character.T;

   type Int32_T is new Aida_Z.Int32.T;

   subtype Pos32_T is Int32_T range 1 .. Int32_T'Last;

   subtype Nat32_T is Int32_T range 0 .. Int32_T'Last;

   type Float_T is new Aida_Z.Zzz_Float_T;

   type String_T is new Aida_Z.String.T;

   type Hash32_T is new Aida_Z.Hash32.T;

   function Hash32 (This : Int32_T) return Hash32_T with
     Global => null;

   function Hash32 (This : String_T) return Hash32_T with
     Global => null;

   function To_String (This : Int32_T) return String_T with
     Global => null,
     Post   =>  (if This < 0 then
                   To_String'Result'Length >= 2 and To_String'Result'Length <= 11 else
                     To_String'Result'Length >= 1 and To_String'Result'Length <= 10);

   function To_String (This : Float_T) return String_T with
     Global => null,
     Post   => To_String'Result'Length >= 1 and To_String'Result'Length <= 11;

   function I (Source : String_T;
               Index  : Natural) return Int32_T is (Int32_T (To_Int32 (Character_T (Source (Source'First + Index))))) with
   Ghost => True,
   Pre   => Index < Source'Length and then Source'First + Index <= Source'Last and then Is_Digit (Character_T (Source (Source'First + Index)));

   procedure To_Int32 (Source     : in  String_T;
                       Target     : out Int32_T;
                       Has_Failed : out Boolean) with
     Global         => null,
     Contract_Cases => (Source'Length = 0 => Has_Failed,
                        Source'Length = 1 => (if Source(Source'First) = '-' then
                                                Has_Failed
                                                  elsif Is_Digit (Character_T (Source (Source'First))) then
                                                (Has_Failed = False and Target = I (Source, 0))
                                                else
                                                    Has_Failed),
                        Source'Length = 2 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (Has_Failed = False and
                                                   Target = 10*I (Source, 0) + I (Source, 1))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -I (Source, 1))
                                                else
                                                    Has_Failed),
                        Source'Length = 3 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                               (Has_Failed = False and
                                                   Target = 100*I (Source, 0) + 10*I (Source, 1) + I (Source, 2))
                                                elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -10*I (Source, 1) - I (Source, 2))
                                                else
                                                    Has_Failed),
                        Source'Length = 4 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (Has_Failed = False and
                                                   Target = 1_000*I (Source, 0) + 100*I (Source, 1) + 10*I (Source, 2) + I (Source, 3))
                                                elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -100*I (Source, 1) - 10*I (Source, 2) - I (Source, 3))
                                                  else
                                                    Has_Failed),
                        Source'Length = 5 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (Has_Failed = False and
                                                   Target = 10_000*I (Source, 0) + 1_000*I (Source, 1) + 100*I (Source, 2) + 10*I (Source, 3) + I (Source, 4))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -1_000*I (Source, 1) - 100*I (Source, 2) - 10*I (Source, 3) - I (Source, 4))
                                                  else
                                                    Has_Failed),
                        Source'Length = 6 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (Has_Failed = False and
                                                   Target = 100_000*I (Source, 0) + 10_000*I (Source, 1) + 1_000*I (Source, 2) + 100*I (Source, 3) + 10*I (Source, 4) + I (Source, 5))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -10_000*I (Source, 1) - 1_000*I (Source, 2) - 100*I (Source, 3) - 10*I (Source, 4) - I (Source, 5))
                                                else
                                                  Has_Failed),
                        Source'Length = 7 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (Has_Failed = False and
                                                   Target = 1_000_000*I (Source, 0) + 100_000*I (Source, 1) + 10_000*I (Source, 2) + 1_000*I (Source, 3) + 100*I (Source, 4) + 10*I (Source, 5) + I (Source, 6))
                                                elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -100_000*I (Source, 1) - 10_000*I (Source, 2) - 1_000*I (Source, 3) - 100*I (Source, 4) - 10*I (Source, 5) - I (Source, 6))
                                                else
                                                  Has_Failed),
                        Source'Length = 8 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (Has_Failed = False and
                                                   Target = 10_000_000*I (Source, 0) + 1_000_000*I (Source, 1) + 100_000*I (Source, 2) + 10_000*I (Source, 3) + 1_000*I (Source, 4) + 100*I (Source, 5) + 10*I (Source, 6) + I (Source, 7))
                                                elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -1_000_000*I (Source, 1) - 100_000*I (Source, 2) - 10_000*I (Source, 3) - 1_000*I (Source, 4) - 100*I (Source, 5) - 10*I (Source, 6) - I (Source, 7))
                                                  else
                                                    Has_Failed),
                        Source'Length = 9 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (Has_Failed = False and
                                                   Target = 100_000_000*I (Source, 0) + 10_000_000*I (Source, 1) + 1_000_000*I (Source, 2) + 100_000*I (Source, 3) + 10_000*I (Source, 4) + 1_000*I (Source, 5) + 100*I (Source, 6) + 10*I (Source, 7) + I (Source, 8))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (Has_Failed = False and Target = -10_000_000*I (Source, 1) - 1_000_000*I (Source, 2) - 100_000*I (Source, 3) - 10_000*I (Source, 4) - 1_000*I (Source, 5) - 100*I (Source, 6) - 10*I (Source, 7) - I (Source, 8))
                                                  else
                                                    Has_Failed),
                        Source'Length = 10 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                 (if (Source(Source'First + 0) = '2' and
                                                    Source(Source'First + 1) = '1' and
                                                    Source(Source'First + 2) = '4' and
                                                    Source(Source'First + 3) = '7' and
                                                    Source(Source'First + 4) = '4' and
                                                    Source(Source'First + 5) = '8' and
                                                    Source(Source'First + 6) = '3' and
                                                    Source(Source'First + 7) = '6' and
                                                    Source(Source'First + 8) = '4' and
                                                    Source(Source'First + 9) < '8') then
                                                    (Has_Failed = False and
                                                     Target = 2_147_483_640 + I (Source, 9))
                                                    elsif (Source(Source'First + 0) = '2' and
                                                         Source(Source'First + 1) = '1' and
                                                         Source(Source'First + 2) = '4' and
                                                         Source(Source'First + 3) = '7' and
                                                         Source(Source'First + 4) = '4' and
                                                         Source(Source'First + 5) = '8' and
                                                         Source(Source'First + 6) = '3' and
                                                         Source(Source'First + 7) = '6' and
                                                          Source(Source'First + 8) < '4') then
                                                    (Has_Failed = False and
                                                           Target = 2_147_483_600 + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                             Source(Source'First + 1) = '1' and
                                                             Source(Source'First + 2) = '4' and
                                                             Source(Source'First + 3) = '7' and
                                                             Source(Source'First + 4) = '4' and
                                                             Source(Source'First + 5) = '8' and
                                                             Source(Source'First + 6) = '3' and
                                                      Source(Source'First + 7) < '6') then
                                                    (Has_Failed = False and
                                                         Target = 2_147_483_000 + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                    elsif (Source(Source'First + 0) = '2' and
                                                         Source(Source'First + 1) = '1' and
                                                         Source(Source'First + 2) = '4' and
                                                         Source(Source'First + 3) = '7' and
                                                         Source(Source'First + 4) = '4' and
                                                         Source(Source'First + 5) = '8' and
                                                      Source(Source'First + 6) < '3') then
                                                    (Has_Failed = False and
                                                         Target = 2_147_480_000 + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                    elsif (Source(Source'First + 0) = '2' and
                                                             Source(Source'First + 1) = '1' and
                                                             Source(Source'First + 2) = '4' and
                                                             Source(Source'First + 3) = '7' and
                                                             Source(Source'First + 4) = '4' and
                                                          Source(Source'First + 5) < '8') then
                                                    (Has_Failed = False and
                                                         Target = 2_147_400_000 + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                         Source(Source'First + 1) = '1' and
                                                         Source(Source'First + 2) = '4' and
                                                         Source(Source'First + 3) = '7' and
                                                          Source(Source'First + 4) < '4') then
                                                    (Has_Failed = False and
                                                         Target = 2_147_000_000 + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                             Source(Source'First + 1) = '1' and
                                                             Source(Source'First + 2) = '4' and
                                                      Source(Source'First + 3) < '7') then
                                                    (Has_Failed = False and
                                                         Target = 2_140_000_000 + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                    elsif (Source(Source'First + 0) = '2' and
                                                         Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) < '4') then
                                                    (Has_Failed = False and
                                                         Target = 2_100_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) < '1') then
                                                    (Has_Failed = False and
                                                         Target = 2_000_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                        elsif (Source(Source'First + 0) < '2') then
                                                 (Has_Failed = False and
                                                    Target = 1_000_000_000*I (Source, 0) + 100_000_000*I (Source, 1) + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                   else
                                                      Has_Failed)
                                                   elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                   (Has_Failed = False and
                                                      Target = -100_000_000*I (Source, 1) - 10_000_000*I (Source, 2) - 1_000_000*I (Source, 3) - 100_000*I (Source, 4) - 10_000*I (Source, 5) - 1_000*I (Source, 6) - 100*I (Source, 7) - 10*I (Source, 8) - I (Source, 9))
                                                     else
                                              Has_Failed),
                        Source'Length = 11 => (if ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                 (if (Source(Source'First + 1) = '2' and
                                                    Source(Source'First + 2) = '1' and
                                                    Source(Source'First + 3) = '4' and
                                                    Source(Source'First + 4) = '7' and
                                                    Source(Source'First + 5) = '4' and
                                                    Source(Source'First + 6) = '8' and
                                                    Source(Source'First + 7) = '3' and
                                                    Source(Source'First + 8) = '6' and
                                                    Source(Source'First + 9) = '4' and
                                                    Source(Source'First + 10) <= '8') then
                                                    (Has_Failed = False and
                                                         Target = -2_147_483_640 - I (Source, 10))
                                                    elsif (Source(Source'First + 1) = '2' and
                                                          Source(Source'First + 2) = '1' and
                                                          Source(Source'First + 3) = '4' and
                                                          Source(Source'First + 4) = '7' and
                                                          Source(Source'First + 5) = '4' and
                                                          Source(Source'First + 6) = '8' and
                                                          Source(Source'First + 7) = '3' and
                                                          Source(Source'First + 8) = '6' and
                                                          Source(Source'First + 9) < '4') then
                                                      (Has_Failed = False and
                                                           Target = -2_147_483_600 - 10*I (Source, 9) - I (Source, 10))
                                                    elsif (Source(Source'First + 1) = '2' and
                                                          Source(Source'First + 2) = '1' and
                                                          Source(Source'First + 3) = '4' and
                                                          Source(Source'First + 4) = '7' and
                                                          Source(Source'First + 5) = '4' and
                                                          Source(Source'First + 6) = '8' and
                                                          Source(Source'First + 7) = '3' and
                                                          Source(Source'First + 8) < '6') then
                                                      (Has_Failed = False and
                                                           Target = -2_147_483_000 - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) = '4' and
                                                      Source(Source'First + 4) = '7' and
                                                      Source(Source'First + 5) = '4' and
                                                      Source(Source'First + 6) = '8' and
                                                      Source(Source'First + 7) < '3') then
                                                    (Has_Failed = False and
                                                         Target = -2_147_480_000 - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif
                                                    (Source(Source'First + 1) = '2' and
                                                         Source(Source'First + 2) = '1' and
                                                         Source(Source'First + 3) = '4' and
                                                         Source(Source'First + 4) = '7' and
                                                         Source(Source'First + 5) = '4' and
                                                         Source(Source'First + 6) < '8') then
                                                      (Has_Failed = False and
                                                           Target = -2_147_400_000 - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                    elsif (Source(Source'First + 1) = '2' and
                                                          Source(Source'First + 2) = '1' and
                                                          Source(Source'First + 3) = '4' and
                                                          Source(Source'First + 4) = '7' and
                                                          Source(Source'First + 5) < '4') then
                                                      (Has_Failed = False and
                                                           Target = -2_147_000_000 - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                    elsif (Source(Source'First + 1) = '2' and
                                                          Source(Source'First + 2) = '1' and
                                                          Source(Source'First + 3) = '4' and
                                                          Source(Source'First + 4) < '7') then
                                                      (Has_Failed = False and
                                                           Target = -2_140_000_000 - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) < '4') then
                                                    (Has_Failed = False and
                                                         Target = -2_100_000_000 - 10_000_000*I (Source, 3) - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) < '1') then
                                                    (Has_Failed = False and
                                                         Target = -2_000_000_000 - 100_000_000*I (Source, 2) - 10_000_000*I (Source, 3) - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                    elsif (Source(Source'First + 1) < '2') then
                                                      (Has_Failed = False and
                                                           Target = -1_000_000_000*I (Source, 1) - 100_000_000*I (Source, 2) - 10_000_000*I (Source, 3) - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                    else
                                                      Has_Failed)
                                                     else
                                                       Has_Failed),
                        Source'Length >= 12 => Has_Failed);

   function To_Int32 (Source : String_T) return Int32_T with
     Global => null,
     Pre => ((Source'Length >= 1 and Source'Length <= 11) and then (
                 if Source'Length = 1 then
                   Is_Digit (Character_T (Source (Source'First)))
               elsif (Source'Length >= 2 and Source'Length <= 9) then
                 ((for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) or
                    ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))))
               elsif Source'Length = 10 then
                 (((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) or
                    ((for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) and then
                         ((Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) = '4' and
                              Source(Source'First + 3) = '7' and
                              Source(Source'First + 4) = '4' and
                              Source(Source'First + 5) = '8' and
                              Source(Source'First + 6) = '3' and
                              Source(Source'First + 7) = '6' and
                              Source(Source'First + 8) = '4' and
                              Source(Source'First + 9) < '8') or
                             (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) = '4' and
                              Source(Source'First + 3) = '7' and
                              Source(Source'First + 4) = '4' and
                              Source(Source'First + 5) = '8' and
                              Source(Source'First + 6) = '3' and
                              Source(Source'First + 7) = '6' and
                                     Source(Source'First + 8) < '4') or
                             (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) = '4' and
                              Source(Source'First + 3) = '7' and
                              Source(Source'First + 4) = '4' and
                              Source(Source'First + 5) = '8' and
                              Source(Source'First + 6) = '3' and
                                 Source(Source'First + 7) < '6') or
                              (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) = '4' and
                              Source(Source'First + 3) = '7' and
                              Source(Source'First + 4) = '4' and
                              Source(Source'First + 5) = '8' and
                              Source(Source'First + 6) < '3') or
                              (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) = '4' and
                              Source(Source'First + 3) = '7' and
                              Source(Source'First + 4) = '4' and
                              Source(Source'First + 5) < '8') or
                             (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) = '4' and
                              Source(Source'First + 3) = '7' and
                              Source(Source'First + 4) < '4') or
                             (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) = '4' and
                              Source(Source'First + 3) < '7') or
                             (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) = '1' and
                              Source(Source'First + 2) < '4') or
                             (Source(Source'First + 0) = '2' and
                              Source(Source'First + 1) < '1') or
                              (Source(Source'First + 0) < '2'))))
                   elsif Source'Length = 11 then
               (((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) and then
                    ((Source(Source'First + 1) = '2' and
                         Source(Source'First + 2) = '1' and
                         Source(Source'First + 3) = '4' and
                         Source(Source'First + 4) = '7' and
                         Source(Source'First + 5) = '4' and
                         Source(Source'First + 6) = '8' and
                         Source(Source'First + 7) = '3' and
                         Source(Source'First + 8) = '6' and
                         Source(Source'First + 9) = '4' and
                         Source(Source'First + 10) <= '8') or
                         (Source(Source'First + 1) = '2' and
                                Source(Source'First + 2) = '1' and
                                Source(Source'First + 3) = '4' and
                                Source(Source'First + 4) = '7' and
                                Source(Source'First + 5) = '4' and
                                Source(Source'First + 6) = '8' and
                                Source(Source'First + 7) = '3' and
                                Source(Source'First + 8) = '6' and
                                Source(Source'First + 9) < '4') or
                       (Source(Source'First + 1) = '2' and
                            Source(Source'First + 2) = '1' and
                            Source(Source'First + 3) = '4' and
                            Source(Source'First + 4) = '7' and
                            Source(Source'First + 5) = '4' and
                            Source(Source'First + 6) = '8' and
                            Source(Source'First + 7) = '3' and
                            Source(Source'First + 8) < '6') or
                         (Source(Source'First + 1) = '2' and
                                Source(Source'First + 2) = '1' and
                                Source(Source'First + 3) = '4' and
                                Source(Source'First + 4) = '7' and
                                Source(Source'First + 5) = '4' and
                                Source(Source'First + 6) = '8' and
                                Source(Source'First + 7) < '3') or
                       (Source(Source'First + 1) = '2' and
                            Source(Source'First + 2) = '1' and
                            Source(Source'First + 3) = '4' and
                            Source(Source'First + 4) = '7' and
                            Source(Source'First + 5) = '4' and
                            Source(Source'First + 6) < '8') or
                         (Source(Source'First + 1) = '2' and
                                Source(Source'First + 2) = '1' and
                                Source(Source'First + 3) = '4' and
                                Source(Source'First + 4) = '7' and
                                Source(Source'First + 5) < '4') or
                       (Source(Source'First + 1) = '2' and
                            Source(Source'First + 2) = '1' and
                            Source(Source'First + 3) = '4' and
                            Source(Source'First + 4) < '7') or
                         (Source(Source'First + 1) = '2' and
                                Source(Source'First + 2) = '1' and
                                Source(Source'First + 3) < '4') or
                       (Source(Source'First + 1) = '2' and
                            Source(Source'First + 2) < '1') or
                         (Source(Source'First + 1) < '2'))))),
     Contract_Cases => (Source'Length = 1 => To_Int32'Result = I (Source, 0),
                        Source'Length = 2 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 10*I (Source, 0) + I (Source, 1))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -I (Source, 1))),
                        Source'Length = 3 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 100*I (Source, 0) + 10*I (Source, 1) + I (Source, 2))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -10*I (Source, 1) - I (Source, 2))),
                        Source'Length = 4 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 1_000*I (Source, 0) + 100*I (Source, 1) + 10*I (Source, 2) + I (Source, 3))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -100*I (Source, 1) - 10*I (Source, 2) - I (Source, 3))),
                        Source'Length = 5 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 10_000*I (Source, 0) + 1_000*I (Source, 1) + 100*I (Source, 2) + 10*I (Source, 3) + I (Source, 4))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -1_000*I (Source, 1) - 100*I (Source, 2) - 10*I (Source, 3) - I (Source, 4))),
                        Source'Length = 6 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 100_000*I (Source, 0) + 10_000*I (Source, 1) + 1_000*I (Source, 2) + 100*I (Source, 3) + 10*I (Source, 4) + I (Source, 5))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -10_000*I (Source, 1) - 1_000*I (Source, 2) - 100*I (Source, 3) - 10*I (Source, 4) - I (Source, 5))),
                        Source'Length = 7 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 1_000_000*I (Source, 0) + 100_000*I (Source, 1) + 10_000*I (Source, 2) + 1_000*I (Source, 3) + 100*I (Source, 4) + 10*I (Source, 5) + I (Source, 6))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -100_000*I (Source, 1) - 10_000*I (Source, 2) - 1_000*I (Source, 3) - 100*I (Source, 4) - 10*I (Source, 5) - I (Source, 6))),
                        Source'Length = 8 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 10_000_000*I (Source, 0) + 1_000_000*I (Source, 1) + 100_000*I (Source, 2) + 10_000*I (Source, 3) + 1_000*I (Source, 4) + 100*I (Source, 5) + 10*I (Source, 6) + I (Source, 7))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -1_000_000*I (Source, 1) - 100_000*I (Source, 2) - 10_000*I (Source, 3) - 1_000*I (Source, 4) - 100*I (Source, 5) - 10*I (Source, 6) - I (Source, 7))),
                        Source'Length = 9 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                (To_Int32'Result = 100_000_000*I (Source, 0) + 10_000_000*I (Source, 1) + 1_000_000*I (Source, 2) + 100_000*I (Source, 3) + 10_000*I (Source, 4) + 1_000*I (Source, 5) + 100*I (Source, 6) + 10*I (Source, 7) + I (Source, 8))
                                                  elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                (To_Int32'Result = -10_000_000*I (Source, 1) - 1_000_000*I (Source, 2) - 100_000*I (Source, 3) - 10_000*I (Source, 4) - 1_000*I (Source, 5) - 100*I (Source, 6) - 10*I (Source, 7) - I (Source, 8))),
                        Source'Length = 10 => (if (for all Index in Source'Range => Is_Digit (Character_T (Source (Index)))) then
                                                 (if (Source(Source'First + 0) = '2' and
                                                    Source(Source'First + 1) = '1' and
                                                    Source(Source'First + 2) = '4' and
                                                    Source(Source'First + 3) = '7' and
                                                    Source(Source'First + 4) = '4' and
                                                    Source(Source'First + 5) = '8' and
                                                    Source(Source'First + 6) = '3' and
                                                    Source(Source'First + 7) = '6' and
                                                    Source(Source'First + 8) = '4' and
                                                    Source(Source'First + 9) < '8') then
                                                    (To_Int32'Result = 2_147_483_640 + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) = '4' and
                                                      Source(Source'First + 3) = '7' and
                                                      Source(Source'First + 4) = '4' and
                                                      Source(Source'First + 5) = '8' and
                                                      Source(Source'First + 6) = '3' and
                                                      Source(Source'First + 7) = '6' and
                                                      Source(Source'First + 8) < '4') then
                                                    (To_Int32'Result = 2_147_483_600 + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) = '4' and
                                                      Source(Source'First + 3) = '7' and
                                                      Source(Source'First + 4) = '4' and
                                                      Source(Source'First + 5) = '8' and
                                                      Source(Source'First + 6) = '3' and
                                                      Source(Source'First + 7) < '6') then
                                                    (To_Int32'Result = 2_147_483_000 + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) = '4' and
                                                      Source(Source'First + 3) = '7' and
                                                      Source(Source'First + 4) = '4' and
                                                      Source(Source'First + 5) = '8' and
                                                      Source(Source'First + 6) < '3') then
                                                    (To_Int32'Result = 2_147_480_000 + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) = '4' and
                                                      Source(Source'First + 3) = '7' and
                                                      Source(Source'First + 4) = '4' and
                                                      Source(Source'First + 5) < '8') then
                                                    (To_Int32'Result = 2_147_400_000 + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) = '4' and
                                                      Source(Source'First + 3) = '7' and
                                                      Source(Source'First + 4) < '4') then
                                                    (To_Int32'Result = 2_147_000_000 + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) = '4' and
                                                      Source(Source'First + 3) < '7') then
                                                    (To_Int32'Result = 2_140_000_000 + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) = '1' and
                                                      Source(Source'First + 2) < '4') then
                                                    (To_Int32'Result = 2_100_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) = '2' and
                                                      Source(Source'First + 1) < '1') then
                                                    (To_Int32'Result = 2_000_000_000 + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9))
                                                      elsif (Source(Source'First + 0) < '2') then
                                                    (To_Int32'Result = 1_000_000_000*I (Source, 0) + 100_000_000*I (Source, 1) + 10_000_000*I (Source, 2) + 1_000_000*I (Source, 3) + 100_000*I (Source, 4) + 10_000*I (Source, 5) + 1_000*I (Source, 6) + 100*I (Source, 7) + 10*I (Source, 8) + I (Source, 9)))
                                                       elsif ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                 (To_Int32'Result = -100_000_000*I (Source, 1) - 10_000_000*I (Source, 2) - 1_000_000*I (Source, 3) - 100_000*I (Source, 4) - 10_000*I (Source, 5) - 1_000*I (Source, 6) - 100*I (Source, 7) - 10*I (Source, 8) - I (Source, 9))),
                        Source'Length = 11 => (if ((Source (Source'First) = '-') and (for all Index in Integer range (Source'First + 1) .. Source'Last => Is_Digit (Character_T (Source (Index))))) then
                                                 (if (Source(Source'First + 1) = '2' and
                                                    Source(Source'First + 2) = '1' and
                                                    Source(Source'First + 3) = '4' and
                                                    Source(Source'First + 4) = '7' and
                                                    Source(Source'First + 5) = '4' and
                                                    Source(Source'First + 6) = '8' and
                                                    Source(Source'First + 7) = '3' and
                                                    Source(Source'First + 8) = '6' and
                                                    Source(Source'First + 9) = '4' and
                                                    Source(Source'First + 10) <= '8') then
                                                    (To_Int32'Result = -2_147_483_640 - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) = '4' and
                                                      Source(Source'First + 4) = '7' and
                                                      Source(Source'First + 5) = '4' and
                                                      Source(Source'First + 6) = '8' and
                                                      Source(Source'First + 7) = '3' and
                                                      Source(Source'First + 8) = '6' and
                                                      Source(Source'First + 9) < '4') then
                                                    (To_Int32'Result = -2_147_483_600 - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) = '4' and
                                                      Source(Source'First + 4) = '7' and
                                                      Source(Source'First + 5) = '4' and
                                                      Source(Source'First + 6) = '8' and
                                                      Source(Source'First + 7) = '3' and
                                                      Source(Source'First + 8) < '6') then
                                                    (To_Int32'Result = -2_147_483_000 - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) = '4' and
                                                      Source(Source'First + 4) = '7' and
                                                      Source(Source'First + 5) = '4' and
                                                      Source(Source'First + 6) = '8' and
                                                      Source(Source'First + 7) < '3') then
                                                    (To_Int32'Result = -2_147_480_000 - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif
                                                    (Source(Source'First + 1) = '2' and
                                                         Source(Source'First + 2) = '1' and
                                                         Source(Source'First + 3) = '4' and
                                                         Source(Source'First + 4) = '7' and
                                                         Source(Source'First + 5) = '4' and
                                                         Source(Source'First + 6) < '8') then
                                                      (To_Int32'Result = -2_147_400_000 - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                        elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) = '4' and
                                                      Source(Source'First + 4) = '7' and
                                                      Source(Source'First + 5) < '4') then
                                                    (To_Int32'Result = -2_147_000_000 - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) = '4' and
                                                      Source(Source'First + 4) < '7') then
                                                    (To_Int32'Result = -2_140_000_000 - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) = '1' and
                                                      Source(Source'First + 3) < '4') then
                                                    (To_Int32'Result = -2_100_000_000 - 10_000_000*I (Source, 3) - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) = '2' and
                                                      Source(Source'First + 2) < '1') then
                                                    (To_Int32'Result = -2_000_000_000 - 100_000_000*I (Source, 2) - 10_000_000*I (Source, 3) - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10))
                                                      elsif (Source(Source'First + 1) < '2') then
                                                    (To_Int32'Result = -1_000_000_000*I (Source, 1) - 100_000_000*I (Source, 2) - 10_000_000*I (Source, 3) - 1_000_000*I (Source, 4) - 100_000*I (Source, 5) - 10_000*I (Source, 6) - 1_000*I (Source, 7) - 100*I (Source, 8) - 10*I (Source, 9) - I (Source, 10)))));

   procedure To_Float (Source     : in  String_T;
                       Target     : out Float_T;
                       Has_Failed : out Boolean) with
     Global         => null;

   type Max_Hash_Map_Size_T is range 3..(2**31);

private

   function To_String (This : Int32_T) return String_T is (String_T (Aida_Z.Int32.To_String (Aida_Z.Int32.T (This))));

   function Hash32 (This : Int32_T) return Hash32_T is (Hash32_T (Aida_Z.Int32.Hash32 (Aida_Z.Int32.T (This))));

   function Hash32 (This : String_T) return Hash32_T is (Hash32_T (Aida_Z.String.Hash32 (Aida_Z.String.T (This))));

end Aida;
