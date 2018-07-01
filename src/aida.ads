package Aida with Pure is
   pragma SPARK_Mode;

   subtype Int32 is Standard.Integer range -2**31 .. (2**31 - 1);

   subtype Pos32 is Int32 range 1 .. Int32'Last;

   subtype Nat32 is Int32 range 0 .. Int32'Last;

   type Hash32 is mod 2**32;

   function Is_Digit (C : Standard.Character) return Boolean with
     Contract_Cases => (C = '0' => Is_Digit'Result,
                        C = '1' => Is_Digit'Result,
                        C = '2' => Is_Digit'Result,
                        C = '3' => Is_Digit'Result,
                        C = '4' => Is_Digit'Result,
                        C = '5' => Is_Digit'Result,
                        C = '6' => Is_Digit'Result,
                        C = '7' => Is_Digit'Result,
                        C = '8' => Is_Digit'Result,
                        C = '9' => Is_Digit'Result,
                        C > '9' => Is_Digit'Result = False,
                        C < '0' => Is_Digit'Result = False);

   function To_Int32 (Source : in Standard.Character) return Int32 with
     Pre  => Is_Digit (Source),
     Post => To_Int32'Result in 0 .. 9,
     Contract_Cases => (Source = '0' => To_Int32'Result = 0,
                        Source = '1' => To_Int32'Result = 1,
                        Source = '2' => To_Int32'Result = 2,
                        Source = '3' => To_Int32'Result = 3,
                        Source = '4' => To_Int32'Result = 4,
                        Source = '5' => To_Int32'Result = 5,
                        Source = '6' => To_Int32'Result = 6,
                        Source = '7' => To_Int32'Result = 7,
                        Source = '8' => To_Int32'Result = 8,
                        Source = '9' => To_Int32'Result = 9);

   procedure To_Int32 (Source : in     Standard.Character;
                       Target :    out Int32) with
     Pre  => Is_Digit (Source),
     Post => Target in 0 .. 9 and Target = To_Int32 (Source),
     Contract_Cases => (Source = '0' => Target = 0,
                        Source = '1' => Target = 1,
                        Source = '2' => Target = 2,
                        Source = '3' => Target = 3,
                        Source = '4' => Target = 4,
                        Source = '5' => Target = 5,
                        Source = '6' => Target = 6,
                        Source = '7' => Target = 7,
                        Source = '8' => Target = 8,
                        Source = '9' => Target = 9);

   function Is_One_Byte_UTF8 (C : Character) return Boolean is
     (Character'Pos (C) <= 127);
   --  A UTF8 code point can be represented by 1-4 characters.
   --  The first 128 characters (US-ASCII) need only one byte.

   function To_Hash32 (This : Int32) return Aida.Hash32 with
     Global => null;
   --  Calculates the hash of an 32-bit Int32 based upon the following post
   --  on Stackoverlow:
   --  http://stackoverflow.com/questions/664014/
   --  what-Int32-hash-function-are-good-that-accepts-an-Int32-hash-key
   --
   --  I found the following algorithm provides
   --  a very good statistical distribution.
   --  Each input bit affects each output bit with about 50% probability.
   --  There are no collisions (each input results in a different output).
   --  The algorithm is fast except if the CPU doesn't have a
   --  built-in Int32 multiplication unit. C-Code:
   --
   --  unsigned int hash(unsigned int x) {
   --      x = ((x >> 16) ^ x) * 0x45d9f3b;
   --      x = ((x >> 16) ^ x) * 0x45d9f3b;
   --      x = (x >> 16) ^ x;
   --      return x;
   --  }
   --
   --  The magic number was calculated using a special multi-threaded
   --  test program that ran for many hours,
   --  which calculates the avalanche effect
   --  (the number of output bits that change if a single input bit is changed;
   --  should be nearly 16 on average), independence of output bit changes
   --  (output bits should not depend on each other),
   --  and the probability of a change in each output bit if any input bit
   --  is changed. The calculated values are better than the 32-bit finalizer
   --   used by MurmurHash, and nearly as good (not quite) as when using AES.

   function To_Hash32 (This : String) return Hash32 with
     Global => null;

   function To_String (This : Int32) return String with
     Global => null,
     Post   =>
       (if This < 0 then
          To_String'Result'Length >= 2 and To_String'Result'Length <= 11
            else
              To_String'Result'Length >= 1 and To_String'Result'Length <= 10);

   function To_Char (This : Int32) return Standard.Character with
     Global => null,
     Pre    => This >= 0 and This <= 9;

   function To_String (This : Standard.Float) return Standard.String with
     Global => null,
     Post   => To_String'Result'Length >= 1 and To_String'Result'Length <= 11;

   function I (Source : String;
               Index  : Nat32) return Int32 is
     (Aida.To_Int32 (Source (Source'First + Index))) with
     Pre   => (Index < Source'Length and then
                 Source'First + Index <= Source'Last and then
                   Aida.Is_Digit (Source (Source'First + Index)));

   procedure To_Int32 (Source     : in  String;
                       Target     : out Int32;
                       Has_Failed : out Boolean) with
     Global         => null,
     Contract_Cases =>
       (Source'Length = 0 => Has_Failed,
        Source'Length = 1 =>
          (if Source (Source'First) = '-' then
               Has_Failed
                 elsif
                   Aida.Is_Digit (Source (Source'First)) then
               (Has_Failed = False and Target = I (Source, 0))
                 else
                   Has_Failed),
        Source'Length = 2 =>
          (if (for all Index in Source'Range => Is_Digit (Source (Index))) then
               (Has_Failed = False and
                    Target = 10 * I (Source, 0) + I (Source, 1)
               ) elsif
             (Source (Source'First) = '-' and (for all Index in Int32 range
                  (Source'First + 1) .. Source'Last =>
                  Is_Digit (Source (Index)))
             ) then
               (Has_Failed = False and Target = -I (Source, 1))
                 else
                   Has_Failed
          ),
        Source'Length = 3 =>
          (if (for all Index in Source'Range => Is_Digit (Source (Index))) then
               (Has_Failed = False and
                    Target = 100 * I (Source, 0) +
                      10 * I (Source, 1) +
                      I (Source, 2))
                 elsif ((Source (Source'First) = '-') and
                 (for all Index in Int32 range
                    (Source'First + 1) .. Source'Last =>
                        Is_Digit (Source (Index))))
             then
               (Has_Failed = False and Target = -10 * I (Source, 1) -
                      I (Source, 2))
                 else
                   Has_Failed),
        Source'Length = 4 =>
          (if (for all Index in Source'Range => Is_Digit (Source (Index))) then
             (Has_Failed = False and
                    Target = 1_000 * I (Source, 0) +
                      100 * I (Source, 1) +
                      10 * I (Source, 2) +
                      I (Source, 3))
                 elsif ((Source (Source'First) = '-') and
                 (for all Index in Int32 range
                    (Source'First + 1) .. Source'Last =>
                        Is_Digit (Source (Index))
                 )) then
             (Has_Failed = False and Target = -100 * I (Source, 1) -
                  10 * I (Source, 2) -
                  I (Source, 3))
                 else
                   Has_Failed),
        Source'Length = 5 =>
          (if (for all Index in Source'Range => Is_Digit (Source (Index))) then
               (Has_Failed = False and
                    Target = 10_000 * I (Source, 0) +
                      1_000 * I (Source, 1) +
                      100 * I (Source, 2) +
                      10 * I (Source, 3) +
                      I (Source, 4))
                 elsif ((Source (Source'First) = '-') and
                 (for all Index in Int32 range
                    (Source'First + 1) .. Source'Last =>
                        Is_Digit (Source (Index))
                 )) then
             (Has_Failed = False and Target = -1_000 * I (Source, 1) -
                  100 * I (Source, 2) -
                  10 * I (Source, 3) -
                  I (Source, 4))
                 else
                   Has_Failed),
        Source'Length = 6 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                (Has_Failed = False and
                                   Target = 100_000*I (Source, 0) + 10_000*I (Source, 1) + 1_000*I (Source, 2) + 100*I (Source, 3) + 10*I (Source, 4) + I (Source, 5))
                                  elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                (Has_Failed = False and Target = -10_000*I (Source, 1) - 1_000*I (Source, 2) - 100*I (Source, 3) - 10*I (Source, 4) - I (Source, 5))
                                  else
                                    Has_Failed),
        Source'Length = 7 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                (Has_Failed = False and
                                   Target = 1_000_000*I (Source, 0) + 100_000*I (Source, 1) + 10_000*I (Source, 2) + 1_000*I (Source, 3) + 100*I (Source, 4) + 10*I (Source, 5) + I (Source, 6))
                                  elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                (Has_Failed = False and Target = -100_000*I (Source, 1) - 10_000*I (Source, 2) - 1_000*I (Source, 3) - 100*I (Source, 4) - 10*I (Source, 5) - I (Source, 6))
                                  else
                                    Has_Failed),
        Source'Length = 8 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                (Has_Failed = False and
                                   Target = 10_000_000*I (Source, 0) + 1_000_000*I (Source, 1) + 100_000*I (Source, 2) + 10_000*I (Source, 3) + 1_000*I (Source, 4) + 100*I (Source, 5) + 10*I (Source, 6) + I (Source, 7))
                                  elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                (Has_Failed = False and Target = -1_000_000*I (Source, 1) - 100_000*I (Source, 2) - 10_000*I (Source, 3) - 1_000*I (Source, 4) - 100*I (Source, 5) - 10*I (Source, 6) - I (Source, 7))
                                  else
                                    Has_Failed),
        Source'Length = 9 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                (Has_Failed = False and
                                   Target = 100_000_000*I (Source, 0) + 10_000_000*I (Source, 1) + 1_000_000*I (Source, 2) + 100_000*I (Source, 3) + 10_000*I (Source, 4) + 1_000*I (Source, 5) + 100*I (Source, 6) + 10*I (Source, 7) + I (Source, 8))
                                  elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                (Has_Failed = False and Target = -10_000_000*I (Source, 1) - 1_000_000*I (Source, 2) - 100_000*I (Source, 3) - 10_000*I (Source, 4) - 1_000*I (Source, 5) - 100*I (Source, 6) - 10*I (Source, 7) - I (Source, 8))
                                  else
                                    Has_Failed),
        Source'Length = 10 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
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
                                       elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                 (Has_Failed = False and
                                    Target = -100_000_000*I (Source, 1) - 10_000_000*I (Source, 2) - 1_000_000*I (Source, 3) - 100_000*I (Source, 4) - 10_000*I (Source, 5) - 1_000*I (Source, 6) - 100*I (Source, 7) - 10*I (Source, 8) - I (Source, 9))
                                   else
                                     Has_Failed),
        Source'Length = 11 => (if ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
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

   function To_Int32 (Source : Standard.String) return Int32 with
     Global => null,
     Pre => ((Source'Length >= 1 and Source'Length <= 11) and then (
                 if Source'Length = 1 then
                   Aida.Is_Digit (Source (Source'First))
               elsif (Source'Length >= 2 and Source'Length <= 9) then
                 ((for all Index in Source'Range => Aida.Is_Digit (Source (Index))) or
                    ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))))
               elsif Source'Length = 10 then
                 (((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) or
                    ((for all Index in Source'Range => Aida.Is_Digit (Source (Index))) and then
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
               (((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) and then
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
                          Source'Length = 2 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 10*I (Source, 0) + I (Source, 1))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -I (Source, 1))),
                          Source'Length = 3 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 100*I (Source, 0) + 10*I (Source, 1) + I (Source, 2))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -10*I (Source, 1) - I (Source, 2))),
                          Source'Length = 4 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 1_000*I (Source, 0) + 100*I (Source, 1) + 10*I (Source, 2) + I (Source, 3))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -100*I (Source, 1) - 10*I (Source, 2) - I (Source, 3))),
                          Source'Length = 5 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 10_000*I (Source, 0) + 1_000*I (Source, 1) + 100*I (Source, 2) + 10*I (Source, 3) + I (Source, 4))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -1_000*I (Source, 1) - 100*I (Source, 2) - 10*I (Source, 3) - I (Source, 4))),
                          Source'Length = 6 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 100_000*I (Source, 0) + 10_000*I (Source, 1) + 1_000*I (Source, 2) + 100*I (Source, 3) + 10*I (Source, 4) + I (Source, 5))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -10_000*I (Source, 1) - 1_000*I (Source, 2) - 100*I (Source, 3) - 10*I (Source, 4) - I (Source, 5))),
                          Source'Length = 7 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 1_000_000*I (Source, 0) + 100_000*I (Source, 1) + 10_000*I (Source, 2) + 1_000*I (Source, 3) + 100*I (Source, 4) + 10*I (Source, 5) + I (Source, 6))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -100_000*I (Source, 1) - 10_000*I (Source, 2) - 1_000*I (Source, 3) - 100*I (Source, 4) - 10*I (Source, 5) - I (Source, 6))),
                          Source'Length = 8 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 10_000_000*I (Source, 0) + 1_000_000*I (Source, 1) + 100_000*I (Source, 2) + 10_000*I (Source, 3) + 1_000*I (Source, 4) + 100*I (Source, 5) + 10*I (Source, 6) + I (Source, 7))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -1_000_000*I (Source, 1) - 100_000*I (Source, 2) - 10_000*I (Source, 3) - 1_000*I (Source, 4) - 100*I (Source, 5) - 10*I (Source, 6) - I (Source, 7))),
                          Source'Length = 9 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
                                                  (To_Int32'Result = 100_000_000*I (Source, 0) + 10_000_000*I (Source, 1) + 1_000_000*I (Source, 2) + 100_000*I (Source, 3) + 10_000*I (Source, 4) + 1_000*I (Source, 5) + 100*I (Source, 6) + 10*I (Source, 7) + I (Source, 8))
                                                    elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                  (To_Int32'Result = -10_000_000*I (Source, 1) - 1_000_000*I (Source, 2) - 100_000*I (Source, 3) - 10_000*I (Source, 4) - 1_000*I (Source, 5) - 100*I (Source, 6) - 10*I (Source, 7) - I (Source, 8))),
                          Source'Length = 10 => (if (for all Index in Source'Range => Aida.Is_Digit (Source (Index))) then
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
                                                     elsif ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
                                                   (To_Int32'Result = -100_000_000*I (Source, 1) - 10_000_000*I (Source, 2) - 1_000_000*I (Source, 3) - 100_000*I (Source, 4) - 10_000*I (Source, 5) - 1_000*I (Source, 6) - 100*I (Source, 7) - 10*I (Source, 8) - I (Source, 9))),
                          Source'Length = 11 => (if ((Source (Source'First) = '-') and (for all Index in Int32 range (Source'First + 1) .. Source'Last => Aida.Is_Digit (Source (Index)))) then
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

   procedure To_Float (Source     : in  Standard.String;
                       Target     : out Standard.Float;
                       Has_Failed : out Boolean) with
     Global => null;

   function Is_Latin1_Graphic_Characters (Text : Standard.String) return Boolean with
     Global => null;

   function Starts_With (This         : Standard.String;
                         Searched_For : Standard.String) return Boolean with
     Global => null,
     Pre    => Searched_For'Length > 0;

   function Concat (Left, Right : Standard.String) return Standard.String with
     Global => null,
     Pre    => Left'Length < Pos32'Last/2 and Right'Length < Pos32'Last/2,
     Post   => Concat'Result'Length = Left'Length + Right'Length;

   type Max_Hash_Map_Size_T is range 3..(2**31);

end Aida;
