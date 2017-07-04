package Aida_Z.Int32 with SPARK_Mode, Pure is

   type T is new Zzz_Int32_T;

   function To_String (This : T) return Zzz_String_T with
     Global => null,
     Post   => To_String'Result'Length <= 11;

   function To_Char (This : T) return Zzz_Char_T with
     Global => null,
     Pre    => This >= 0 and This <= 9;

   -- Calculates the hash of an 32-bit integer based upon the following post
   -- on Stackoverlow:
   -- http://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key
   --
   -- I found the following algorithm provides a very good statistical distribution.
   -- Each input bit affects each output bit with about 50% probability.
   -- There are no collisions (each input results in a different output).
   -- The algorithm is fast except if the CPU doesn't have a
   -- built-in integer multiplication unit. C-Code:
   --
   --  unsigned int hash(unsigned int x) {
   --      x = ((x >> 16) ^ x) * 0x45d9f3b;
   --      x = ((x >> 16) ^ x) * 0x45d9f3b;
   --      x = (x >> 16) ^ x;
   --      return x;
   --  }
   --
   -- The magic number was calculated using a special multi-threaded
   -- test program that ran for many hours, which calculates the avalanche effect
   -- (the number of output bits that change if a single input bit is changed;
   -- should be nearly 16 on average), independence of output bit changes
   -- (output bits should not depend on each other), and the probability of a change
   -- in each output bit if any input bit is changed. The calculated values are better
   -- than the 32-bit finalizer used by MurmurHash, and nearly as good (not quite) as when using AES.
   function Hash32 (This : T) return Zzz_Hash32_T with
     Global => null;


private

   function To_Char (This : T) return Zzz_Char_T is (
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

end Aida_Z.Int32;
