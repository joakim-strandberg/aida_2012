-- The contents of this package and its childpackages should not be used directly.
-- It exists solely because the number and string types have circular-dependencies
-- and the code that does the conversions should be in SPARK.
package Aida_Z with SPARK_Mode, Pure is

   type Zzz_Char_T is new Character;
   type Zzz_Int32_T is new Long_Integer      range -2**31 .. (2**31 - 1);
   type Zzz_Int64_T is new Long_Long_Integer range -2**63 .. (2**63 - 1);
   type Zzz_Float_T is new Float;
   type Zzz_String_T is new Standard.String;
   type Zzz_Hash32_T is mod 2**32;

end Aida_Z;
