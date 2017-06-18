package Aida with SPARK_Mode is
   pragma Pure;

   type Zzz_Int32_T is new Long_Integer      range -2**31 .. (2**31 - 1);
   type Zzz_Int64_T is new Long_Long_Integer range -2**63 .. (2**63 - 1);
   type Zzz_String_T is new Standard.String;
   type Zzz_Hash32_T is mod 2**32;

end Aida;
