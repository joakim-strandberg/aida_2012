package Aida_Z.Float with SPARK_Mode, Pure is

   type T is new Zzz_Float_T;

   function To_String (This : T) return Zzz_String_T with
      Global => null;

end Aida_Z.Float;
