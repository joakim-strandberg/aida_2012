package body Aida_Z.Float with SPARK_Mode, Pure is

function To_String (This : T) return Zzz_String_T with
   SPARK_Mode => Off
is
begin
   return Zzz_String_T (T'Image (This));
end To_String;

end Aida_Z.Float;
