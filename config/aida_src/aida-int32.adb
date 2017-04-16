with Ada.Strings.Fixed;
with Interfaces;

package body Aida.Int32 is

   use type Interfaces.Unsigned_32;

   function To_String (This : T) return Zzz_String_T with
     SPARK_Mode => Off
   is
   begin
      return Zzz_String_T (Ada.Strings.Fixed.Trim (T'Image (This), Ada.Strings.Both));
   end To_String;

   function Hash32 (This : T) return Zzz_Hash32_T with
     SPARK_Mode => On
   is
      X : Interfaces.Unsigned_32 := (if This >= 0 then
                                        Interfaces.Unsigned_32 (This)
                                     else
                                        Interfaces.Unsigned_32 (-This));
   begin
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X);

      return Zzz_Hash32_T (X);
   end Hash32;

end Aida.Int32;
