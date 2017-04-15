with Interfaces;

package body Aida.Int32 is

   use type Interfaces.Unsigned_32;

   function Hash32 (This : T) return Zzz_Hash32_T is
      X : Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (This);
   begin
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X);

      return Zzz_Hash32_T (X);
   end Hash32;

end Aida.Int32;
