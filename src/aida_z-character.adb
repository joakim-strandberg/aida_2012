package body Aida_Z.Character with SPARK_Mode is
   pragma Suppress (Discriminant_Check);
   pragma Suppress (Division_Check);
   pragma Suppress (Index_Check);
   pragma Suppress (Length_Check);
   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);
   pragma Suppress (Tag_Check);
--     pragma Suppress (Elaboration_Check);

   use type Zzz_Int32_T;

   function Is_Digit (C : T) return Boolean is
   begin
      return C in '0'..'9';
   end Is_Digit;

   function To_Int32 (Source : in T) return Zzz_Int32_T is
   begin
      return Standard.Character'Pos (Standard.Character (Source)) - Standard.Character'Pos ('0');
   end To_Int32;

   procedure To_Int32 (Source : in T;
                       Target : out Zzz_Int32_T) is
   begin
      Target := Standard.Character'Pos (Standard.Character (Source)) - Standard.Character'Pos ('0');
   end To_Int32;

end Aida_Z.Character;
