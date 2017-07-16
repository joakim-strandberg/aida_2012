package body Aida with SPARK_Mode is

   use all type Aida_Z.String.T;
   use all type Aida_Z.Int32.T;
   use all type Aida_Z.Float.T;

   procedure To_Int32 (Source     : in  String_T;
                       Target     : out Int32_T;
                       Has_Failed : out Boolean) is
   begin
      To_Int32 (Aida_Z.String.T (Source), Aida_Z.Zzz_Int32_T (Target), Has_Failed);
   end To_Int32;

   function To_Int32 (Source : String_T) return Int32_T is
   begin
      return  Int32_T (Aida_Z.Zzz_Int32_T'(To_Int32 (Aida_Z.String.T (Source))));
   end To_Int32;

   procedure To_Float (Source     : in  String_T;
                       Target     : out Float_T;
                       Has_Failed : out Boolean) is
   begin
      To_Float (Aida_Z.String.T (Source), Aida_Z.Zzz_Float_T (Target), Has_Failed);
   end To_Float;

end Aida;
