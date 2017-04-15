package body Aida.Types is

   use all type Aida.String.T;

   procedure To_Int32 (Source     : in  String_T;
                       Target     : out Int32_T;
                       Has_Failed : out Boolean) is
   begin
      To_Int32 (Aida.String.T (Source), Zzz_Int32_T (Target), Has_Failed);
   end To_Int32;

end Aida.Types;
