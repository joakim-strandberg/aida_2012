package Aida.Text_IO with SPARK_Mode is

   procedure Put_Line (Item : String_T) with
     Global => null;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T) with
     Global => null;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T;
                       S3 : String_T) with
     Global => null;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T;
                       S3 : String_T;
                       S4 : String_T) with
     Global => null;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T;
                       S3 : String_T;
                       S4 : String_T;
                       S5 : String_T) with
     Global => null;

   procedure Put (Item : String_T) with
     Global => null;

   procedure Put (S1 : String_T;
                  S2 : String_T) with
     Global => null;

   procedure Put (S1 : String_T;
                  S2 : String_T;
                  S3 : String_T) with
     Global => null;

   function Get_Line return String_T with
     Global => null;

end Aida.Text_IO;
