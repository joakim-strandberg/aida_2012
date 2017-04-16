with Aida.Types;

package Aida.Text_IO with SPARK_Mode is

   procedure Put_Line (Item : String) with
     Global => null;

   procedure Put_Line (S1 : String;
                       S2 : String) with
     Global => null;

   procedure Put_Line (S1 : String;
                       S2 : Aida.Types.String_T) with
     Global => null;

   procedure Put_Line (S1 : Aida.Types.String_T;
                       S2 : String) with
     Global => null;

   procedure Put_Line (S1 : String;
                       S2 : String;
                       S3 : String) with
     Global => null;

   procedure Put_Line (S1 : Aida.Types.String_T;
                       S2 : String;
                       S3 : String) with
     Global => null;

   procedure Put_Line (S1 : String;
                       S2 : Aida.Types.String_T;
                       S3 : String) with
     Global => null;

   procedure Put_Line (S1 : String;
                       S2 : String;
                       S3 : Aida.Types.String_T) with
     Global => null;

   procedure Put_Line (S1 : String;
                       S2 : String;
                       S3 : String;
                       S4 : String) with
     Global => null;

   procedure Put_Line (S1 : String;
                       S2 : String;
                       S3 : String;
                       S4 : String;
                       S5 : String) with
     Global => null;

   procedure Put (Item : String) with
     Global => null;

   procedure Put (S1 : String;
                  S2 : String) with
     Global => null;

   procedure Put (S1 : String;
                  S2 : String;
                  S3 : String) with
     Global => null;

   function Get_Line return String with
     Global => null;

end Aida.Text_IO;
