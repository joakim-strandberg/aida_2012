package Aida.Text_IO with SPARK_Mode is

   procedure Put_Line (Item : Standard.String) with
     Global => null;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String) with
     Global => null;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String;
                       S3 : Standard.String) with
     Global => null;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String;
                       S3 : Standard.String;
                       S4 : Standard.String) with
     Global => null;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String;
                       S3 : Standard.String;
                       S4 : Standard.String;
                       S5 : Standard.String) with
     Global => null;

   procedure Put (Item : Standard.String) with
     Global => null;

   procedure Put (S1 : Standard.String;
                  S2 : Standard.String) with
     Global => null;

   procedure Put (S1 : Standard.String;
                  S2 : Standard.String;
                  S3 : Standard.String) with
     Global => null;

   function Get_Line return Standard.String with
     Global => null;

end Aida.Text_IO;
