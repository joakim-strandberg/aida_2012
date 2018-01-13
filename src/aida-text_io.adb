with Ada.Text_IO;

package body Aida.Text_IO with SPARK_Mode => Off is

   procedure Put_Line (Item : Standard.String) is
   begin
      Ada.Text_IO.Put_Line(Item);
   end Put_Line;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put_Line (S2);
   end Put_Line;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String;
                       S3 : Standard.String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put_Line (S3);
   end Put_Line;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String;
                       S3 : Standard.String;
                       S4 : Standard.String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put (S3);
      Ada.Text_IO.Put_Line (S4);
   end Put_Line;

   procedure Put_Line (S1 : Standard.String;
                       S2 : Standard.String;
                       S3 : Standard.String;
                       S4 : Standard.String;
                       S5 : Standard.String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put (S3);
      Ada.Text_IO.Put (S4);
      Ada.Text_IO.Put_Line (S5);
   end Put_Line;

   procedure Put (Item : Standard.String) is
   begin
      Ada.Text_IO.Put (Item);
   end Put;

   procedure Put (S1 : Standard.String;
                  S2 : Standard.String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
   end Put;

   procedure Put (S1 : Standard.String;
                  S2 : Standard.String;
                  S3 : Standard.String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put (S3);
   end Put;

   function Get_Line return Standard.String is
   begin
      return Standard.String (Ada.Text_IO.Get_Line);
   end Get_Line;

end Aida.Text_IO;
