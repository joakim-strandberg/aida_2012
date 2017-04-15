with Ada.Text_IO;

package body Aida.Text_IO with SPARK_Mode => Off is

   procedure Put_Line (Item : String) is
   begin
      Ada.Text_IO.Put_Line(Item);
   end Put_Line;

   procedure Put_Line (S1 : String;
                       S2 : String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put_Line (S2);
   end Put_Line;

   procedure Put_Line (S1 : String;
                       S2 : Aida.Types.String_T) is
   begin
      Put_Line (S1, String (S2));
   end Put_Line;

   procedure Put_Line (S1 : String;
                       S2 : String;
                       S3 : String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put_Line (S3);
   end Put_Line;

   procedure Put_Line (S1 : String;
                       S2 : String;
                       S3 : String;
                       S4 : String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put (S3);
      Ada.Text_IO.Put_Line (S4);
   end Put_Line;

   procedure Put_Line (S1 : String;
                       S2 : String;
                       S3 : String;
                       S4 : String;
                       S5 : String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put (S3);
      Ada.Text_IO.Put (S4);
      Ada.Text_IO.Put_Line (S5);
   end Put_Line;

   procedure Put (Item : String) is
   begin
      Ada.Text_IO.Put (Item);
   end Put;

   procedure Put (S1 : String;
                  S2 : String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
   end Put;

   procedure Put (S1 : String;
                  S2 : String;
                  S3 : String) is
   begin
      Ada.Text_IO.Put (S1);
      Ada.Text_IO.Put (S2);
      Ada.Text_IO.Put (S3);
   end Put;

   function Get_Line return String is
   begin
      return Ada.Text_IO.Get_Line;
   end Get_Line;

end Aida.Text_IO;
