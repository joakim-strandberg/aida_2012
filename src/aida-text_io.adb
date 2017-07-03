with Ada.Text_IO;

package body Aida.Text_IO with SPARK_Mode => Off is

   procedure Put_Line (Item : String_T) is
   begin
      Ada.Text_IO.Put_Line(String (Item));
   end Put_Line;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T) is
   begin
      Ada.Text_IO.Put (String (S1));
      Ada.Text_IO.Put_Line (String (S2));
   end Put_Line;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T;
                       S3 : String_T) is
   begin
      Ada.Text_IO.Put (String (S1));
      Ada.Text_IO.Put (String (S2));
      Ada.Text_IO.Put_Line (String (S3));
   end Put_Line;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T;
                       S3 : String_T;
                       S4 : String_T) is
   begin
      Ada.Text_IO.Put (String (S1));
      Ada.Text_IO.Put (String (S2));
      Ada.Text_IO.Put (String (S3));
      Ada.Text_IO.Put_Line (String (S4));
   end Put_Line;

   procedure Put_Line (S1 : String_T;
                       S2 : String_T;
                       S3 : String_T;
                       S4 : String_T;
                       S5 : String_T) is
   begin
      Ada.Text_IO.Put (String (S1));
      Ada.Text_IO.Put (String (S2));
      Ada.Text_IO.Put (String (S3));
      Ada.Text_IO.Put (String (S4));
      Ada.Text_IO.Put_Line (String (S5));
   end Put_Line;

   procedure Put (Item : String_T) is
   begin
      Ada.Text_IO.Put (String (Item));
   end Put;

   procedure Put (S1 : String_T;
                  S2 : String_T) is
   begin
      Ada.Text_IO.Put (String (S1));
      Ada.Text_IO.Put (String (S2));
   end Put;

   procedure Put (S1 : String_T;
                  S2 : String_T;
                  S3 : String_T) is
   begin
      Ada.Text_IO.Put (String (S1));
      Ada.Text_IO.Put (String (S2));
      Ada.Text_IO.Put (String (S3));
   end Put;

   function Get_Line return String_T is
   begin
      return String_T (Ada.Text_IO.Get_Line);
   end Get_Line;

end Aida.Text_IO;
