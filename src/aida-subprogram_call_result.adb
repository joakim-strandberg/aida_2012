package body Aida.Subprogram_Call_Result with SPARK_Mode is

   use all type Aida.String_T;
   use all type Aida.Int32_T;

   procedure Initialize (This    : in out T;
                         Code_1 : Int32_T;
                         Code_2 : Int32_T) is
   begin
      This.My_Code_1 := Code_1;
      This.My_Code_2 := Code_2;
      This.My_Has_Failed := True;
   end Initialize;

   function Message (This : T) return String_T is
      subtype Index_T is Positive range 1..24;

      Text : String_T (Index_T'Range) := (others => '0');

      Max : Index_T;
   begin
      if This.My_Code_1 >= 0 then
         if This.My_Code_2 >= 0 then
            Max := 22;

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_1);
               L : Integer := 10 - Text2'Length + 1;
            begin
               pragma Assert (L <= 10);
               Text (L..10) := Text2 (Text2'First .. Text2'Last);
            end;

            Text (11..12) := ", ";

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_2);
               L : Integer := 22 - Text2'Length + 1;
            begin
               Text (L..22) := Text2 (Text2'First .. Text2'Last);
            end;
         else
            Max := 23;

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_1);
               L : Integer := 10 - Text2'Length + 1;
            begin
               pragma Assert (L <= 10);
               Text (L..10) := Text2 (Text2'First .. Text2'Last);
            end;

            Text (11..12) := ", ";

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_2);
               L : Integer := 23 - Text2'Length + 1;
            begin
               Text (13) := '-';
               Text (L + 1..23) := Text2 (Text2'First + 1 .. Text2'Last);
            end;
         end if;
      else
         if This.My_Code_2 >= 0 then
            Max := 23;

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_1);
               L : Integer := 11 - Text2'Length + 1;
            begin
               pragma Assert (L <= 11);
               Text (1) := '-';
               Text (L + 1..11) := Text2 (Text2'First + 1 .. Text2'Last);
            end;

            Text (12..13) := ", ";

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_2);
               L : Integer := 23 - Text2'Length + 1;
            begin
               Text (L..23) := Text2 (Text2'First .. Text2'Last);
            end;
         else
            Max := 24;

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_1);
               L : Integer := 11 - Text2'Length + 1;
            begin
               pragma Assert (L <= 11);
               Text (1) := '-';
               Text (L + 1..11) := Text2 (Text2'First + 1 .. Text2'Last);
            end;

            Text (12..13) := ", ";

            declare
               Text2 : String_T := Aida.Int32.To_String (This.My_Code_2);
               L : Integer := 24 - Text2'Length + 1;
            begin
               Text (14) := '-';
               Text (L + 1..24) := Text2 (Text2'First + 1 .. Text2'Last);
            end;
         end if;
      end if;

      return Text (1..Max);
   end Message;

end Aida.Subprogram_Call_Result;
