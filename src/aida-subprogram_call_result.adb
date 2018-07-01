package body Aida.Subprogram_Call_Result with SPARK_Mode is

   procedure Initialize (This    : in out T;
                         Code_1 : Int32;
                         Code_2 : Int32) is
   begin
      This.My_Code_1 := Code_1;
      This.My_Code_2 := Code_2;
      This.My_Has_Failed := True;
   end Initialize;

   function Message (This : T) return Standard.String is
      subtype Index_T is Pos32 range 1..24;

      Text : Standard.String (Index_T'Range) := (others => '0');

      Max : Index_T;
   begin
      if This.My_Code_1 >= 0 then
         if This.My_Code_2 >= 0 then
            Max := 22;

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_1);
               L : Int32 := 10 - Text2'Length + 1;
            begin
               pragma Assert (L <= 10);
               Text (L..10) := Text2 (Text2'First .. Text2'Last);
            end;

            Text (11..12) := ", ";

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_2);
               L : Int32 := 22 - Text2'Length + 1;
            begin
               Text (L..22) := Text2 (Text2'First .. Text2'Last);
            end;
         else
            Max := 23;

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_1);
               L : Int32 := 10 - Text2'Length + 1;
            begin
               pragma Assert (L <= 10);
               Text (L..10) := Text2 (Text2'First .. Text2'Last);
            end;

            Text (11..12) := ", ";

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_2);
               L : Int32 := 23 - Text2'Length + 1;
            begin
               Text (13) := '-';
               Text (L + 1..23) := Text2 (Text2'First + 1 .. Text2'Last);
            end;
         end if;
      else
         if This.My_Code_2 >= 0 then
            Max := 23;

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_1);
               L : Int32 := 11 - Text2'Length + 1;
            begin
               pragma Assert (L <= 11);
               Text (1) := '-';
               Text (L + 1..11) := Text2 (Text2'First + 1 .. Text2'Last);
            end;

            Text (12..13) := ", ";

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_2);
               L : Int32 := 23 - Text2'Length + 1;
            begin
               Text (L..23) := Text2 (Text2'First .. Text2'Last);
            end;
         else
            Max := 24;

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_1);
               L : Int32 := 11 - Text2'Length + 1;
            begin
               pragma Assert (L <= 11);
               Text (1) := '-';
               Text (L + 1..11) := Text2 (Text2'First + 1 .. Text2'Last);
            end;

            Text (12..13) := ", ";

            declare
               Text2 : Standard.String := Aida.To_String (This.My_Code_2);
               L : Int32 := 24 - Text2'Length + 1;
            begin
               Text (14) := '-';
               Text (L + 1..24) := Text2 (Text2'First + 1 .. Text2'Last);
            end;
         end if;
      end if;

      return Text (1..Max);
   end Message;

end Aida.Subprogram_Call_Result;
