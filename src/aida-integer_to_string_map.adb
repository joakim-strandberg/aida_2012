package body Aida.Integer_To_String_Map is

   procedure Append (This  : in out T;
                     Value : Value_T;
                     Key   : out Key_T) is
   begin
      This.My_Huge_Text (Positive (This.My_Next + 1)..Positive (This.My_Next + Value'Length)) := Value;
      This.My_Next_Index := This.My_Next_Index + 1;
      This.My_Substrings (This.My_Next_Index) := (From => This.My_Next + 1,
                                                  To   => This.My_Next + Value'Length);
      This.My_Next := This.My_Next + Value'Length;
      Key := This.My_Next_Index;
   end Append;

end Aida.Integer_To_String_Map;
