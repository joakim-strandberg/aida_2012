package body Aida.Integer_To_String_Map is

   procedure Append (This     : in out T;
                     New_Item : Element_T) is
   begin
      This.My_Huge_Text (This.My_Next + 1..This.My_Next + New_Item'Length) := New_Item;
      Item_Vector.Append (This     => This.My_Substrings,
                          New_Item => (From => This.My_Next + 1,
                                       To   => This.My_Next + New_Item'Length));
      This.My_Next := This.My_Next + New_Item'Length;
   end Append;

   procedure Act_On_Immutable_Text (This  : T;
                                    Index : Index_T)
   is
      pragma Assume (This.Capacity >= Item_Vector.Element (This.My_Substrings, Index).To);

      S : constant Substring_T := Element (This.My_Substrings, Index);
   begin
      Do_Something (This.My_Huge_Text (S.From..S.To));
   end Act_On_Immutable_Text;

   function Check_Something_On_Immutable_Text (This  : T;
                                               Index : Index_T;
                                               Arg   : Arg_T) return Return_T
   is
      pragma Assume (This.Capacity >= Item_Vector.Element (This.My_Substrings, Index).To);

      S : constant Substring_T := Element (This.My_Substrings, Index);
   begin
      return Check_Something (This.My_Huge_Text (S.From..S.To), Arg);
   end Check_Something_On_Immutable_Text;

end Aida.Integer_To_String_Map;
