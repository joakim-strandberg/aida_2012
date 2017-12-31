with Aida.XML_SAX_Parse;

pragma Elaborate_All (Aida.XML_SAX_Parse);

package body Aida.XML_DOM_Parser is

   package body Current_Ids_Def is

      function Max_Node_Id (This : Current_Ids_T) return Node_Id_T is
         pragma Unreferenced (This);
      begin
         return (Node_Id_T'Last);
      end Max_Node_Id;

      procedure Append_Node (This : in out Current_Ids_T;
                             Node : in     Current_Node_T) is
      begin
         This.Node_Ids.Append (Node);
      end Append_Node;

      --        procedure Append_Array_Id (This     : in out Current_Ids_T;
      --                                   Array_Id : in     Array_Index_T) is
      --        begin
      --           Append (This.Node_Ids, (Id       => Array_Construct,
      --                                   Array_Id => Array_Id));
      --        end Append_Array_Id;

   end Current_Ids_Def;

   package body Max_Indices_Def is

      procedure Allocate_Node_Id (This : in out T;
                                  Id   : out Node_Id_T) is
      begin
         This.My_Node_Id_Max := This.My_Node_Id_Max + 1;
         Id := This.My_Node_Id_Max;
      end Allocate_Node_Id;

      procedure Allocate_Attribute_Id (This : in out T;
                                       Id   : out Attribute_Id_T) is
      begin
         This.My_Attribute_Id_Max := This.My_Attribute_Id_Max + 1;
         Id := This.My_Attribute_Id_Max;
      end Allocate_Attribute_Id;

      procedure Clear (This : in out T) is
         pragma Unreferenced (This);
      begin
         This.My_Node_Id_Max      := 0;
         This.My_Attribute_Id_Max := 0;
      end Clear;

   end Max_Indices_Def;

   procedure Start_Tag (This        : in out Public_Part_Def.Public_Part_T;
                        Max_Indices : in out Max_Indices_T;
                        State       : in out State_T;
                        Current_Ids : in out Current_Ids_T;
                        Tag_Name    : Aida.String_T;
                        Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure End_Tag (This        : in out Public_Part_Def.Public_Part_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Tag_Name    : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Text (This        : in out Public_Part_Def.Public_Part_T;
                   Max_Indices : in out Max_Indices_T;
                   State       : in out State_T;
                   Current_Ids : in out Current_Ids_T;
                   Value       : Aida.String_T;
                   Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Attribute (This            : in out Public_Part_Def.Public_Part_T;
                        Max_Indices     : in out Max_Indices_T;
                        State           : in out State_T;
                        Current_Ids     : in out Current_Ids_T;
                        Attribute_Name  : Aida.String_T;
                        Attribute_Value : Aida.String_T;
                        Call_Result     : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Comment (This        : in out Public_Part_Def.Public_Part_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure CDATA (This        : in out Public_Part_Def.Public_Part_T;
                    Max_Indices : in out Max_Indices_T;
                    State       : in out State_T;
                    Current_Ids : in out Current_Ids_T;
                    Value       : Aida.String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Start_Tag (This        : in out Public_Part_Def.Public_Part_T;
                        Max_Indices : in out Max_Indices_T;
                        State       : in out State_T;
                        Current_Ids : in out Current_Ids_T;
                        Tag_Name    : Aida.String_T;
                        Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case State is
         when Expecting_Object_Start =>
            if
              Tag_Name'Length > 0 and
              Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
--              not Current_Ids.Node_Ids.Is_Full and
              Current_Ids.Node_Ids.Is_Empty
            then
               declare
                  Id : Node_Id_T;
                  Current_Node : Current_Node_T;
                  Key : Int_To_String_Map.Key_T;
               begin
                  Max_Indices.Allocate_Node_Id (Id);
                  Current_Node.Node_Id := Id;
                  Current_Ids.Node_Ids.Append (Current_Node);

                  This.Map.Append (Tag_Name, Key);
                  This.Nodes (Id).Inner := (My_Id        => XML_Tag,
                                            My_Next_Node => Extended_Node_Id_T'First,
                                            My_JSON_Key  => Key,
                                            My_First_Child_Node   => Extended_Node_Id_T'First,
                                            My_First_Attribute_Id => Extended_Attribute_Id_T'First);
               end;
               State := Expecting_Default;
            else
               Call_Result.Initialize (-1835682926, -1646923797);
            end if;
         when Expecting_Default =>
            if
              Tag_Name'Length > 0 and
              Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
              not Current_Ids.Node_Ids.Is_Full
            then
               declare
                  Id : Node_Id_T;
                  Current_Node : Current_Node_T;
                  Key : Int_To_String_Map.Key_T;
               begin
                  Max_Indices.Allocate_Node_Id (Id);
                  Current_Node.Node_Id := Id;
                  Current_Ids.Node_Ids.Append (Current_Node);

                  This.Map.Append (Tag_Name, Key);
                  This.Nodes (Id).Inner := (My_Id        => XML_Tag,
                                            My_Next_Node => Extended_Node_Id_T'First,
                                            My_JSON_Key  => Key,
                                            My_First_Child_Node   => Extended_Node_Id_T'First,
                                            My_First_Attribute_Id => Extended_Attribute_Id_T'First);
               end;
               State := Expecting_Default;
            else
               Call_Result.Initialize (1882474635, -0124544835);
            end if;
         when End_State =>
            Call_Result.Initialize (-1916263923, -1891215598);
      end case;
   end Start_Tag;

   procedure End_Tag (This        : in out Public_Part_Def.Public_Part_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Tag_Name    : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unmodified (This);
      pragma Unmodified (Max_Indices);
   begin
      case State is
         when Expecting_Default =>
            if not Current_Ids.Node_Ids.Is_Empty then
               if This.Nodes (Current_Ids.Node_Ids.Last_Element.Node_Id).Name (This.Map) = Tag_Name then
                  Current_Ids.Node_Ids.Delete_Last;
                  if Current_Ids.Node_Ids.Is_Empty then
                     State := End_State;
                  end if;
               else
                  Call_Result.Initialize (-1262093307, -2026465349);
               end if;
            else
               Call_Result.Initialize (1242872628, -0551706534);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1504928428, -0102009206);
      end case;
   end End_Tag;

   procedure Text (This        : in out Public_Part_Def.Public_Part_T;
                   Max_Indices : in out Max_Indices_T;
                   State       : in out State_T;
                   Current_Ids : in out Current_Ids_T;
                   Value       : Aida.String_T;
                   Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case State is
         when Expecting_Default =>
            if not Current_Ids.Node_Ids.Is_Empty then
               if Value'Length = 0 or (Value'Length > 0 and then
                                         (for all I in Value'Range => Value (I) = ' ' or Value (I) = Character'Val (12) or Value (I) = Character'Val (13))) then
                  null;
               else
                  declare
                     Id  : Node_Id_T;
                     Key : Int_To_String_Map.Key_T;

                     Current_Node : Current_Node_T;
                  begin
                     Max_Indices.Allocate_Node_Id (Id);
                     This.Map.Append (Value, Key);

                     This.Nodes (Id).Inner := (My_Id        => XML_Text,
                                               My_Next_Node => Extended_Node_Id_T'First,
                                               My_Key       => Key);

                     if Current_Ids.Node_Ids.Last_Element.Last_Child_Id = Extended_Node_Id_T'First then
                        This.Nodes (Current_Ids.Node_Ids.Last_Element.Node_Id).Inner.My_First_Child_Node := Id;
                     else
                        This.Nodes (Current_Ids.Node_Ids.Last_Element.Last_Child_Id).Inner.My_Next_Node := Id;
                     end if;

                     Current_Node := Current_Ids.Node_Ids.Last_Element;
                     Current_Node.Last_Child_Id := Id;
                     Current_Ids.Node_Ids.Replace_Last_Element (Current_Node);
                  end;
               end if;
            else
               Call_Result.Initialize (0621778668, -1095646032);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-0440932756, 1176624933);
      end case;
   end Text;

   procedure Attribute (This            : in out Public_Part_Def.Public_Part_T;
                        Max_Indices     : in out Max_Indices_T;
                        State           : in out State_T;
                        Current_Ids     : in out Current_Ids_T;
                        Attribute_Name  : Aida.String_T;
                        Attribute_Value : Aida.String_T;
                        Call_Result     : in out Aida.Subprogram_Call_Result.T) is
   begin
      case State is
         when Expecting_Default =>
            if not Current_Ids.Node_Ids.Is_Empty then
               declare
                  Id  : Attribute_Id_T;
                  Name_Key  : Int_To_String_Map.Key_T;
                  Value_Key : Int_To_String_Map.Key_T;

                  Current_Node : Current_Node_T;
               begin
                  Max_Indices.Allocate_Attribute_Id (Id);
                  This.Map.Append (Attribute_Name,  Name_Key);
                  This.Map.Append (Attribute_Value, Value_Key);



--                    This.Nodes (Id).Inner := (My_Id        => XML_Text,
--                                              My_Next_Node => Extended_Node_Id_T'First,
--                                              My_Key       => Key);
                  if Current_Ids.Node_Ids.Last_Element.Last_Attribute_Id = Extended_Attribute_Id_T'First then
                     This.Nodes (Current_Ids.Node_Ids.Last_Element.Node_Id).Inner.My_First_Attribute_Id := Id;

                     Current_Node := Current_Ids.Node_Ids.Last_Element;
                     Current_Node.Last_Attribute_Id := Id;
                     Current_Ids.Node_Ids.Replace_Last_Element (Current_Node);
                  else
                     This.Attributes (Current_Ids.Node_Ids.Last_Element.Last_Attribute_Id).My_Next_Attribute := Id;
                  end if;
               end;
            else
               Call_Result.Initialize (0161942634, 2123499335);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-0353530820, -0093804016);
      end case;
   end Attribute;

   procedure Comment (This        : in out Public_Part_Def.Public_Part_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case State is
         when Expecting_Default =>
            if not Current_Ids.Node_Ids.Is_Empty then
               declare
                  Id  : Node_Id_T;
                  Key : Int_To_String_Map.Key_T;

                  Current_Node : Current_Node_T;
               begin
                  Max_Indices.Allocate_Node_Id (Id);
                  This.Map.Append (Value, Key);

                  This.Nodes (Id).Inner := (My_Id        => XML_Comment,
                                            My_Next_Node => Extended_Node_Id_T'First,
                                            My_Key       => Key);

                  if Current_Ids.Node_Ids.Last_Element.Last_Child_Id = Extended_Node_Id_T'First then
                     This.Nodes (Current_Ids.Node_Ids.Last_Element.Node_Id).Inner.My_First_Child_Node := Id;
                  else
                     This.Nodes (Current_Ids.Node_Ids.Last_Element.Last_Child_Id).Inner.My_Next_Node := Id;
                  end if;

                  Current_Node := Current_Ids.Node_Ids.Last_Element;
                  Current_Node.Last_Child_Id := Id;
                  Current_Ids.Node_Ids.Replace_Last_Element (Current_Node);
               end;
            else
               Call_Result.Initialize (1397833757, -1569655120);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1935786430, 1870268822);
      end case;
   end Comment;

   procedure CDATA (This        : in out Public_Part_Def.Public_Part_T;
                    Max_Indices : in out Max_Indices_T;
                    State       : in out State_T;
                    Current_Ids : in out Current_Ids_T;
                    Value       : Aida.String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case State is
         when Expecting_Default =>
            if not Current_Ids.Node_Ids.Is_Empty then
               declare
                  Id  : Node_Id_T;
                  Key : Int_To_String_Map.Key_T;

                  Current_Node : Current_Node_T;
               begin
                  Max_Indices.Allocate_Node_Id (Id);
                  This.Map.Append (Value, Key);

                  This.Nodes (Id).Inner := (My_Id        => XML_CDATA,
                                            My_Next_Node => Extended_Node_Id_T'First,
                                            My_Key       => Key);

                  if Current_Ids.Node_Ids.Last_Element.Last_Child_Id = Extended_Node_Id_T'First then
                     This.Nodes (Current_Ids.Node_Ids.Last_Element.Node_Id).Inner.My_First_Child_Node := Id;
                  else
                     This.Nodes (Current_Ids.Node_Ids.Last_Element.Last_Child_Id).Inner.My_Next_Node := Id;
                  end if;

                  Current_Node := Current_Ids.Node_Ids.Last_Element;
                  Current_Node.Last_Child_Id := Id;
                  Current_Ids.Node_Ids.Replace_Last_Element (Current_Node);
               end;
            else
               Call_Result.Initialize (0616203826, 1974203512);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1478093578, 1816232498);
      end case;
   end CDATA;

   procedure Parse_XML is new Aida.XML_SAX_Parse (Arg1_T    => Public_Part_Def.Public_Part_T,
                                                  Arg2_T    => Max_Indices_Def.T,
                                                  Arg3_T    => State_T,
                                                  Arg4_T    => Current_Ids_Def.Current_Ids_T,
                                                  Start_Tag => Start_Tag,
                                                  End_Tag   => End_Tag,
                                                  Text      => Text,
                                                  Attribute => Attribute,
                                                  Comment   => Comment,
                                                  CDATA     => CDATA);

   procedure Parse (This        : in out T;
                    XML_Message : String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      Max_Indices : Max_Indices_Def.T;

      Current_Ids : Current_Ids_Def.Current_Ids_T;

      State : State_T := Expecting_Object_Start;
   begin
      Max_Indices.Clear;

      Parse_XML (Public_Part_Def.Public_Part_T (This),
                 Max_Indices,
                 State,
                 Current_Ids,
                 XML_Message,
                 Call_Result);

      pragma Unused (Max_Indices);
      pragma Unused (Current_Ids);
      pragma Unused (State);
   end Parse;

end Aida.XML_DOM_Parser;
