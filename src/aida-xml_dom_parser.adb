with Aida.XML.Generic_Parse_XML_File;

pragma Elaborate_All (Aida.XML.Generic_Parse_XML_File);

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
                        Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (This);
   begin
      Call_Result.Initialize (-1916263923, -1891215598);
--        case State is
--           when Expecting_Person_Start_Tag =>
--              if
--                Tag_Name = "person" and
--                Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
--                Last_Index (Current_Ids.Person_Ids) < Max_Index (Current_Ids.Person_Ids)
--              then
--                 declare
--                    Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
--                 begin
--                    Allocate_Person_Id (This      => Max_Indices,
--                                        Person_Id => Person_Id);
--                    Append (Current_Ids.Person_Ids, Person_Id);
--                 end;
--                 State := Expecting_Pre_Age_Text;
--              else
--                 Call_Result.Initialize (-1514498303, 1742243882);
--              end if;
--           when Expecting_Pre_Age_Text |
--                Expecting_Pre_Age_CDATA |
--                Expecting_Age_Value |
--                Expecting_Person_End_Tag |
--                Final_State =>
--              Call_Result.Initialize (-1916263923, -1891215598);
--        end case;
   end Start_Tag;

   procedure End_Tag (This        : in out Public_Part_Def.Public_Part_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Tag_Name    : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
   begin
      Call_Result.Initialize (-1916263923, -1891215598);

--        case State is
--           when Expecting_Person_End_Tag =>
--              if Tag_Name = "person" then
--                 State := Final_State;
--              else
--                 Call_Result.Initialize (1757113648, -2065026636);
--              end if;
--           when Expecting_Pre_Age_Text |
--                Expecting_Pre_Age_CDATA |
--                Expecting_Age_Value |
--                Expecting_Person_Start_Tag |
--                Final_State =>
--              Call_Result.Initialize (1987615793, 0718894067);
--        end case;
   end End_Tag;

   procedure Text (This        : in out Public_Part_Def.Public_Part_T;
                   Max_Indices : in out Max_Indices_T;
                   State       : in out State_T;
                   Current_Ids : in out Current_Ids_T;
                   Value       : Aida.String_T;
                   Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Max_Indices);
      pragma Unmodified (Current_Ids);
   begin
      Call_Result.Initialize (-1916263923, -1891215598);
--        case State is
--           when Expecting_Age_Value  =>
--              if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
--                 Call_Result.Initialize (-1824053881, 2061413792);
--              else
--                 declare
--                    Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
--                      Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
--                 begin
--                    if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
--                       Call_Result.Initialize (-1672668363, 0966305970);
--                    else
--                       Initialize (Result.Person (Person_Id).Name,
--                                   Value);
--                    end if;
--                 end;
--                 State := Expecting_Person_End_Tag;
--              end if;
--           when Expecting_Pre_Age_Text =>
--              if Value = "" then
--                 State := Expecting_Pre_Age_CDATA;
--              else
--                 Call_Result.Initialize (-0353024501, 0095044560);
--              end if;
--           when Expecting_Pre_Age_CDATA |
--                Expecting_Person_Start_Tag |
--                Expecting_Person_End_Tag |
--                Final_State =>
--              Call_Result.Initialize (-0440932756, 1176624933);
--        end case;
   end Text;

   procedure Attribute (This            : in out Public_Part_Def.Public_Part_T;
                        Max_Indices     : in out Max_Indices_T;
                        State           : in out State_T;
                        Current_Ids     : in out Current_Ids_T;
                        Attribute_Name  : Aida.String_T;
                        Attribute_Value : Aida.String_T;
                        Call_Result     : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Attribute_Name);
      pragma Unreferenced (Attribute_Value);
      pragma Unreferenced (State);
   begin
      Call_Result.Initialize (1506567129, -1557041959);
   end Attribute;

   procedure Comment (This        : in out Public_Part_Def.Public_Part_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
      pragma Unreferenced (State);
   begin
      Call_Result.Initialize (1733782299, 1310378793);
   end Comment;

   procedure CDATA (This        : in out Public_Part_Def.Public_Part_T;
                    Max_Indices : in out Max_Indices_T;
                    State       : in out State_T;
                    Current_Ids : in out Current_Ids_T;
                    Value       : Aida.String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
   begin
      Call_Result.Initialize (-1916263923, -1891215598);
--        case State is
--           when Expecting_Pre_Age_CDATA =>
--              if Result.Header_Comment'Length > Value'Length then
--                 Result.Header_Comment (1..Value'Length) := Value (Value'Range);
--                 State := Expecting_Age_Value;
--              else
--                 Call_Result.Initialize (0339578717, -0632456392);
--              end if;
--           when Expecting_Pre_Age_Text |
--                Expecting_Age_Value |
--                Expecting_Person_End_Tag |
--                Expecting_Person_Start_Tag |
--                Final_State =>
--              Call_Result.Initialize (-0580058624, 0796480717);
--        end case;
   end CDATA;

   procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Arg1_T    => Public_Part_Def.Public_Part_T,
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
