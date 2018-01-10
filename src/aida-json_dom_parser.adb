with Aida.JSON_SAX_Parse;

package body Aida.JSON_DOM_Parser is

   use all type Node_Id_Vector.T;

   package body Current_Ids_Def is

      function Max_Node_Id (This : Current_Ids_T) return Node_Index_T is
         pragma Unreferenced (This);
      begin
         return(Node_Index_T'Last);
      end Max_Node_Id;

      procedure Append_Node_Id (This    : in out Current_Ids_T;
                                Node_Id : in     Node_Index_T) is
      begin
         Append (This.Node_Ids, (Id      => Node_Construct,
                                 Node_Id => Node_Id));
      end Append_Node_Id;

      procedure Append_Array_Id (This     : in out Current_Ids_T;
                                 Array_Id : in     Array_Index_T) is
      begin
         Append (This.Node_Ids, (Id       => Array_Construct,
                                 Array_Id => Array_Id));
      end Append_Array_Id;

   end Current_Ids_Def;

   package body Max_Indices_Def is

      procedure Allocate_Node_Id (This : in out T;
                                  Id   : out Node_Index_T) is
      begin
         This.My_Node_Id_Max := This.My_Node_Id_Max + 1;
         Id := This.My_Node_Id_Max;
      end Allocate_Node_Id;

      procedure Allocate_Array_Id (This : in out T;
                                   Id   : out Array_Index_T) is
      begin
         This.My_Array_Id_Max := This.My_Array_Id_Max + 1;
         Id := This.My_Array_Id_Max;
      end Allocate_Array_Id;

      procedure Clear (This : in out T) is
         pragma Unreferenced (This);
      begin
         This.My_Node_Id_Max  := 0;
         This.My_Array_Id_Max := 0;
      end Clear;

   end Max_Indices_Def;

   procedure Start_Object (This        : in out Public_Part_Def.Public_Part_T;
                           Max_Indices : in out Max_Indices_T;
                           Arg3        : in out Arg3_T;
                           Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);
   begin
      case Current_Ids.State is
         when Expecting_Object_Start =>
            if
              Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
              Last_Index (Current_Ids.Node_Ids) < Max_Index (Current_Ids.Node_Ids)
            then
               declare
                  Node_Id : Node_Index_T;
               begin
                  Max_Indices.Allocate_Node_Id (Node_Id);
                  Current_Ids.Append_Node_Id (Node_Id);
               end;

               Current_Ids.State := Expecting_Key_Or_Object_End_After_Object_Start;
            else
               Call_Result.Initialize (-1009145668, 1145949162);
            end if;
         when Expecting_Value =>
            if
              Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) and
              Last_Index (Current_Ids.Node_Ids) < Max_Index (Current_Ids.Node_Ids)
            then
               declare
                  Node_Id : Node_Index_T;
               begin
                  Max_Indices.Allocate_Node_Id (Node_Id);

                  case Last_Element (Current_Ids.Node_Ids).Id is
                     when Node_Construct =>
                        pragma Assert (not This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value'Constrained);

                        This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id      => JSON_Object,
                                                                                                   Node_Id => Node_Id);
                     when Array_Construct =>
                        if Max_Indices.Array_Id_Max < Array_Index_T'Last then
                           declare
                              Array_Id : Array_Index_T;
                           begin
                              Max_Indices.Allocate_Array_Id (Array_Id);

                              pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                              This.Arrays (Array_Id).My_JSON_Value := (Id      => JSON_Object,
                                                                       Node_Id => Node_Id);
                              This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_Next := Array_Id;

                              Replace_Last_Element (Current_Ids.Node_Ids, ((Id       => Array_Construct,
                                                                            Array_Id => Array_Id)));
                           end;
                        else
                           Call_Result.Initialize (-1740465586, 0780682940);
                        end if;
                  end case;

                  Current_Ids.Append_Node_Id (Node_Id);
               end;

               Current_Ids.State := Expecting_Key_Or_Object_End_After_Object_Start;
            else
               Call_Result.Initialize (-0170327793, 0219201593);
            end if;
         when Expecting_Array_Value_After_Array_Start =>
            if
              Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) and
              Last_Index (Current_Ids.Node_Ids) < Max_Index (Current_Ids.Node_Ids)
            then
               declare
                  Node_Id : Node_Index_T;
               begin
                  Max_Indices.Allocate_Node_Id (Node_Id);

                  case Last_Element (Current_Ids.Node_Ids).Id is
                     when Node_Construct =>
                        Call_Result.Initialize (-0567777302, 0417247411);
                     when Array_Construct =>
                        pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                        This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value := (Id      => JSON_Object,
                                                                                                     Node_Id => Node_Id);
                        Current_Ids.Append_Node_Id (Node_Id);
                  end case;
               end;

               Current_Ids.State := Expecting_Key_Or_Object_End_After_Object_Start;
            else
               Call_Result.Initialize (1102643068, -1354814457);
            end if;
         when Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              End_State =>
            Call_Result.Initialize (2125265128, 1677007664);
      end case;
   end Start_Object;

   procedure End_Object (This        : in out Public_Part_Def.Public_Part_T;
                         Max_Indices : in out Max_Indices_T;
                         Arg3        : in out Arg3_T;
                         Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Max_Indices);
      pragma Unused (Arg3);
   begin
      case Current_Ids.State is
         when Expecting_Key_Or_Object_End | Expecting_Key_Or_Object_End_After_Object_Start =>
            if Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) then
               Delete_Last (Current_Ids.Node_Ids);

               if Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) then
                  case Last_Element (Current_Ids.Node_Ids).Id is
                     when Node_Construct  => Current_Ids.State := Expecting_Key_Or_Object_End;
                     when Array_Construct => Current_Ids.State := Expecting_Value;
                  end case;
               else
                  Current_Ids.State := End_State;
               end if;
            else
               Call_Result.Initialize (-1994021865, 0012512465);
            end if;
         when Expecting_Object_Start |
              Expecting_Value |
              Expecting_Array_Value_After_Array_Start |
              End_State =>
            Call_Result.Initialize (-1219882720, -2073561428);
      end case;
   end End_Object;

   procedure Key (This        : in out Public_Part_Def.Public_Part_T;
                  Max_Indices : in out Max_Indices_T;
                  Arg3        : in out Arg3_T;
                  Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                  Name        : Aida.String_T;
                  Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);

      Key : Int_To_String_Map.Key_T;
   begin
      case Current_Ids.State is
         when Expecting_Key_Or_Object_End =>
            if
              (Name'Length >= 1 and
                 This.Map.Available_Chars >= Name'Length and
                   This.Map.Available_Keys > 0 and
                     Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) and
                   Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
                     Last_Index (Current_Ids.Node_Ids) < Max_Index (Current_Ids.Node_Ids)) and then
              Last_Element (Current_Ids.Node_Ids).Id = Node_Construct
            then
               This.Map.Append (Value => Name,
                                Key   => Key);

               declare
                  Node_Id : Node_Index_T;
               begin
                  Max_Indices.Allocate_Node_Id (Node_Id);
                  This.Nodes (Node_Id).My_JSON_Key := Key;

                  This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_Next_Node := Node_Id;
                  Replace_Last_Element (Current_Ids.Node_Ids, ((Id      => Node_Construct,
                                                                Node_Id => Node_Id)));
               end;

               Current_Ids.State := Expecting_Value;
            else
               Call_Result.Initialize (-0036863206, 1757298512);
            end if;
         when Expecting_Key_Or_Object_End_After_Object_Start =>
            if
              (Name'Length >= 1 and
                 This.Map.Available_Chars >= Name'Length and
                   This.Map.Available_Keys > 0 and
                     Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)) and then
              Last_Element (Current_Ids.Node_Ids).Id = Node_Construct
            then
               This.Map.Append (Value => Name,
                                Key   => Key);
               This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Key := Key;
               Current_Ids.State := Expecting_Value;
            else
               Call_Result.Initialize (-1008424990, 0342344471);
            end if;
         when Expecting_Object_Start |
              Expecting_Value |
              Expecting_Array_Value_After_Array_Start |
              End_State =>
            Call_Result.Initialize (-0797629840, -2042971987);
      end case;
   end Key;

   procedure String_Value (This        : in out Public_Part_Def.Public_Part_T;
                           Max_Indices : in out Max_Indices_T;
                           Arg3        : in out Arg3_T;
                           Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                           Value       : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);

      Key : Int_To_String_Map.Key_T;
   begin
      case Current_Ids.State is
         when Expecting_Value =>
            if
              Value'Length >= 1 and
              This.Map.Available_Chars >= Value'Length and
              This.Map.Available_Keys > 0 and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               This.Map.Append (Value => Value,
                                Key   => Key);

               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
--                     if Last_Element (Current_Ids.Node_Ids).Node_Id < Node_Index_T'Last then -- TODO: Remove if statement?
                        This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id  => JSON_Text,
                                                                                                   Key => Key);
--                       else
--                          Call_Result.Initialize (0390188723, -2098347786);
--                       end if;

                     Current_Ids.State := Expecting_Key_Or_Object_End;
                  when Array_Construct =>
                     if Max_Indices.Array_Id_Max < Array_Index_T'Last then
                        declare
                           Array_Id : Array_Index_T;
                        begin
                           Max_Indices.Allocate_Array_Id (Array_Id);

                           pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                           This.Arrays (Array_Id).My_JSON_Value := (Id  => JSON_Text,
                                                                    Key => Key);
                           This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_Next := Array_Id;

                           Replace_Last_Element (Current_Ids.Node_Ids, ((Id       => Array_Construct,
                                                                         Array_Id => Array_Id)));
                        end;

                        Current_Ids.State := Expecting_Value;
                     else
                        Call_Result.Initialize (0057564449, 1521149725);
                     end if;
               end case;
            else
               Call_Result.Initialize (-1300101017, -2051786091);
            end if;
         when Expecting_Array_Value_After_Array_Start =>
            if
              Value'Length >= 1 and
              This.Map.Available_Chars >= Value'Length and
              This.Map.Available_Keys > 0 and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     Call_Result.Initialize (0050575217, 2066756199);
                  when Array_Construct =>
                     if Last_Element (Current_Ids.Node_Ids).Array_Id < Array_Index_T'Last then
                        This.Map.Append (Value => Value,
                                         Key   => Key);
                        This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value := (Id  => JSON_Text,
                                                                                                     Key => Key);
                     else
                        Call_Result.Initialize (1856910549, -0283523795);
                     end if;
               end case;

               Current_Ids.State := Expecting_Value;
            else
               Call_Result.Initialize (-0308019407, 1704305907);
            end if;
         when Expecting_Object_Start |
              Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              End_State =>
            Call_Result.Initialize (-0448826664, 0300129095);
      end case;
   end String_Value;

   procedure Integer_Value (This        : in out Public_Part_Def.Public_Part_T;
                            Max_Indices : in out Max_Indices_T;
                            Arg3        : in out Arg3_T;
                            Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                            Value       : in     Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);

      Key : Int_To_String_Map.Key_T;
   begin
      case Current_Ids.State is
         when Expecting_Value =>
            if
              Value'Length >= 1 and
              This.Map.Available_Chars >= Value'Length and
              This.Map.Available_Keys > 0 and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               This.Map.Append (Value => Value,
                                Key   => Key);

               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id  => JSON_Integer,
                                                                                                Key => Key);

                     Current_Ids.State := Expecting_Key_Or_Object_End;
                  when Array_Construct =>
                     if Max_Indices.Array_Id_Max < Array_Index_T'Last then
                        declare
                           Array_Id : Array_Index_T;
                        begin
                           Max_Indices.Allocate_Array_Id (Array_Id);

                           pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                           This.Arrays (Array_Id).My_JSON_Value := (Id  => JSON_Integer,
                                                                    Key => Key);
                           This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_Next := Array_Id;

                           Replace_Last_Element (Current_Ids.Node_Ids, ((Id       => Array_Construct,
                                                                         Array_Id => Array_Id)));
                        end;

                        Current_Ids.State := Expecting_Value;
                     else
                        Call_Result.Initialize (0940180052, -0596365545);
                     end if;
               end case;
            else
               Call_Result.Initialize (-0036863206, 1757298512);
            end if;
         when Expecting_Array_Value_After_Array_Start =>
            if
              Value'Length >= 1 and
              This.Map.Available_Chars >= Value'Length and
              This.Map.Available_Keys > 0 and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               This.Map.Append (Value => Value,
                                Key   => Key);

               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     Call_Result.Initialize (-1116089480, -1840821770);
                  when Array_Construct =>
                     pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                     This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value := (Id  => JSON_Integer,
                                                                                                  Key => Key);

                     Current_Ids.State := Expecting_Value;
               end case;
            else
               Call_Result.Initialize (0570112179, -1852002755);
            end if;
         when Expecting_Object_Start |
              Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              End_State =>
            Call_Result.Initialize (-0619940489, 0872953166);
      end case;
   end Integer_Value;

   procedure Real_Value (This        : in out Public_Part_Def.Public_Part_T;
                         Max_Indices : in out Max_Indices_T;
                         Arg3        : in out Arg3_T;
                         Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);

      Key : Int_To_String_Map.Key_T;
   begin
      case Current_Ids.State is
         when Expecting_Value =>
            if
              Value'Length >= 1 and
              This.Map.Available_Chars >= Value'Length and
              This.Map.Available_Keys > 0 and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               This.Map.Append (Value => Value,
                                Key   => Key);

               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id  => JSON_Integer,
                                                                                                Key => Key);

                     Current_Ids.State := Expecting_Key_Or_Object_End;
                  when Array_Construct =>
                     if Max_Indices.Array_Id_Max < Array_Index_T'Last then
                        declare
                           Array_Id : Array_Index_T;
                        begin
                           Max_Indices.Allocate_Array_Id (Array_Id);

                           pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                           This.Arrays (Array_Id).My_JSON_Value := (Id  => JSON_Real,
                                                                    Key => Key);
                           This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_Next := Array_Id;

                           Replace_Last_Element (Current_Ids.Node_Ids, ((Id       => Array_Construct,
                                                                         Array_Id => Array_Id)));
                        end;

                        Current_Ids.State := Expecting_Value;
                     else
                        Call_Result.Initialize (-1801369631, 2075621022);
                     end if;
               end case;
            else
               Call_Result.Initialize (-1613099809, -2137872755);
            end if;
         when Expecting_Array_Value_After_Array_Start =>
            if
              Value'Length >= 1 and
              This.Map.Available_Chars >= Value'Length and
              This.Map.Available_Keys > 0 and
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               This.Map.Append (Value => Value,
                                Key   => Key);

               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     Call_Result.Initialize (-0819941234, 1174478527);
                  when Array_Construct =>
                     pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                     This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value := (Id  => JSON_Real,
                                                                                                  Key => Key);

                     Current_Ids.State := Expecting_Value;
               end case;
            else
               Call_Result.Initialize (0415795942, -0293538010);
            end if;
         when Expecting_Object_Start |
              Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              End_State =>
            Call_Result.Initialize (-1960472466, 0176668249);
      end case;
   end Real_Value;

   procedure Boolean_Value (This        : in out Public_Part_Def.Public_Part_T;
                            Max_Indices : in out Max_Indices_T;
                            Arg3        : in out Arg3_T;
                            Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                            Value       : in     Boolean;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);
   begin
      case Current_Ids.State is
         when Expecting_Value =>
            if
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id      => JSON_Boolean,
                                                                                                Is_True => Value);

                     Current_Ids.State := Expecting_Key_Or_Object_End;
                  when Array_Construct =>
                     if Max_Indices.Array_Id_Max < Array_Index_T'Last then
                        declare
                           Array_Id : Array_Index_T;
                        begin
                           Max_Indices.Allocate_Array_Id (Array_Id);

                           pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                           This.Arrays (Array_Id).My_JSON_Value := (Id      => JSON_Boolean,
                                                                    Is_True => Value);
                           This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_Next := Array_Id;

                           Replace_Last_Element (Current_Ids.Node_Ids, ((Id       => Array_Construct,
                                                                         Array_Id => Array_Id)));
                        end;
                     else
                        Call_Result.Initialize (1359929063, 1175382196);
                     end if;
               end case;
            else
               Call_Result.Initialize (-2041731431, -1617070961);
            end if;
         when Expecting_Array_Value_After_Array_Start =>
            if
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id      => JSON_Boolean,
                                                                                                Is_True => Value);

                     Current_Ids.State := Expecting_Key_Or_Object_End;
                  when Array_Construct =>
                     pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                     This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value := (Id      => JSON_Boolean,
                                                                                                  Is_True => Value);
               end case;
            else
               Call_Result.Initialize (2020924097, 0707662188);
            end if;
         when Expecting_Object_Start |
              Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              End_State =>
            Call_Result.Initialize (1940144865, 1043910129);
      end case;
   end Boolean_Value;

   procedure Null_Value (This        : in out Public_Part_Def.Public_Part_T;
                         Max_Indices : in out Max_Indices_T;
                         Arg3        : in out Arg3_T;
                         Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);
   begin
      case Current_Ids.State is
         when Expecting_Value =>
            if
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id => JSON_Null);

                     Current_Ids.State := Expecting_Key_Or_Object_End;
                 when Array_Construct =>
                     if Max_Indices.Array_Id_Max < Array_Index_T'Last then
                     declare
                        Array_Id : Array_Index_T;
                     begin
                        Max_Indices.Allocate_Array_Id (Array_Id);

                        pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                        This.Arrays (Array_Id).My_JSON_Value := (Id => JSON_Null);
                        This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_Next := Array_Id;

                        Replace_Last_Element (Current_Ids.Node_Ids, ((Id       => Array_Construct,
                                                                      Array_Id => Array_Id)));
                        end;
                     else
                        Call_Result.Initialize (0666213217, 1667042557);
                     end if;
               end case;
            else
               Call_Result.Initialize (-2043468213, 1486043026);
            end if;
         when Expecting_Array_Value_After_Array_Start =>
            if
              Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
            then
               case Last_Element (Current_Ids.Node_Ids).Id is
                  when Node_Construct =>
                     Call_Result.Initialize (-0533702430, 1758897363);
                  when Array_Construct =>
                     pragma Assert (not This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value'Constrained);

                     This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value := (Id => JSON_Null);
               end case;
            else
               Call_Result.Initialize (1165054440, -0809031016);
            end if;
         when Expecting_Object_Start |
              Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              End_State =>
            Call_Result.Initialize (-2117690582, 1141092830);
      end case;
   end Null_Value;

   procedure Array_Start (This        : in out Public_Part_Def.Public_Part_T;
                          Max_Indices : in out Max_Indices_T;
                          Arg3        : in out Arg3_T;
                          Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unused (Arg3);
   begin
      case Current_Ids.State is
         when Expecting_Value =>
            if
              (Max_Indices.Array_Id_Max < Extended_Array_Id_T'Last and
                 Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)) and
              Last_Index (Current_Ids.Node_Ids) < Max_Index (Current_Ids.Node_Ids)
            then
               declare
                  Array_Id : Array_Index_T;
               begin
                  Max_Indices.Allocate_Array_Id (Array_Id);

                  case Last_Element (Current_Ids.Node_Ids).Id is
                     when Node_Construct =>
                        This.Nodes (Last_Element (Current_Ids.Node_Ids).Node_Id).My_JSON_Value := (Id       => JSON_Array,
                                                                                                   Array_Id => Array_Id);

                     when Array_Construct => null;
                        This.Arrays (Last_Element (Current_Ids.Node_Ids).Array_Id).My_JSON_Value := (Id       => JSON_Array,
                                                                                                     Array_Id => Array_Id);
                  end case;

                  Current_Ids.Append_Array_Id (Array_Id);
               end;

               Current_Ids.State := Expecting_Array_Value_After_Array_Start;
            else
               Call_Result.Initialize (-1612726960, 1239521507);
            end if;
         when Expecting_Object_Start |
              Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              Expecting_Array_Value_After_Array_Start | -- TODO: Fix this!
              End_State =>
            Call_Result.Initialize (-2117690582, 1141092830);
      end case;
   end Array_Start;

   procedure Array_End (This        : in out Public_Part_Def.Public_Part_T;
                        Max_Indices : in out Max_Indices_T;
                        Arg3        : in out Arg3_T;
                        Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                        Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Max_Indices);
      pragma Unused (Arg3);
   begin
      case Current_Ids.State is
         when Expecting_Value | Expecting_Array_Value_After_Array_Start =>
            if Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) then
               Delete_Last (Current_Ids.Node_Ids);
               if Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) then
                  case Last_Element (Current_Ids.Node_Ids).Id is
                     when Node_Construct  => Current_Ids.State := Expecting_Key_Or_Object_End;
                     when Array_Construct => Current_Ids.State := Expecting_Value;
                  end case;
               else
                  Call_Result.Initialize (-0663861421, -0728092707);
               end if;
            end if;
         when Expecting_Object_Start |
              Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              End_State =>
            Call_Result.Initialize (-0138993184, -0084170657);
      end case;
   end Array_End;

   procedure Parse_JSON is new Aida.JSON_SAX_Parse (Public_Part_Def.Public_Part_T,
                                                    Max_Indices_Def.T,
                                                    Arg3_T,
                                                    Current_Ids_Def.Current_Ids_T,
                                                    Start_Object,
                                                    End_Object,
                                                    Key,
                                                    String_Value,
                                                    Integer_Value,
                                                    Real_Value,
                                                    Boolean_Value,
                                                    Null_Value,
                                                    Array_Start,
                                                    Array_End);

   procedure Parse (This         : in out T;
                    JSON_Message : String_T;
                    Call_Result  : in out Aida.Subprogram_Call_Result.T)
   is
      Max_Indices : Max_Indices_Def.T;

      Current_Ids : Current_Ids_Def.Current_Ids_T;

      Arg3 : Arg3_T := Dummy_Value;
   begin
      Max_Indices.Clear;

      Parse_JSON (Public_Part_Def.Public_Part_T (This),
                  Max_Indices,
                  Arg3,
                  Current_Ids,
                  JSON_Message,
                  Call_Result);

      pragma Unused (Max_Indices);
      pragma Unused (Current_Ids);
      pragma Unused (Arg3);
   end Parse;

end Aida.JSON_DOM_Parser;
