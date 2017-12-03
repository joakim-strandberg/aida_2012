with Aida.JSON.Generic_Parse_JSON;

package body Aida.JSON.Generic_DOM_Parser is

   use all type Node_Id_Vector.T;

   package body Max_Indices_Def is

      procedure Allocate_Node_Id (This : in out T;
                                  Id   : out Node_Index_T) is
      begin
         This.My_Node_Id_Max := This.My_Node_Id_Max + 1;
         Id := This.My_Node_Id_Max;
      end Allocate_Node_Id;

      procedure Clear (This : in out T) is
         pragma Unreferenced (This);
      begin
         This.My_Node_Id_Max  := 0;
      end Clear;

   end Max_Indices_Def;

   procedure Start_Object (Storage     : in out Node_Array_T;
                           Max_Indices : in out Max_Indices_T;
                           Map         : in out Int_To_String_Map.T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Storage);
      pragma Unmodified (Map);
   begin
      case Current_Ids.State is
         when Expecting_Object_Start                    =>
            if
              Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
              Last_Index (Current_Ids.Node_Ids) < Max_Index (Current_Ids.Node_Ids)
            then
               declare
                  Node_Id : Node_Index_T;
               begin
                  Max_Indices.Allocate_Node_Id (Node_Id);
                  Append (Current_Ids.Node_Ids, Node_Id);
               end;

               Current_Ids.State := Expecting_Key_Or_Object_End_After_Object_Start;
            else
               Call_Result.Initialize (-1009145668, 1145949162);
            end if;
         when Expecting_Key_Or_Object_End |
              Expecting_Key_Or_Object_End_After_Object_Start |
              Expecting_Value |
              End_State =>
            Call_Result.Initialize (2125265128, 1677007664);
      end case;
   end Start_Object;

   procedure End_Object (Result      : in out Node_Array_T;
                         Max_Indices : in out Max_Indices_T;
                         Map         : in out Int_To_String_Map.T;
                         Current_Ids : in out Current_Ids_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Max_Indices);
      pragma Unmodified (Map);
   begin
      if
        Current_Ids.State = Expecting_Key_Or_Object_End or
        Current_Ids.State = Expecting_Key_Or_Object_End_After_Object_Start
      then
         if Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) then
            Current_Ids.State := Expecting_Key_Or_Object_End;
            Delete_Last (Current_Ids.Node_Ids);
         else
            Current_Ids.State := End_State;
         end if;
      else
         Call_Result.Initialize (-1994021865, 0012512465);
      end if;
   end End_Object;

   procedure Key (Result      : in out Node_Array_T;
                  Max_Indices : in out Max_Indices_T;
                  Map         : in out Int_To_String_Map.T;
                  Current_Ids : in out Current_Ids_T;
                  Name        : Aida.String_T;
                  Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);

      Key : Int_To_String_Map.Key_T;
   begin
      if Current_Ids.State = Expecting_Key_Or_Object_End then
         if
           Name'Length >= 1 and
           Map.Available_Chars >= Name'Length and
           Map.Available_Keys > 0 and
           Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids) and
           Max_Indices.Node_Id_Max < Extended_Node_Id_T'Last and
           Last_Index (Current_Ids.Node_Ids) < Max_Index (Current_Ids.Node_Ids)
         then
            Map.Append (Value => String (Name),
                        Key   => Key);

            declare
               Node_Id : Node_Index_T;
            begin
               Max_Indices.Allocate_Node_Id (Node_Id);
               Result (Node_Id).My_JSON_Key := Key;

               Result (Last_Element (Current_Ids.Node_Ids)).My_Next_Node := Node_Id;
               Replace_Last_Element (Current_Ids.Node_Ids, Node_Id);
            end;

            Current_Ids.State := Expecting_Value;
         else
            Call_Result.Initialize (-0036863206, 1757298512);
         end if;
      elsif Current_Ids.State = Expecting_Key_Or_Object_End_After_Object_Start then
         if
           Name'Length >= 1 and
           Map.Available_Chars >= Name'Length and
           Map.Available_Keys > 0 and
           Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
         then
            Map.Append (Value => String (Name),
                        Key   => Key);
            Result (Last_Element (Current_Ids.Node_Ids)).My_JSON_Key := Key;
            Current_Ids.State := Expecting_Value;
         else
            Call_Result.Initialize (-1008424990, 0342344471);
         end if;
      else
         Call_Result.Initialize (-0797629840, -2042971987);
      end if;
   end Key;

   procedure String_Value (Result      : in out Node_Array_T;
                           Max_Indices : in out Max_Indices_T;
                           Map         : in out Int_To_String_Map.T;
                           Current_Ids : in out Current_Ids_T;
                           Value       : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unmodified (Max_Indices);

      Key : Int_To_String_Map.Key_T;
   begin
      if Current_Ids.State = Expecting_Value then
         if
           (Value'Length >= 1 and
           Map.Available_Chars >= Value'Length and
           Map.Available_Keys > 0 and
           Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)) and then
           Last_Element (Current_Ids.Node_Ids) < Node_Index_T'Last
         then
            Map.Append (Value => String (Value),
                        Key   => Key);
            Result (Last_Element (Current_Ids.Node_Ids)).My_JSON_Value := (
                                                                           Id  => JSON_Text,
                                                                           Key => Key
                                                                          );

            Current_Ids.State := Expecting_Key_Or_Object_End;
         else
            Call_Result.Initialize (-1300101017, -2051786091);
         end if;
      else
         Call_Result.Initialize (-0448826664, 0300129095);
      end if;
   end String_Value;

   procedure Integer_Value (Storage     : in out Node_Array_T;
                            Max_Indices : in out Max_Indices_T;
                            Map         : in out Int_To_String_Map.T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : in     Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unmodified (Max_Indices);

      Key : Int_To_String_Map.Key_T;
   begin
      if Current_Ids.State = Expecting_Value then
         if
           Value'Length >= 1 and
           Map.Available_Chars >= Value'Length and
           Map.Available_Keys > 0 and
           Last_Index (Current_Ids.Node_Ids) >= First_Index (Current_Ids.Node_Ids)
         then
            if Last_Element (Current_Ids.Node_Ids) < Node_Index_T'Last then


               Map.Append (Value => String (Value),
                           Key   => Key);
               Storage (Last_Element (Current_Ids.Node_Ids)).My_JSON_Value := (
                                                                               Id  => JSON_Integer,
                                                                               Key => Key
                                                                              );
               Current_Ids.State := Expecting_Key_Or_Object_End;
            else
               Call_Result.Initialize (0940180052, -0596365545);
            end if;
         else
            Call_Result.Initialize (-0036863206, 1757298512);
         end if;
      else
         Call_Result.Initialize (-0619940489, 0872953166);
      end if;
   end Integer_Value;

   procedure Real_Value (Storage     : in out Node_Array_T;
                         Max_Indices : in out Max_Indices_T;
                         Map         : in out Int_To_String_Map.T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Storage);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Map);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
   begin
--        case State is
--           when Expecting_Length_Float   =>
--              declare
--                 V : Aida.Float_T;
--                 Has_Failed : Boolean;
--              begin
--                 To_Float (Value, V, Has_Failed);
--
--                 if Has_Failed then
--                    Call_Result.Initialize (1713066840, 1748338706);
--                 else
--                    if
--                      Is_Empty (Current_Ids.Node_Ids)
--                    then
--                       Call_Result.Initialize (1885775356, 1741885967);
--                    else
--                       declare
--                          Node_Id : Node_Index_T renames Last_Element (Current_Ids.Node_Ids);
--                       begin
--  --                        Storage (Node_Id).Length := Json_Parsing_Tests_Model.Person_Def.Length_T (V);
--                          State := Expecting_Object_End;
--                       end;
--                    end if;
--                 end if;
--              end;
--           when Expecting_Object_Start |
--                Expecting_Length_Keyword |
--                Expecting_Object_End |
--                End_State =>
            Call_Result.Initialize (-1960472466, 0176668249);
--        end case;
   end Real_Value;

   procedure Boolean_Value (Storage     : in out Node_Array_T;
                            Max_Indices : in out Max_Indices_T;
                            Map         : in out Int_To_String_Map.T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : in     Boolean;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Storage);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Map);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
   begin
      Call_Result.Initialize (1940144865, 1043910129);
   end Boolean_Value;

   procedure Null_Value (Storage     : in out Node_Array_T;
                         Max_Indices : in out Max_Indices_T;
                         Map         : in out Int_To_String_Map.T;
                         Current_Ids : in out Current_Ids_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Storage);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Map);
      pragma Unreferenced (Current_Ids);
   begin
      Call_Result.Initialize (-2117690582, 1141092830);
   end Null_Value;

   procedure Array_Start (Result      : in out Node_Array_T;
                          Max_Indices : in out Max_Indices_T;
                          Map         : in out Int_To_String_Map.T;
                          Current_Ids : in out Current_Ids_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Map);
   begin
      Call_Result.Initialize (0146723155, -0285464918);
   end Array_Start;

   procedure Array_End (Result      : in out Node_Array_T;
                        Max_Indices : in out Max_Indices_T;
                        Map         : in out Int_To_String_Map.T;
                        Current_Ids : in out Current_Ids_T;
                        Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Map);
   begin
      Call_Result.Initialize (-0138993184, -0084170657);
   end Array_End;

   procedure Parse_JSON is new Aida.JSON.Generic_Parse_JSON (Node_Array_T,
                                                             Max_Indices_Def.T,
                                                             Int_To_String_Map.T,
                                                             Current_Ids_T,
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

      Current_Ids : Current_Ids_T;
   begin
      Max_Indices.Clear;

      Parse_JSON (This.Nodes,
                  Max_Indices,
                  This.Map,
                  Current_Ids,
                  JSON_Message,
                  Call_Result);

      pragma Unused (Max_Indices);
      pragma Unused (Current_Ids);
   end Parse;

end Aida.JSON.Generic_DOM_Parser;
