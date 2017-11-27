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

               Current_Ids.State := Expecting_Length_Keyword;
            else
               Call_Result.Initialize (-1131940586, -1869733641);
            end if;
         when Expecting_Length_Keyword |
              Expecting_Length_Float |
              Expecting_Object_End |
              End_State =>
            Call_Result.Initialize (-0937105173, 1809300858);
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
   begin
      if Current_Ids.State = Expecting_Object_End then
         Current_Ids.State := End_State;
      else
         Call_Result.Initialize (-1088171163, 0366088626);
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
      pragma Unreferenced (Max_Indices);
   begin
      if
        Current_Ids.State = Expecting_Length_Keyword and then
        Name = "length"
      then
         Current_Ids.State := Expecting_Length_Float;
      else
         Call_Result.Initialize (-1277436309, -1289320473);
      end if;
   end Key;

   procedure String_Value (Result      : in out Node_Array_T;
                           Max_Indices : in out Max_Indices_T;
                           Map         : in out Int_To_String_Map.T;
                           Current_Ids : in out Current_Ids_T;
                           Value       : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Map);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
   begin
      Call_Result.Initialize (0973362602, 1877007627);
   end String_Value;

   procedure Integer_Value (Storage     : in out Node_Array_T;
                            Max_Indices : in out Max_Indices_T;
                            Map         : in out Int_To_String_Map.T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : in     Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Storage);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Map);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
   begin
      Call_Result.Initialize (0836080939, 0159783088);
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
            Call_Result.Initialize (1841755090, -1014383624);
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
      Call_Result.Initialize (-0755111556, 0751766857);
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
      Call_Result.Initialize (1558346114, -0799263304);
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
      Call_Result.Initialize (1729178755, 1696906378);
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
      Call_Result.Initialize (1329123355, -0441958475);
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
   end Parse;

end Aida.JSON.Generic_DOM_Parser;
