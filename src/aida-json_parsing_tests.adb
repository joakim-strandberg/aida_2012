with Aida.JSON_SAX_Parse;
with Aida.Subprogram_Call_Result;

package body Aida.JSON_Parsing_Tests is

   use type Json_Parsing_Tests_Model.Extended_Person_Id_T;
   use type Json_Parsing_Tests_Model.Extended_Hand_Id_T;
   use type Json_Parsing_Tests_Model.Extended_Vehicle_Id_T;
   use type Json_Parsing_Tests_Model.Person_Def.Age_T;
   use type Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T;
   use type Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T;

   -- The trailing numbers are to differentiate between the same json except different number of spaces
   JSON_Test_Person_With_Age_0            : constant Aida.String_T := "{""age"" : 10}";
   JSON_Test_Person_With_Age_1            : constant Aida.String_T := "{""age"" : 10 }";
   JSON_Test_Person_With_Hand_0           : constant Aida.String_T := "{""hand"" : { ""fingers"" : 4 }}";
   JSON_Test_Person_With_Name_Adam_0      : constant Aida.String_T := "{""name"" : ""adam""}";
   JSON_Test_Person_With_Name_Adam_1      : constant Aida.String_T := "   {""name"" : ""adam""}";
   JSON_Test_Person_With_Name_And_Age_0   : constant Aida.String_T := "{""name"" : ""bertil"", ""age"" : 5}";
   JSON_Test_Person_With_Vehicles_0       : constant Aida.String_T := "{""vehicles"" : [ {""wheels"" : 4 }, {""wheels"" : 2 } ]}";
   JSON_Test_Person_With_Length_0         : constant Aida.String_T := "{""length"" : 1.98}";
   JSON_Test_Person_With_Is_Happy_True_0  : constant Aida.String_T := "{""isHappy"" : true}";
   JSON_Test_Person_With_Is_Happy_False_0 : constant Aida.String_T := "{""isHappy"" : false}";
   JSON_Test_Person_With_Is_Happy_Null_0  : constant Aida.String_T := "{""isHappy"" : null}";

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON_SAX_Parse package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_1'Access, "Test_Person_With_Age_1");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_0'Access, "Test_Person_With_Name_Adam_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_1'Access, "Test_Person_With_Name_Adam_1");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Length_0'Access, "Test_Person_With_Length_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_0'Access, "Test_Person_With_Hand_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Vehicles_0'Access, "Test_Person_With_Vehicles_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_And_Age_0'Access, "Test_Person_With_Name_And_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_True_0'Access, "Test_Person_With_Is_Happy_True_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_False_0'Access, "Test_Person_With_Is_Happy_False_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_Null_0'Access, "Test_Person_With_Is_Happy_Null_0");
   end Initialize;

   type Current_Ids_T is limited record
      Person_Ids  : Person_Id_Vector.T;
      Hand_Ids    : Hand_Id_Vector.T;
      Vehicle_Ids : Vehicle_Id_Vector.T;
   end record;

   type Unused_State_T is (
                           Default_State,
                           End_Of_Json_Object_Reached
                          );

   procedure Unused_Value_String (Result      : in out Storage_T;
                                  Max_Indices : in out Max_Indices_T;
                                  State       : in out Unused_State_T;
                                  Current_Ids : in out Current_Ids_T;
                                  Value       : Aida.String_T;
                                  Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null;

   procedure Unused_Value_String (Result      : in out Storage_T;
                                  Max_Indices : in out Max_Indices_T;
                                  State       : in out Unused_State_T;
                                  Current_Ids : in out Current_Ids_T;
                                  Value       : Aida.String_T;
                                  Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
   begin
      Call_Result.Initialize (-0845450877, 0860763139);
   end Unused_Value_String;

   procedure Unused_Value_Integer (Result      : in out Storage_T;
                                   Max_Indices : in out Max_Indices_T;
                                   State       : in out Unused_State_T;
                                   Current_Ids : in out Current_Ids_T;
                                   Value       : Aida.String_T;
                                   Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null;

   procedure Unused_Value_Integer (Result      : in out Storage_T;
                                   Max_Indices : in out Max_Indices_T;
                                   State       : in out Unused_State_T;
                                   Current_Ids : in out Current_Ids_T;
                                   Value       : Aida.String_T;
                                   Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Value);
   begin
      Call_Result.Initialize (-0348997721, 1611178481);
   end Unused_Value_Integer;

   procedure Unused_Real_Value (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Value       : Aida.String_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null;

   procedure Unused_Real_Value (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Value       : Aida.String_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Value);
   begin
      Call_Result.Initialize (0979433843, 1294565993);
   end Unused_Real_Value;

   procedure Unused_Boolean_Value (Result      : in out Storage_T;
                                   Max_Indices : in out Max_Indices_T;
                                   State       : in out Unused_State_T;
                                   Current_Ids : in out Current_Ids_T;
                                   Value       : in     Boolean;
                                   Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null;

   procedure Unused_Boolean_Value (Result      : in out Storage_T;
                                   Max_Indices : in out Max_Indices_T;
                                   State       : in out Unused_State_T;
                                   Current_Ids : in out Current_Ids_T;
                                   Value       : in     Boolean;
                                   Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Value);
   begin
      Call_Result.Initialize (-0024574236, -1975535361);
   end Unused_Boolean_Value;

   procedure Unused_Null_Value (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null;

   procedure Unused_Null_Value (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
   begin
      Call_Result.Initialize (0420358725, -2067049251);
   end Unused_Null_Value;

   procedure Unused_Array_Start (Result      : in out Storage_T;
                                 Max_Indices : in out Max_Indices_T;
                                 State       : in out Unused_State_T;
                                 Current_Ids : in out Current_Ids_T;
                                 Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null;

   procedure Unused_Array_Start (Result      : in out Storage_T;
                                 Max_Indices : in out Max_Indices_T;
                                 State       : in out Unused_State_T;
                                 Current_Ids : in out Current_Ids_T;
                                 Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
   begin
      Call_Result.Initialize (-2052576866, 0063903515);
   end Unused_Array_Start;

   procedure Unused_Array_End (Result      : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null;

   procedure Unused_Array_End (Result      : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
   begin
      Call_Result.Initialize (1781041313, 0812552885);
   end Unused_Array_End;

   package Test_Person_With_Name_Adam_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Name_Adam_Utils;

   package body Test_Person_With_Name_Adam_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
      begin
         if
           Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
           Current_Ids.Person_Ids.Last_Index < Current_Ids.Person_Ids.Max_Index
         then
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
            begin
               Max_Indices.Allocate_Person_Id (Person_Id);
               Current_Ids.Person_Ids.Append (Person_Id);
            end;
         else
            Call_Result.Initialize (0313390224, -1612869688);
         end if;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Default_State then
            State := End_Of_Json_Object_Reached;
         else
            Call_Result.Initialize (-1912053429, 0608638866);
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         if Name /= "name" then
            Call_Result.Initialize (-1611290092, -1907219884);
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
            Call_Result.Initialize (-0630434603, -1681577383);
         else
            if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
               Call_Result.Initialize (-1216758070, 0245459309);
            else
               Result.Person (Current_Ids.Person_Ids.Last_Element).Set_Name (Value);
            end if;
         end if;
      end Value_String;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         Unused_State_T,
                                                         Current_Ids_T,
                                                         Test_Person_With_Name_Adam_Utils.Root_Start_Tag,
                                                         Test_Person_With_Name_Adam_Utils.Root_End_Tag,
                                                         Test_Person_With_Name_Adam_Utils.Key_Name,
                                                         Test_Person_With_Name_Adam_Utils.Value_String,
                                                         Unused_Value_Integer,
                                                         Unused_Real_Value,
                                                         Unused_Boolean_Value,
                                                         Unused_Null_Value,
                                                         Unused_Array_Start,
                                                         Unused_Array_End);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : Unused_State_T := Default_State;

         Max_Indices : Json_Parsing_Tests_Model.Max_Indices_Def.T;

         Current_Ids : Current_Ids_T;
      begin
         Max_Indices.Clear;

         Parse_XML (Storage,
                    Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (State = End_Of_Json_Object_Reached, "dd1327b2-c0d2-4414-87f5-8a35d0a94d6f");
         Ahven.Assert (Current_Ids.Person_Ids.Last_Index >= Current_Ids.Person_Ids.First_Index, "150be077-0e4c-43b2-901e-1ffc9f57bb76");
         Ahven.Assert (not Call_Result.Has_Failed, "5a84dd71-1bee-4e2c-b7f8-13915f953605");
         Ahven.Assert (Max_Indices.Person_Id_Max = 1, "87f1346a-607c-4e7a-8a3a-621365c323d9");
         Ahven.Assert (Max_Indices.Person_Id_Max > 0, "87f1346a-607c-4e7a-8a3a-621365c323d9");
         Ahven.Assert (Storage.Person (Max_Indices.Person_Id_Max).Name = "adam", "11bdb82b-275f-4432-87f0-33d27925d7b6");
      end Run_Test;

   end Test_Person_With_Name_Adam_Utils;

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Name_Adam_Utils.Run_Test (JSON_Test_Person_With_Name_Adam_0);
   end Test_Person_With_Name_Adam_0;

   procedure Test_Person_With_Name_Adam_1 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Name_Adam_Utils.Run_Test (JSON_Test_Person_With_Name_Adam_1);
   end Test_Person_With_Name_Adam_1;

   package Test_Person_With_Age_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Age_Utils;

   package body Test_Person_With_Age_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
      begin
         if
           Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
           Current_Ids.Person_Ids.Last_Index < Current_Ids.Person_Ids.Max_Index
         then
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
            begin
               Max_Indices.Allocate_Person_Id (Person_Id);
               Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
            end;
         else
            Call_Result.Initialize (-0919103745, 1750432710);
         end if;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Default_State then
            State := End_Of_Json_Object_Reached;
         else
            Call_Result.Initialize (0570840892, -1431870346);
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         if Name /= "age" then
            Call_Result.Initialize (1454931047, 1198562324);
         end if;
      end Key_Name;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);

         V : Aida.Int32_T;
         Has_Failed : Boolean;
      begin
         To_Int32 (Value, V, Has_Failed);

         if Has_Failed then
            Call_Result.Initialize (-2115808473, 0706780051);
         else
            if
              Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) or
              V not in Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'Last)
            then
               Call_Result.Initialize (0846449148, -1192649274);
            else
               declare
                  Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                    Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
               begin
                  Storage.Person (Person_Id).Age := Json_Parsing_Tests_Model.Person_Def.Age_T (V);
               end;
            end if;
         end if;
      end Value_Integer;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         Unused_State_T,
                                                         Current_Ids_T,
                                                         Root_Start_Tag,
                                                         Root_End_Tag,
                                                         Key_Name,
                                                         Unused_Value_String,
                                                         Value_Integer,
                                                         Unused_Real_Value,
                                                         Unused_Boolean_Value,
                                                         Unused_Null_Value,
                                                         Unused_Array_Start,
                                                         Unused_Array_End);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : Unused_State_T := Default_State;

         Current_Ids : Current_Ids_T;
      begin
         Aida.Json_Parsing_Tests_Model.Max_Indices.Clear;

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
         Ahven.Assert (State = End_Of_Json_Object_Reached, "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Current_Ids.Person_Ids.Last_Index >= Current_Ids.Person_Ids.First_Index, "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         Ahven.Assert (Storage.Person (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
      end Run_Test;

   end Test_Person_With_Age_Utils;

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (JSON_Test_Person_With_Age_0);
   end Test_Person_With_Age_0;

   procedure Test_Person_With_Age_1 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (JSON_Test_Person_With_Age_1);
   end Test_Person_With_Age_1;

   package Test_Person_With_Name_And_Age_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Name_And_Age_Utils;

   package body Test_Person_With_Name_And_Age_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
      begin
         if
           Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
           Current_Ids.Person_Ids.Last_Index < Current_Ids.Person_Ids.Max_Index
         then
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
            begin
               Max_Indices.Allocate_Person_Id (Person_Id);
               Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
            end;
         else
            Call_Result.Initialize (1969275028, -0062370286);
         end if;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Default_State then
            State := End_Of_Json_Object_Reached;
         else
            Call_Result.Initialize (-1355360737, -1089990248);
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         if Name = "name" or Name = "age" then
            null;
         else
            Call_Result.Initialize (-1152427876, -0525228364);
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
            Call_Result.Initialize (-0975421460, 1917346408);
         else
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                 Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
            begin
               if Value'Length > Result.Person (Person_Id).Max_Name_Size then
                  Call_Result.Initialize (1602718144, 0498843329);
               else
                  Result.Person (Person_Id).Set_Name (Value);
               end if;
            end;
         end if;
      end Value_String;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         V : Aida.Int32_T;
         Has_Failed : Boolean;
      begin
         To_Int32 (Value, V, Has_Failed);

         if Has_Failed then
            Call_Result.Initialize (1220688762, 1735324821);
         else
            if
              Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) or
              V not in Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'Last)
            then
               Call_Result.Initialize (2126631415, 1712278471);
            else
               declare
                  Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                    Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
               begin
                  Storage.Person (Person_Id).Age := Json_Parsing_Tests_Model.Person_Def.Age_T (V);
               end;
            end if;
         end if;
      end Value_Integer;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         Unused_State_T,
                                                         Current_Ids_T,
                                                         Root_Start_Tag,
                                                         Root_End_Tag,
                                                         Key_Name,
                                                         Value_String,
                                                         Value_Integer,
                                                         Unused_Real_Value,
                                                         Unused_Boolean_Value,
                                                         Unused_Null_Value,
                                                         Unused_Array_Start,
                                                         Unused_Array_End);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : Unused_State_T := Default_State;

         Current_Ids : Current_Ids_T;
      begin
         Aida.Json_Parsing_Tests_Model.Max_Indices.Clear;

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
         Ahven.Assert (State = End_Of_Json_Object_Reached, "4b9650d2-c30f-401a-a060-a0e039fe413c");
         Ahven.Assert (Current_Ids.Person_Ids.Last_Index >= Current_Ids.Person_Ids.First_Index, "bb095008-4756-4392-ac77-03799c82a947");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max = 1, "e095f887-42d1-4fbf-846c-75e32af16af6");
         Ahven.Assert (not Call_Result.Has_Failed, "decc36b9-2538-4cfb-8fa2-4fd7b240abd8");
         Ahven.Assert (Storage.Person (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max).Name = "bertil", "bc2f314b-46c2-4310-849e-93d3380f0cf3");
         Ahven.Assert (Storage.Person (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max).Age = 5, "0b99115c-279b-4b51-991f-4fafcb0fd21f");
      end Run_Test;

   end Test_Person_With_Name_And_Age_Utils;

   procedure Test_Person_With_Name_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Name_And_Age_Utils.Run_Test (JSON_Test_Person_With_Name_And_Age_0);
   end Test_Person_With_Name_And_Age_0;

   package Test_Person_With_Hand_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Object_Start,
                       Expecting_Hand_Keyword,
                       Expecting_Hand_Object_Start,
                       Expecting_Fingers_Keyword,
                       Expecting_Fingers_Value,
                       Expecting_Hand_Object_End,
                       Expecting_Object_End,
                       End_State
                      );

      procedure Root_Start_Tag (Storage     : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Real_Value (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Hand_Utils;

   package body Test_Person_With_Hand_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Storage     : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
      begin
         case State is
            when Expecting_Object_Start      =>
               if
                 Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Current_Ids.Person_Ids.Last_Index < Current_Ids.Person_Ids.Max_Index
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Max_Indices.Allocate_Person_Id (Person_Id);
                     Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
                  end;

                  State := Expecting_Hand_Keyword;
               else
                  Call_Result.Initialize (0963645767, -0975435440);
               end if;
            when Expecting_Hand_Object_Start =>
               if
               not Current_Ids.Person_Ids.Is_Empty and then
                 (Max_Indices.Hand_Id_Max < Json_Parsing_Tests_Model.Extended_Hand_Id_T'Last and
                      Current_Ids.Hand_Ids.Last_Index < Current_Ids.Hand_Ids.Max_Index and
                      Storage.Person (Current_Ids.Person_Ids.Last_Element).Hands.Last_Index < Storage.Person (Current_Ids.Person_Ids.Last_Element).Hands.Max_Index)
               then
                  declare
                     Hand_Id : Aida.Json_Parsing_Tests_Model.Hand_Id_T;
                  begin
                     Max_Indices.Allocate_Hand_Id (Hand_Id);
                     Hand_Id_Vector.Append (Current_Ids.Hand_Ids, Hand_Id);

                     Json_Parsing_Tests_Model.Person_Def.Hand_Vector.Append (Storage.Person (Person_Id_Vector.Last_Element (Current_Ids.Person_Ids)).Hands, Hand_Id);
                  end;

                  State := Expecting_Fingers_Keyword;
               else
                  Call_Result.Initialize (-1674331374, 2128229649);
               end if;
            when Expecting_Hand_Keyword |
                 Expecting_Fingers_Keyword |
                 Expecting_Fingers_Value |
                 Expecting_Hand_Object_End |
                 Expecting_Object_End |
                 End_State =>
               Call_Result.Initialize (1470753973, 1162502132);
         end case;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Expecting_Hand_Object_End then
            State := Expecting_Object_End;
         elsif State = Expecting_Object_End then
            State := End_State;
         else
            Call_Result.Initialize (0903752807, 2025920358);
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if
           State = Expecting_Hand_Keyword and then
           Name = "hand"
         then
            State := Expecting_Hand_Object_Start;
         elsif
           State = Expecting_Fingers_Keyword and then
           Name = "fingers"
         then
            State := Expecting_Fingers_Value;
         else
            Call_Result.Initialize (0173444107, 0390655871);
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-1874513743, -1947208265);
      end Value_String;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         V : Aida.Int32_T;
         Has_Failed : Boolean;
      begin
         To_Int32 (Value, V, Has_Failed);

         if Has_Failed then
            Call_Result.Initialize (0437172259, 0138260948);
         else
            if State = Expecting_Fingers_Value then
               if
                 Hand_Id_Vector.Is_Empty (Current_Ids.Hand_Ids) or
                 V not in Aida.Int32_T (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'Last)
               then
                  Call_Result.Initialize (1043954405, 0619872171);
               else
                  declare
                     Id : Aida.Json_Parsing_Tests_Model.Hand_Id_T renames
                       Hand_Id_Vector.Last_Element (Current_Ids.Hand_Ids);
                  begin
                     Storage.Hand (Id).Number_Of_Fingers := Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T (V);
                  end;

                  State := Expecting_Hand_Object_End;
               end if;
            else
               Call_Result.Initialize (-0763768917, 1627513178);
            end if;
         end if;
      end Value_Integer;

      procedure Real_Value (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-1556592998, 0751534135);
      end Real_Value;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0051019455, 1051443055);
      end Boolean_Value;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         Call_Result.Initialize (-1657720900, 0196188316);
      end Null_Value;

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
      begin
         Call_Result.Initialize (-1342221233, -1711751067);
      end Array_Start;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
      begin
         Call_Result.Initialize (0684008567, 0622549007);
      end Array_End;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         State_T,
                                                         Current_Ids_T,
                                                         Root_Start_Tag,
                                                         Root_End_Tag,
                                                         Key_Name,
                                                         Value_String,
                                                         Value_Integer,
                                                         Real_Value,
                                                         Boolean_Value,
                                                         Null_Value,
                                                         Array_Start,
                                                         Array_End);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Object_Start;

         Current_Ids : Current_Ids_T;
      begin
         Aida.Json_Parsing_Tests_Model.Max_Indices.Clear;

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
         Ahven.Assert (State = End_State, "397d359d-2d92-462b-8b32-2a4bbdc6ce25");
         Ahven.Assert (Current_Ids.Person_Ids.Last_Index >= Current_Ids.Person_Ids.First_Index, "810561fa-2c9f-4582-a5cf-10e5abd85113");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max = 1, "ae7399ea-3d2a-4400-a10f-34104d439978");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Hand_Id_Max = 1, "0b77dd49-3cbd-44cd-ab53-9b65d0d75c05");
         if
           (Json_Parsing_Tests_Model.Max_Indices.Hand_Id_Max > 0 and
                Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max > 0) and then (not Storage.Person (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max).Hands.Is_Empty)
         then
            declare
               Hand_Id : constant Json_Parsing_Tests_Model.Hand_Id_T :=
                 Storage.Person (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max).Hands.Last_Element;
            begin
               Ahven.Assert (Storage.Hand (Hand_Id).Number_Of_Fingers = 4, "bf757f75-1d4a-425c-9842-27e1f6de2841");
            end;
            null;
         end if;
      end Run_Test;

   end Test_Person_With_Hand_Utils;

   procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (JSON_Test_Person_With_Hand_0);
   end Test_Person_With_Hand_0;

   package Test_Person_With_Vehicles_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Object_Start,
                       Expecting_Vehicles_Keyword,
                       Expecting_Array_Start,
                       Expecting_Array_Object_Start_Or_Array_End,
                       Expecting_Wheels_Keyword,
                       Expecting_Wheels_Integer,
                       Expecting_Array_Object_End,
                       Expecting_Object_End,
                       End_State
                      );

      procedure Root_Start_Tag (Storage     : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Real_Value (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Vehicles_Utils;

   package body Test_Person_With_Vehicles_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Storage     : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
      begin
         case State is
            when Expecting_Object_Start                    =>
               if
                 Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Current_Ids.Person_Ids.Last_Index < Current_Ids.Person_Ids.Max_Index
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Max_Indices.Allocate_Person_Id (Person_Id);
                     Current_Ids.Person_Ids.Append (Person_Id);
                  end;

                  State := Expecting_Vehicles_Keyword;
               else
                  Call_Result.Initialize (0621579901, -0324134420);
               end if;
            when Expecting_Array_Object_Start_Or_Array_End =>
               if
               not Current_Ids.Person_Ids.Is_Empty and then
                 (
                  Storage.Person (Current_Ids.Person_Ids.Last_Element).Vehicles.Last_Index <
                      Storage.Person (Current_Ids.Person_Ids.Last_Element).Vehicles.Max_Index and
                      Max_Indices.Vehicle_Id_Max < Json_Parsing_Tests_Model.Extended_Vehicle_Id_T'Last and
                      Current_Ids.Vehicle_Ids.Last_Index < Current_Ids.Vehicle_Ids.Max_Index)
               then
                  declare
                     Id : Aida.Json_Parsing_Tests_Model.Vehicle_Id_T;
                  begin
                     Max_Indices.Allocate_Vehicle_Id (Id);
                     Current_Ids.Vehicle_Ids.Append (Id);

                     Storage.Person (Current_Ids.Person_Ids.Last_Element).Vehicles.Append (Id);
                  end;

                  State := Expecting_Wheels_Keyword;
               else
                  Call_Result.Initialize (1054910554, -2083890212);
               end if;
            when Expecting_Vehicles_Keyword |
                 Expecting_Array_Start |
                 Expecting_Wheels_Keyword |
                 Expecting_Wheels_Integer |
                 Expecting_Array_Object_End |
                 Expecting_Object_End |
                 End_State =>
               Call_Result.Initialize (-0167559177, 1644512204);
         end case;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Expecting_Array_Object_End then
            State := Expecting_Array_Object_Start_Or_Array_End;
         elsif State = Expecting_Object_End then
            State := End_State;
         else
            Call_Result.Initialize (1155657656, -0687632106);
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if
           State = Expecting_Vehicles_Keyword and then
           Name = "vehicles"
         then
            State := Expecting_Array_Start;
         elsif
           State = Expecting_Wheels_Keyword and then
           Name = "wheels"
         then
            State := Expecting_Wheels_Integer;
         else
            Call_Result.Initialize (1349641251, -1844644941);
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-2016661801, 0990153871);
      end Value_String;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);

         V : Aida.Int32_T;
         Has_Failed : Boolean;
      begin
         To_Int32 (Value, V, Has_Failed);

         if Has_Failed then
            Call_Result.Initialize (1492457862, 0015598968);
         else
            pragma Warnings (Off, "explicit membership test may be optimized away");
            if State = Expecting_Wheels_Integer then
               if
                 Current_Ids.Vehicle_Ids.Is_Empty or
                 V not in Aida.Int32_T (Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T'Last)
               then
                  Call_Result.Initialize (1062490083, -1931193623);
               else
                  declare
                     Id : Aida.Json_Parsing_Tests_Model.Vehicle_Id_T renames Current_Ids.Vehicle_Ids.Last_Element;
                  begin
                     Storage.Vehicle (Id).Wheels := Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T (V);
                  end;

                  State := Expecting_Array_Object_End;
               end if;
            else
               Call_Result.Initialize (0880800237, -1013051047);
            end if;
            pragma Warnings (On, "explicit membership test may be optimized away");
         end if;
      end Value_Integer;

      procedure Real_Value (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0273718273, 1867419500);
      end Real_Value;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (0498143319, -0323152002);
      end Boolean_Value;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         Call_Result.Initialize (-1850181303, -1732790230);
      end Null_Value;

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
      begin
         case State is
            when Expecting_Array_Start                     =>
               State := Expecting_Array_Object_Start_Or_Array_End;
            when Expecting_Object_Start                    |
                 Expecting_Vehicles_Keyword                |
                 Expecting_Array_Object_Start_Or_Array_End |
                 Expecting_Wheels_Keyword                  |
                 Expecting_Wheels_Integer                  |
                 Expecting_Array_Object_End                |
                 Expecting_Object_End                      |
                 End_State                                 =>
               Call_Result.Initialize (-0897465149, -1790850210);
         end case;
      end Array_Start;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
      begin
         case State is
            when Expecting_Array_Object_Start_Or_Array_End =>
               State := Expecting_Object_End;
            when Expecting_Object_Start                    |
                 Expecting_Vehicles_Keyword                |
                 Expecting_Array_Start                     |
                 Expecting_Wheels_Keyword                  |
                 Expecting_Wheels_Integer                  |
                 Expecting_Array_Object_End                |
                 Expecting_Object_End                      |
                 End_State                                 =>
               Call_Result.Initialize (-0750888177, 1037484298);
         end case;
      end Array_End;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         State_T,
                                                         Current_Ids_T,
                                                         Root_Start_Tag,
                                                         Root_End_Tag,
                                                         Key_Name,
                                                         Value_String,
                                                         Value_Integer,
                                                         Real_Value,
                                                         Boolean_Value,
                                                         Null_Value,
                                                         Array_Start,
                                                         Array_End);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Object_Start;

         Current_Ids : Current_Ids_T;
      begin
         Aida.Json_Parsing_Tests_Model.Max_Indices.Clear;

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
         Ahven.Assert (State = End_State, "18f1a2a4-741c-4a0c-90d3-8854e8a70a6d");
         Ahven.Assert (Current_Ids.Person_Ids.Last_Index >= Current_Ids.Person_Ids.First_Index, "07c6cd3a-40ce-4b81-9681-9954e56c9670");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max = 1, "4bc9f5b1-5451-49cd-b7aa-3ebd79f0abd3");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Vehicle_Id_Max = 2, "d1c1c8fb-539c-456e-8c41-8c2254079abe");
         if
           (Json_Parsing_Tests_Model.Max_Indices.Vehicle_Id_Max > 0 and
                Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max > 0) and then (not Is_Empty (Storage.Person (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max).Vehicles))
         then
            Ahven.Assert (Storage.Vehicle (1).Wheels = 4, "84d4a3a0-bc8a-4918-8ff7-5fa9a2fc1e9f");
            Ahven.Assert (Storage.Vehicle (2).Wheels = 2, "984aa38f-7efa-46db-a93e-768c4e36ebe6");
         end if;
      end Run_Test;

   end Test_Person_With_Vehicles_Utils;

   procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Vehicles_Utils.Run_Test (JSON_Test_Person_With_Vehicles_0);
   end Test_Person_With_Vehicles_0;

   package Test_Person_With_Length_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Object_Start,
                       Expecting_Length_Keyword,
                       Expecting_Length_Float,
                       Expecting_Object_End,
                       End_State
                      );

      procedure Start_Object (Storage     : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Object (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Key (Result      : in out Storage_T;
                     Max_Indices : in out Max_Indices_T;
                     State       : in out State_T;
                     Current_Ids : in out Current_Ids_T;
                     Name        : Aida.String_T;
                     Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure String_Value (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Integer_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Real_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : in     Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Length_Utils;

   package body Test_Person_With_Length_Utils with SPARK_Mode is

      procedure Start_Object (Storage     : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
      begin
         case State is
            when Expecting_Object_Start                    =>
               if
                 Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Current_Ids.Person_Ids.Last_Index < Current_Ids.Person_Ids.Max_Index
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Max_Indices.Allocate_Person_Id (Person_Id);
                     Current_Ids.Person_Ids.Append (Person_Id);
                  end;

                  State := Expecting_Length_Keyword;
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

      procedure End_Object (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Expecting_Object_End then
            State := End_State;
         else
            Call_Result.Initialize (-1088171163, 0366088626);
         end if;
      end End_Object;

      procedure Key (Result      : in out Storage_T;
                     Max_Indices : in out Max_Indices_T;
                     State       : in out State_T;
                     Current_Ids : in out Current_Ids_T;
                     Name        : Aida.String_T;
                     Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if
           State = Expecting_Length_Keyword and then
           Name = "length"
         then
            State := Expecting_Length_Float;
         else
            Call_Result.Initialize (-1277436309, -1289320473);
         end if;
      end Key;

      procedure String_Value (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (0973362602, 1877007627);
      end String_Value;

      procedure Integer_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (0836080939, 0159783088);
      end Integer_Value;

      procedure Real_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
      begin
         case State is
            when Expecting_Length_Float   =>
               declare
                  V : Aida.Float_T;
                  Has_Failed : Boolean;
               begin
                  To_Float (Value, V, Has_Failed);

                  if Has_Failed then
                     Call_Result.Initialize (1713066840, 1748338706);
                  else
                     if
                       Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids)
                     then
                        Call_Result.Initialize (1885775356, 1741885967);
                     else
                        declare
                           Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                             Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                        begin
                           Storage.Person (Person_Id).Length := Json_Parsing_Tests_Model.Person_Def.Length_T (V);
                           State := Expecting_Object_End;
                        end;
                     end if;
                  end if;
               end;
            when Expecting_Object_Start |
                 Expecting_Length_Keyword |
                 Expecting_Object_End |
                 End_State =>
               Call_Result.Initialize (1841755090, -1014383624);
         end case;
      end Real_Value;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0755111556, 0751766857);
      end Boolean_Value;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         Call_Result.Initialize (1558346114, -0799263304);
      end Null_Value;

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize (1729178755, 1696906378);
      end Array_Start;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize (1329123355, -0441958475);
      end Array_End;

      function Expected_Float_Value_1_98 return Float with
        Global => null;

      function Expected_Float_Value_1_98 return Float with
        SPARK_Mode => Off is
      begin
         return (Float'Value ("1.98"));
      end Expected_Float_Value_1_98;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         State_T,
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

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Object_Start;

         Current_Ids : Current_Ids_T;
      begin
         Aida.Json_Parsing_Tests_Model.Max_Indices.Clear;

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
         Ahven.Assert (State = End_State, "192a5b94-e6da-4302-81fc-f98211cd92d7");
         Ahven.Assert (Current_Ids.Person_Ids.Last_Index >= Current_Ids.Person_Ids.First_Index, "72ac1a27-0a07-4e71-ae7f-a49252f51989");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max = 1, "bd1380ff-2a9e-486e-810b-5896bea26d07");
         if
           Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max > 0
         then
            Ahven.Assert (Float (Storage.Person (Aida.Json_Parsing_Tests_Model.Person_Id_T'First).Length) = Expected_Float_Value_1_98, "e7f16b3a-d3f1-4b6b-b258-20fc95ce5bf4");
         end if;
      end Run_Test;
   end Test_Person_With_Length_Utils;

   procedure Test_Person_With_Length_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Length_Utils.Run_Test (JSON_Test_Person_With_Length_0);
   end Test_Person_With_Length_0;

   package Test_Person_With_Is_Happy_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Object_Start,
                       Expecting_Is_Happy_Keyword,
                       Expecting_Is_Happy_Value,
                       Expecting_Object_End,
                       End_State
                      );

      procedure Start_Object (Storage     : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Object (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Key (Result      : in out Storage_T;
                     Max_Indices : in out Max_Indices_T;
                     State       : in out State_T;
                     Current_Ids : in out Current_Ids_T;
                     Name        : Aida.String_T;
                     Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure String_Value (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Integer_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Real_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : in     Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (JSON            : Aida.String_T;
                          Expected_Result : Boolean) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

      procedure Run_Null_Value_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Is_Happy_Utils;

   package body Test_Person_With_Is_Happy_Utils with SPARK_Mode is

      procedure Start_Object (Storage     : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
      begin
         case State is
            when Expecting_Object_Start                    =>
               if
                 Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Current_Ids.Person_Ids.Last_Index < Current_Ids.Person_Ids.Max_Index
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Max_Indices.Allocate_Person_Id (Person_Id);
                     Current_Ids.Person_Ids.Append (Person_Id);
                  end;

                  State := Expecting_Is_Happy_Keyword;
               else
                  Call_Result.Initialize (2007020070, 1498390695);
               end if;
            when Expecting_Is_Happy_Keyword |
                 Expecting_Is_Happy_Value |
                 Expecting_Object_End |
                 End_State =>
               Call_Result.Initialize (2062686987, -0562861754);
         end case;
      end Start_Object;

      procedure End_Object (Result      : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Expecting_Object_End then
            State := End_State;
         else
            Call_Result.Initialize (0788289751, 0833928065);
         end if;
      end End_Object;

      procedure Key (Result      : in out Storage_T;
                     Max_Indices : in out Max_Indices_T;
                     State       : in out State_T;
                     Current_Ids : in out Current_Ids_T;
                     Name        : Aida.String_T;
                     Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if
           State = Expecting_Is_Happy_Keyword and then
           Name = "isHappy"
         then
            State := Expecting_Is_Happy_Value;
         else
            Call_Result.Initialize (-1511520157, -2147353105);
         end if;
      end Key;

      procedure String_Value (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (1935371031, 1770710050);
      end String_Value;

      procedure Integer_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Aida.String_T;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-1511988986, 1664368596);
      end Integer_Value;

      procedure Real_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize (1819683811, 0465871034);
      end Real_Value;

      procedure Boolean_Value (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : in     Boolean;
                               Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
      begin
         case State is
            when Expecting_Is_Happy_Value =>
               if
                 Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids)
               then
                  Call_Result.Initialize (-0910536654, 0511090950);
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     Storage.Person (Person_Id).Is_Happy := (Exists => True,
                                                             Value  => Value);
                     State := Expecting_Object_End;
                  end;
               end if;
            when Expecting_Object_Start |
                 Expecting_Is_Happy_Keyword |
                 Expecting_Object_End |
                 End_State =>
               Call_Result.Initialize (1765895842, 0305140407);
         end case;
      end Boolean_Value;

      procedure Null_Value (Storage     : in out Storage_T;
                            Max_Indices : in out Max_Indices_T;
                            State       : in out State_T;
                            Current_Ids : in out Current_Ids_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Storage);
         pragma Unreferenced (Max_Indices);
      begin
         case State is
            when Expecting_Is_Happy_Value =>
               if
                 Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids)
               then
                  Call_Result.Initialize (-1651870526, -1457115695);
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     Storage.Person (Person_Id).Is_Happy := (Exists => False);
                     State := Expecting_Object_End;
                  end;
               end if;
            when Expecting_Object_Start |
                 Expecting_Is_Happy_Keyword |
                 Expecting_Object_End |
                 End_State =>
               Call_Result.Initialize (1491919780, -1202037962);
         end case;
      end Null_Value;

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize (0902066191, -0202242500);
      end Array_Start;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize (2104342945, -1018218360);
      end Array_End;

      procedure Run_Test (JSON            : Aida.String_T;
                          Expected_Result : Boolean) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         State_T,
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

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Object_Start;

         Current_Ids : Current_Ids_T;
      begin
         Aida.Json_Parsing_Tests_Model.Max_Indices.Clear;

         Storage.Person (1).Is_Happy := (Exists => True,
                                         Value  => not Expected_Result);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
         Ahven.Assert (State = End_State, "3204a87f-ba9d-4564-8f7b-c94397343761");
         Ahven.Assert (Current_Ids.Person_Ids.Last_Index >= Current_Ids.Person_Ids.First_Index, "6b7aebdd-cab8-49aa-b524-cfb906e3c596");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max = 1, "07df518d-3a9e-4225-8795-27443231c29c");
         if
           Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max > 0
         then
            Ahven.Assert (Storage.Person (1).Is_Happy.Exists and then Storage.Person (1).Is_Happy.Value = Expected_Result, "86a232a3-7e4e-46d4-a8ef-a106f6b313a1");
         end if;
      end Run_Test;

      procedure Run_Null_Value_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON_SAX_Parse (Storage_T,
                                                         Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                         State_T,
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

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Object_Start;

         Current_Ids : Current_Ids_T;
      begin
         Aida.Json_Parsing_Tests_Model.Max_Indices.Clear;

         Storage.Person (1).Is_Happy := (Exists => True,
                                         Value  => True);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
         Ahven.Assert (State = End_State, "b8146e47-6f99-4567-90ef-e2297131f667");
         Ahven.Assert (Current_Ids.Person_Ids.Is_Non_Empty, "7072eb74-b0e2-47bc-9122-cdf748fb6dd8");
         Ahven.Assert (Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max = 1, "54753e3d-5985-4c67-8473-763e538e99a4");
         if
           Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max > 0
         then
            Ahven.Assert (Storage.Person (1).Is_Happy.Exists = False, "f4f28206-8e05-4832-900c-73503ccb362a");
         end if;
      end Run_Null_Value_Test;

   end Test_Person_With_Is_Happy_Utils;

   procedure Test_Person_With_Is_Happy_True_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Is_Happy_Utils.Run_Test (JSON_Test_Person_With_Is_Happy_True_0, True);
   end Test_Person_With_Is_Happy_True_0;

   procedure Test_Person_With_Is_Happy_False_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Is_Happy_Utils.Run_Test (JSON_Test_Person_With_Is_Happy_False_0, False);
   end Test_Person_With_Is_Happy_False_0;

   procedure Test_Person_With_Is_Happy_Null_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Is_Happy_Utils.Run_Null_Value_Test (JSON_Test_Person_With_Is_Happy_Null_0);
   end Test_Person_With_Is_Happy_Null_0;

end Aida.JSON_Parsing_Tests;
