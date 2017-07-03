with Aida.JSON.Generic_Parse_JSON;
with Aida.JSON;
with Aida.Bounded_String;

package body Aida.JSON_Parsing_Tests is

   use all type Aida.String_T;
   use all type Aida.JSON.Procedure_Call_Result.T;
   use all type Aida.Bounded_String.T;
   use all type Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
   use all type Aida.Json_Parsing_Tests_Model.Person_T;
   use all type Aida.Json_Parsing_Tests_Model.Hand_T;
   use all type Aida.Json_Parsing_Tests_Model.Vehicle_T;
   use all type Aida.JSON.Tag_Id_T;

   use type Aida.Int32_T;
   use type Json_Parsing_Tests_Model.Extended_Person_Array_Index_T;
   use type Json_Parsing_Tests_Model.Extended_Hand_Array_Index_T;
   use type Json_Parsing_Tests_Model.Extended_Vehicle_Array_Index_T;
   use type Json_Parsing_Tests_Model.Person_Def.Age_T;
   use type Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T;
   use type Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T;
   use type Json_Parsing_Tests_Model.Person_Def.Hand_Vector_Index_T;
   use type Json_Parsing_Tests_Model.Person_Def.Vehicle_Vector_Index_T;

   JSON_Test_Person_With_Name_And_Age : constant Aida.String_T := "{""name"" : ""bertil"", ""age"" : 5}";
   JSON_Test_Person_With_Hand         : constant Aida.String_T := "{""hand"" : { ""fingers"" : 4 }}";
   JSON_Test_Person_With_Vehicles_0   : constant Aida.String_T := "{""vehicles"" : [ {""wheels"" : 4 }, {""wheels"" : 2 } ]}";

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON.Generic_Parse_JSON package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_0'Access, "Test_Person_With_Name_Adam_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_1'Access, "Test_Person_With_Name_Adam_1");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_0'Access, "Test_Person_With_Hand_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Vehicles_0'Access, "Test_Person_With_Vehicles_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_And_Age_0'Access, "Test_Person_With_Name_And_Age_0");
   end Initialize;

   use all type Person_Id_Vector.T;
   use all type Hand_Id_Vector.T;
   use all type Vehicle_Id_Vector.T;

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
                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Value_String (Result      : in out Storage_T;
                                  Max_Indices : in out Max_Indices_T;
                                  State       : in out Unused_State_T;
                                  Current_Ids : in out Current_Ids_T;
                                  Value       : Aida.String_T;
                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
   begin
      Initialize (Call_Result, "69171640-accb-4b2c-b8d6-07d36b2e33b2");
   end Unused_Value_String;

   procedure Unused_Value_Integer (Result      : in out Storage_T;
                                   Max_Indices : in out Max_Indices_T;
                                   State       : in out Unused_State_T;
                                   Current_Ids : in out Current_Ids_T;
                                   Value       : Aida.Int32_T;
                                   Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Value_Integer (Result      : in out Storage_T;
                                   Max_Indices : in out Max_Indices_T;
                                   State       : in out Unused_State_T;
                                   Current_Ids : in out Current_Ids_T;
                                   Value       : Aida.Int32_T;
                                   Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Value);
   begin
      Initialize (Call_Result, "baf7fe57-da60-4245-af9d-2f4a81b007e5");
   end Unused_Value_Integer;

   procedure Unused_Array_Start (Result      : in out Storage_T;
                                 Max_Indices : in out Max_Indices_T;
                                 State       : in out Unused_State_T;
                                 Current_Ids : in out Current_Ids_T;
                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Array_Start (Result      : in out Storage_T;
                                 Max_Indices : in out Max_Indices_T;
                                 State       : in out Unused_State_T;
                                 Current_Ids : in out Current_Ids_T;
                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
   begin
      Initialize (Call_Result, "0a4c81f4-f6e4-477e-a53d-fb1b1b9ed433");
   end Unused_Array_Start;

   procedure Unused_Array_End (Result      : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Array_End (Result      : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (State);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Max_Indices);
   begin
      Initialize (Call_Result, "149f8a60-f48a-4274-baf6-902c0cc00c89");
   end Unused_Array_End;

   package Test_Person_With_Name_Adam_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Name_Adam_Utils;

   package body Test_Person_With_Name_Adam_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
      begin
         if
           Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Array_Index_T'Last and
           Person_Id_Vector.Length (Current_Ids.Person_Ids) < Person_Id_Vector.Length_T'Last
         then
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
            begin
               Allocate_Person_Id (This      => Max_Indices,
                                   Person_Id => Person_Id);
               Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
            end;
         else
            Initialize (Call_Result, "160a399d-2a5a-45f5-aa43-1ca45883ad13");
         end if;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Default_State then
            State := End_Of_Json_Object_Reached;
         else
            Initialize (Call_Result, "1a5f31d4-9be0-44c1-a541-d481c48735a2");
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         if Name /= "name" then
            Initialize (Call_Result, "6f878261-0825-45af-bab1-caf33d6885b6");
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
            Initialize (Call_Result, "add1239e-de10-4258-9bff-5cddf3bbc72f");
         else
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                 Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
            begin
               if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                  Initialize (Call_Result, "6b7c4309-53c8-4dfd-b2d6-23ff6ad0e185");
               else
                  Initialize (Result.Person (Person_Id).Name,
                              Value);
               end if;
            end;
         end if;
      end Value_String;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                                  Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                  Unused_State_T,
                                                                  Current_Ids_T,
                                                                  Test_Person_With_Name_Adam_Utils.Root_Start_Tag,
                                                                  Test_Person_With_Name_Adam_Utils.Root_End_Tag,
                                                                  Test_Person_With_Name_Adam_Utils.Key_Name,
                                                                  Test_Person_With_Name_Adam_Utils.Value_String,
                                                                  Unused_Value_Integer,
                                                                  Unused_Array_Start,
                                                                  Unused_Array_End);

         Call_Result : Aida.JSON.Procedure_Call_Result.T;

         State : Unused_State_T := Default_State;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (State = End_Of_Json_Object_Reached, "dd1327b2-c0d2-4414-87f5-8a35d0a94d6f");
         Ahven.Assert (Person_Id_Vector.Length (Current_Ids.Person_Ids) > 0, "150be077-0e4c-43b2-901e-1ffc9f57bb76");
         Ahven.Assert (not Has_Failed (Call_Result), "5a84dd71-1bee-4e2c-b7f8-13915f953605");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "87f1346a-607c-4e7a-8a3a-621365c323d9");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0, "87f1346a-607c-4e7a-8a3a-621365c323d9");
         if
           Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and then
           Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) <= Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name.Maximum_Length
         then
            Ahven.Assert (To_String (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) = "adam", "11bdb82b-275f-4432-87f0-33d27925d7b6");
         end if;
      end Run_Test;

   end Test_Person_With_Name_Adam_Utils;

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      JSON : constant Aida.String_T := "{""name"" : ""adam""}";
   begin
      Test_Person_With_Name_Adam_Utils.Run_Test (JSON);
   end Test_Person_With_Name_Adam_0;

   procedure Test_Person_With_Name_Adam_1 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      JSON : constant Aida.String_T := "   {""name"" : ""adam""}";

   begin
      Test_Person_With_Name_Adam_Utils.Run_Test (JSON);
   end Test_Person_With_Name_Adam_1;

   package Test_Person_With_Age_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);
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
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
      begin
         if
           Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Array_Index_T'Last and
           Person_Id_Vector.Length (Current_Ids.Person_Ids) < Person_Id_Vector.Length_T'Last
         then
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
            begin
               Allocate_Person_Id (This      => Max_Indices,
                                   Person_Id => Person_Id);
               Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
            end;
         else
            Initialize (Call_Result, "7c0d3af3-a510-42c4-a359-1a41a0e0a953");
         end if;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Default_State then
            State := End_Of_Json_Object_Reached;
         else
            Initialize (Call_Result, "a84e7a37-0fb7-4b87-9d95-66df9538b3d4");
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         if Name /= "age" then
            Initialize (Call_Result, "b9b55fb5-47ff-4ec1-bd25-2f8adf8a31b2");
         end if;
      end Key_Name;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         if
           Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) or
           Value not in Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'Last)
         then
            Initialize (Call_Result, "b3295c97-3add-4a17-9830-92d830a86b4c");
         else
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                 Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
            begin
               Storage.Person (Person_Id).Age := Json_Parsing_Tests_Model.Person_Def.Age_T (Value);
            end;
         end if;
      end Value_Integer;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                                  Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                  Unused_State_T,
                                                                  Current_Ids_T,
                                                                  Root_Start_Tag,
                                                                  Root_End_Tag,
                                                                  Key_Name,
                                                                  Unused_Value_String,
                                                                  Value_Integer,
                                                                  Unused_Array_Start,
                                                                  Unused_Array_End);

         Call_Result : Aida.JSON.Procedure_Call_Result.T;

         State : Unused_State_T := Default_State;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
         Ahven.Assert (State = End_Of_Json_Object_Reached, "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Person_Id_Vector.Length (Current_Ids.Person_Ids) > 0, "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Has_Failed (Call_Result), "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         if
           Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and then
           Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) <= Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name.Maximum_Length
         then
            Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
         end if;
      end Run_Test;

   end Test_Person_With_Age_Utils;

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      JSON : constant Aida.String_T := "{""age"" : 10}";
   begin
      Test_Person_With_Age_Utils.Run_Test (JSON);
   end Test_Person_With_Age_0;

   package Test_Person_With_Name_And_Age_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out Unused_State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);
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
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
      begin
         if
           Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Array_Index_T'Last and
           Person_Id_Vector.Length (Current_Ids.Person_Ids) < Person_Id_Vector.Length_T'Last
         then
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
            begin
               Allocate_Person_Id (This      => Max_Indices,
                                   Person_Id => Person_Id);
               Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
            end;
         else
            Initialize (Call_Result, "40f38692-5dec-41df-adb0-4cc934b069c3");
         end if;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         if State = Default_State then
            State := End_Of_Json_Object_Reached;
         else
            Initialize (Call_Result, "06a82d09-5b78-4eaa-a918-f25ebca07717");
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out Unused_State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
      begin
         if Name = "name" or Name = "age" then
            null;
         else
            Initialize (Call_Result, "7c1646cf-a70e-48ab-b9de-7c2955be572c");
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
            Initialize (Call_Result, "be6b9ea7-fdf9-4ace-8c1b-927162954f48");
         else
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                 Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
            begin
               if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                  Initialize (Call_Result, "f41d64b0-2450-4edf-a81d-82f6963e83fb");
               else
                  Initialize (Result.Person (Person_Id).Name,
                              Value);
               end if;
            end;
         end if;
      end Value_String;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         if
           Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) or
           Value not in Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'Last)
         then
            Initialize (Call_Result, "7423b440-d9a9-49f7-a128-7a9e9e515d33");
         else
            declare
               Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                 Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
            begin
               Storage.Person (Person_Id).Age := Json_Parsing_Tests_Model.Person_Def.Age_T (Value);
            end;
         end if;
      end Value_Integer;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                                  Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                  Unused_State_T,
                                                                  Current_Ids_T,
                                                                  Root_Start_Tag,
                                                                  Root_End_Tag,
                                                                  Key_Name,
                                                                  Value_String,
                                                                  Value_Integer,
                                                                  Unused_Array_Start,
                                                                  Unused_Array_End);

         Call_Result : Aida.JSON.Procedure_Call_Result.T;

         State : Unused_State_T := Default_State;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
         Ahven.Assert (State = End_Of_Json_Object_Reached, "4b9650d2-c30f-401a-a060-a0e039fe413c");
         Ahven.Assert (Person_Id_Vector.Length (Current_Ids.Person_Ids) > 0, "bb095008-4756-4392-ac77-03799c82a947");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "e095f887-42d1-4fbf-846c-75e32af16af6");
         Ahven.Assert (not Has_Failed (Call_Result), "decc36b9-2538-4cfb-8fa2-4fd7b240abd8");
         if
           Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and then
           Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) <= Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name.Maximum_Length
         then
            Ahven.Assert (To_String (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) = "bertil", "bc2f314b-46c2-4310-849e-93d3380f0cf3");
            Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 5, "0b99115c-279b-4b51-991f-4fafcb0fd21f");
         end if;
      end Run_Test;

   end Test_Person_With_Name_And_Age_Utils;

   procedure Test_Person_With_Name_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Name_And_Age_Utils.Run_Test (JSON_Test_Person_With_Name_And_Age);
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
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Hand_Utils;

   package body Test_Person_With_Hand_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Storage     : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
      begin
         case State is
            when Expecting_Object_Start      =>
               if
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Array_Index_T'Last and
                 Person_Id_Vector.Length (Current_Ids.Person_Ids) < Person_Id_Vector.Length_T'Last
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
                  end;

                  State := Expecting_Hand_Keyword;
               else
                  Initialize (Call_Result, "9e4ea1d3-6755-42a9-82df-1405f659476d");
               end if;
            when Expecting_Hand_Object_Start =>
               if
                 Person_Id_Vector.Length (Current_Ids.Person_Ids) > 0 and then
                 (
                  Json_Parsing_Tests_Model.Person_Def.Hand_Vector.Length (Storage.Person (Person_Id_Vector.Last_Element (Current_Ids.Person_Ids)).Hands) <
                  Json_Parsing_Tests_Model.Person_Def.Hand_Vector_Index_T'Last and
                  Hand_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Hand_Array_Index_T'Last and
                  Hand_Id_Vector.Length (Current_Ids.Hand_Ids) < Hand_Id_Vector.Length_T'Last)
               then
                  declare
                     Hand_Id : Aida.Json_Parsing_Tests_Model.Hand_Id_T;
                  begin
                     Allocate_Hand_Id (This    => Max_Indices,
                                       Hand_Id => Hand_Id);
                     Hand_Id_Vector.Append (Current_Ids.Hand_Ids, Hand_Id);

                     Json_Parsing_Tests_Model.Person_Def.Hand_Vector.Append (Storage.Person (Person_Id_Vector.Last_Element (Current_Ids.Person_Ids)).Hands, Hand_Id);
                  end;

                  State := Expecting_Fingers_Keyword;
               else
                  Initialize (Call_Result, "b07541b2-9379-4297-a22d-061c0adf52af");
               end if;
            when Expecting_Hand_Keyword |
                 Expecting_Fingers_Keyword |
                 Expecting_Fingers_Value |
                 Expecting_Hand_Object_End |
                 Expecting_Object_End |
                 End_State =>
               Initialize (Call_Result, "93b136f5-3202-49a4-a85e-f0979a76bfbb");
         end case;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
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
            Initialize (Call_Result, "69086801-17b1-4f4c-b0e1-ab3ca476cbab");
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
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
            Initialize (Call_Result, "e00e930f-512c-4c65-abb5-a0a47b95a359");
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Initialize (Call_Result, "02ea04b7-5851-4aed-a835-a616a62e6a77");
      end Value_String;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
      begin
         if State = Expecting_Fingers_Value then
            if
              Hand_Id_Vector.Is_Empty (Current_Ids.Hand_Ids) or
              Value not in Aida.Int32_T (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'Last)
            then
               Initialize (Call_Result, "1e9cd236-e4df-4eba-af3c-23306e7f1f79");
            else
               declare
                  Id : Aida.Json_Parsing_Tests_Model.Hand_Id_T renames
                    Hand_Id_Vector.Last_Element (Current_Ids.Hand_Ids);
               begin
                  Storage.Hand (Id).Number_Of_Fingers := Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T (Value);
               end;

               State := Expecting_Hand_Object_End;
            end if;
         else
            Initialize (Call_Result, "046a07c4-b375-428c-9615-da783c9f06e1");
         end if;
      end Value_Integer;

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
      begin
         Initialize (Call_Result, "d5c85509-921d-4db8-899a-0baf4621517d");
      end Array_Start;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Max_Indices);
      begin
         Initialize (Call_Result, "6ec8c319-40ce-4ddd-b98e-f116e49c387f");
      end Array_End;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                                  Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                  State_T,
                                                                  Current_Ids_T,
                                                                  Root_Start_Tag,
                                                                  Root_End_Tag,
                                                                  Key_Name,
                                                                  Value_String,
                                                                  Value_Integer,
                                                                  Array_Start,
                                                                  Array_End);

         Call_Result : Aida.JSON.Procedure_Call_Result.T;

         State : State_T := Expecting_Object_Start;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
         Ahven.Assert (State = End_State, "397d359d-2d92-462b-8b32-2a4bbdc6ce25");
         Ahven.Assert (Person_Id_Vector.Length (Current_Ids.Person_Ids) > 0, "810561fa-2c9f-4582-a5cf-10e5abd85113");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "ae7399ea-3d2a-4400-a10f-34104d439978");
         Ahven.Assert (Hand_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "0b77dd49-3cbd-44cd-ab53-9b65d0d75c05");
         if
           (Hand_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and
                Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0) and then (Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Hands) > 0)
         then
            declare
               Hand_Id : constant Json_Parsing_Tests_Model.Hand_Id_T :=
                 Last_Element (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Hands);
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
      Test_Person_With_Hand_Utils.Run_Test (JSON_Test_Person_With_Hand);
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
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      procedure Run_Test (JSON : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => JSON'Last < Integer'Last - 4;

   end Test_Person_With_Vehicles_Utils;

   package body Test_Person_With_Vehicles_Utils with SPARK_Mode is

      procedure Root_Start_Tag (Storage     : in out Storage_T;
                                Max_Indices : in out Max_Indices_T;
                                State       : in out State_T;
                                Current_Ids : in out Current_Ids_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
      begin
         case State is
            when Expecting_Object_Start                    =>
               if
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Array_Index_T'Last and
                 Length (Current_Ids.Person_Ids) < Person_Id_Vector.Length_T'Last
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Append (Current_Ids.Person_Ids, Person_Id);
                  end;

                  State := Expecting_Vehicles_Keyword;
               else
                  Initialize (Call_Result, "3b3dd45e-c7ff-4725-9184-ec5c4524411e");
               end if;
            when Expecting_Array_Object_Start_Or_Array_End =>
               if
                 Length (Current_Ids.Person_Ids) > 0 and then
                 (
                  Length (Storage.Person (Last_Element (Current_Ids.Person_Ids)).Vehicles) <
                  Json_Parsing_Tests_Model.Person_Def.Vehicle_Vector_Index_T'Last and
                  Vehicle_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Vehicle_Array_Index_T'Last and
                  Length (Current_Ids.Vehicle_Ids) < Vehicle_Id_Vector.Length_T'Last)
               then
                  declare
                     Id : Aida.Json_Parsing_Tests_Model.Vehicle_Id_T;
                  begin
                     Allocate_Vehicle_Id (Max_Indices, Id);
                     Append (Current_Ids.Vehicle_Ids, Id);

                     Append (Storage.Person (Last_Element (Current_Ids.Person_Ids)).Vehicles, Id);
                  end;

                  State := Expecting_Wheels_Keyword;
               else
                  Initialize (Call_Result, "27870adc-c5b7-4fa2-88f1-815c57a7e944");
               end if;
            when Expecting_Vehicles_Keyword |
                 Expecting_Array_Start |
                 Expecting_Wheels_Keyword |
                 Expecting_Wheels_Integer |
                 Expecting_Array_Object_End |
                 Expecting_Object_End |
                 End_State =>
               Initialize (Call_Result, "a1ea5299-7708-4683-849a-874266a6b4b4");
         end case;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
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
            Initialize (Call_Result, "8c212d91-7845-465e-b2b4-605e4bc0b91c");
         end if;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Max_Indices : in out Max_Indices_T;
                          State       : in out State_T;
                          Current_Ids : in out Current_Ids_T;
                          Name        : Aida.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
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
            Initialize (Call_Result, "2938110e-dfde-49cf-9670-70b2d2575ccd");
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
      begin
         Initialize (Call_Result, "4c5999f9-b72c-46d3-b579-dadee0e57c28");
      end Value_String;

      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
      begin
         pragma Warnings (Off, "explicit membership test may be optimized away");
         if State = Expecting_Wheels_Integer then
            if
              Is_Empty (Current_Ids.Vehicle_Ids) or
              Value not in Aida.Int32_T (Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T'First)..Aida.Int32_T (Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T'Last)
            then
               Initialize (Call_Result, "d19a12ce-682c-438c-9cd7-209ee262a6c6");
            else
               declare
                  Id : Aida.Json_Parsing_Tests_Model.Vehicle_Id_T renames Last_Element (Current_Ids.Vehicle_Ids);
               begin
                  Storage.Vehicle (Id).Wheels := Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T (Value);
               end;

               State := Expecting_Array_Object_End;
            end if;
         else
            Initialize (Call_Result, "9583972a-ffa8-4f84-8317-74176e985fdc");
         end if;
         pragma Warnings (On, "explicit membership test may be optimized away");
      end Value_Integer;

      procedure Array_Start (Result      : in out Storage_T;
                             Max_Indices : in out Max_Indices_T;
                             State       : in out State_T;
                             Current_Ids : in out Current_Ids_T;
                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
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
               Initialize (Call_Result, "c20f962f-9c73-4917-8758-e494d601b17d");
         end case;
      end Array_Start;

      procedure Array_End (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
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
               Initialize (Call_Result, "01170c8f-c409-4bc4-8b04-679adc6893ad");
         end case;
      end Array_End;

      procedure Run_Test (JSON : Aida.String_T) is

         procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                                  Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                  State_T,
                                                                  Current_Ids_T,
                                                                  Root_Start_Tag,
                                                                  Root_End_Tag,
                                                                  Key_Name,
                                                                  Value_String,
                                                                  Value_Integer,
                                                                  Array_Start,
                                                                  Array_End);

         Call_Result : Aida.JSON.Procedure_Call_Result.T;

         State : State_T := Expecting_Object_Start;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    JSON,
                    Call_Result);

         Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
         Ahven.Assert (State = End_State, "18f1a2a4-741c-4a0c-90d3-8854e8a70a6d");
         Ahven.Assert (Length (Current_Ids.Person_Ids) > 0, "07c6cd3a-40ce-4b81-9681-9954e56c9670");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "4bc9f5b1-5451-49cd-b7aa-3ebd79f0abd3");
         Ahven.Assert (Vehicle_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 2, "d1c1c8fb-539c-456e-8c41-8c2254079abe");
         if
           (Vehicle_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and
                Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0) and then (Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Vehicles) > 0)
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

end Aida.JSON_Parsing_Tests;
