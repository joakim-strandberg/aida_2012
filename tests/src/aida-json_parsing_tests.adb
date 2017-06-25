with Aida.Types;
with Aida.JSON.Generic_Parse_JSON;
with Aida.JSON;
with Aida.Bounded_String;
with Aida.Containers.Bounded_Vector;

package body Aida.JSON_Parsing_Tests is

   use all type Aida.Types.String_T;
   use all type Aida.JSON.Procedure_Call_Result.T;
   use all type Aida.Bounded_String.T;
   use all type Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
   use all type Aida.JSON.Tag_Id_T;

   use type Aida.Types.Int32_T;
   use type Aida.Json_Parsing_Tests_Model.Extended_Person_Array_Index_T;
   use type Aida.Json_Parsing_Tests_Model.Person_Def.Age_T;

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON.Generic_Parse_JSON package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_0'Access, "Test_Person_With_Name_Adam_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_1'Access, "Test_Person_With_Name_Adam_1");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_0'Access, "Test_Person_With_Hand_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Vehicles_0'Access, "Test_Person_With_Vehicles_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_And_Age_0'Access, "Test_Person_With_Name_Adam_And_Age_0");
   end Initialize;

   function Default_Person_Id return Json_Parsing_Tests_Model.Person_Array_Index_T is (1);

   package Person_Id_Vector is new Aida.Containers.Bounded_Vector (Index_T         => Json_Parsing_Tests_Model.Person_Array_Index_T,
                                                                   Element_T       => Json_Parsing_Tests_Model.Person_Array_Index_T,
                                                                   "="             => Json_Parsing_Tests_Model."=",
                                                                   Default_Element => Default_Person_Id);

   use all type Person_Id_Vector.T;

   type Current_Ids_T is limited record
      Person_Ids : Person_Id_Vector.T;
   end record;

   type Unused_State_T is (
                           Default_State,
                           End_Of_Json_Object_Reached
                          );

   procedure Unused_Value_String (Result      : in out Storage_T;
                                  Max_Indices : in out Max_Indices_T;
                                  State       : in out Unused_State_T;
                                  Current_Ids : in out Current_Ids_T;
                                  Value       : Aida.Types.String_T;
                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Value_String (Result      : in out Storage_T;
                                  Max_Indices : in out Max_Indices_T;
                                  State       : in out Unused_State_T;
                                  Current_Ids : in out Current_Ids_T;
                                  Value       : Aida.Types.String_T;
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
                                   Value       : Aida.Types.Int32_T;
                                   Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Value_Integer (Result      : in out Storage_T;
                                   Max_Indices : in out Max_Indices_T;
                                   State       : in out Unused_State_T;
                                   Current_Ids : in out Current_Ids_T;
                                   Value       : Aida.Types.Int32_T;
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
                          Name        : Aida.Types.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_String (Result      : in out Storage_T;
                              Max_Indices : in out Max_Indices_T;
                              State       : in out Unused_State_T;
                              Current_Ids : in out Current_Ids_T;
                              Value       : Aida.Types.String_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Run_Test (JSON : Aida.Types.String_T) with
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
                          Name        : Aida.Types.String_T;
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
                              Value       : Aida.Types.String_T;
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

      procedure Run_Test (JSON : Aida.Types.String_T) is

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

      JSON : constant Aida.Types.String_T := "{""name"" : ""adam""}";
   begin
      Test_Person_With_Name_Adam_Utils.Run_Test (JSON);
   end Test_Person_With_Name_Adam_0;

   procedure Test_Person_With_Name_Adam_1 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      JSON : constant Aida.Types.String_T := "   {""name"" : ""adam""}";

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
                          Name        : Aida.Types.String_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);

      pragma Warnings (Off, """Current_Ids"" is not modified, could be IN");
      procedure Value_Integer (Storage     : in out Storage_T;
                               Max_Indices : in out Max_Indices_T;
                               State       : in out Unused_State_T;
                               Current_Ids : in out Current_Ids_T;
                               Value       : Aida.Types.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null,
        Pre    => not Has_Failed (Call_Result);
      pragma Warnings (On, """Current_Ids"" is not modified, could be IN");

      procedure Run_Test (JSON : Aida.Types.String_T) with
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
                          Name        : Aida.Types.String_T;
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
                               Value       : Aida.Types.Int32_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (State);
      begin
         if
           Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) or
           Value not in Aida.Types.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'First)..Aida.Types.Int32_T (Json_Parsing_Tests_Model.Person_Def.Age_T'Last)
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

      procedure Run_Test (JSON : Aida.Types.String_T) is

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

      JSON : constant Aida.Types.String_T := "{""age"" : 10}";
   begin
      Test_Person_With_Age_Utils.Run_Test (JSON);
   end Test_Person_With_Age_0;
--
--     procedure Test_Person_With_Name_Adam_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       SPARK_Mode => On
--     is
--        pragma Unreferenced (T);
--
--        Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
--
--        procedure Root_Start_Tag (Result      : in out Storage_T;
--                                  Tag_Id      : Aida.JSON.Tag_Id_T;
--                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Root_Start_Tag (Result      : in out Storage_T;
--                                  Tag_Id      : Aida.JSON.Tag_Id_T;
--                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
--                               Person_Id => Person_Id);
--        end Root_Start_Tag;
--
--        procedure Root_End_Tag (Result      : in out Storage_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Root_End_Tag (Result      : in out Storage_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           null;
--        end Root_End_Tag;
--
--        procedure Key_Name (Result      : in out Storage_T;
--                            Name        : Aida.Types.String_T;
--                            Tag_Id      : Aida.JSON.Tag_Id_T;
--                            Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Key_Name (Result      : in out Storage_T;
--                            Name        : Aida.Types.String_T;
--                            Tag_Id      : Aida.JSON.Tag_Id_T;
--                            Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--        begin
--           if Name = "name" or Name = "age" then
--              null;
--           else
--              Initialize (Call_Result, "3503ee95-b4ff-46d5-bbd2-c7b695f63b0c");
--           end if;
--        end Key_Name;
--
--        procedure Value_String (Result      : in out Storage_T;
--                                Value       : Aida.Types.String_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Value_String (Result      : in out Storage_T;
--                                Value        : Aida.Types.String_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--        begin
--           if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
--              Initialize (Call_Result, "e229c14e-950b-4019-be21-37cf5da0a750");
--           else
--              Initialize (Storage.Person (Person_Id).Name,
--                          Value);
--           end if;
--        end Value_String;
--
--        procedure Value_Integer (Result      : in out Storage_T;
--                                 Value       : Aida.Types.Int32_T;
--                                 Tag_Id      : Aida.JSON.Tag_Id_T;
--                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Value_Integer (Result      : in out Storage_T;
--                                 Value       : Aida.Types.Int32_T;
--                                 Tag_Id      : Aida.JSON.Tag_Id_T;
--                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           Put_Line (To_String (Value));
--           Storage.Person (Person_Id).Age := Json_Parsing_Tests_Model.Person_Def.Age_T (Value);
--        end Value_Integer;
--
--        procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
--                                                                 Root_Start_Tag,
--                                                                 Root_End_Tag,
--                                                                 Key_Name,
--                                                                 Value_String,
--                                                                 Value_Integer,
--                                                                 Unused_Array_Start,
--                                                                 Unused_Array_End);
--
--        Call_Result : Aida.JSON.Procedure_Call_Result.T;
--
--        XML : constant Aida.Types.String_T := "{""name"" : ""bertil"", ""age"" : 5}";
--
--     begin
--        Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
--
--        Parse_XML (Storage, XML, Call_Result);
--
--        Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
--        Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "41436cda-0c06-4b34-8963-32a6ea0553b6");
--        Ahven.Assert (To_String (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) = "bertil", "0c7c52a3-774b-4133-9937-733c539d474f");
--        Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 5, "b72edaec-bdb3-4b58-9964-2590c51617b3");
--     end Test_Person_With_Name_Adam_And_Age_0;
--
--     procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       SPARK_Mode => On
--     is
--        pragma Unreferenced (T);
--
--        Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
--
--        procedure Root_Start_Tag (Result      : in out Storage_T;
--                                  Tag_Id      : Aida.JSON.Tag_Id_T;
--                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Root_Start_Tag (Result      : in out Storage_T;
--                                  Tag_Id      : Aida.JSON.Tag_Id_T;
--                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
--                               Person_Id => Person_Id);
--        end Root_Start_Tag;
--
--        procedure Root_End_Tag (Result      : in out Storage_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Root_End_Tag (Result      : in out Storage_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           null;
--        end Root_End_Tag;
--
--        procedure Key_Name (Result      : in out Storage_T;
--                            Name        : Aida.Types.String_T;
--                            Tag_Id      : Aida.JSON.Tag_Id_T;
--                            Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Key_Name (Result      : in out Storage_T;
--                            Name        : Aida.Types.String_T;
--                            Tag_Id      : Aida.JSON.Tag_Id_T;
--                            Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--        begin
--           if Tag_Id = Aida.JSON.Tag_Id_T'First and then Name = "hand" then
--              null;
--           elsif Tag_Id = Aida.JSON.Tag_Id_T'First + 1 and then Name = "fingers" then
--              null;
--           else
--              Initialize (Call_Result, "8792ce90-b45f-4c6d-a1b4-7b4a3fa22df7");
--           end if;
--        end Key_Name;
--
--        procedure Value_Integer (Result      : in out Storage_T;
--                                        Value       : Aida.Types.Int32_T;
--                                        Tag_Id      : Aida.JSON.Tag_Id_T;
--                                        Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Value_Integer (Result      : in out Storage_T;
--                                 Value       : Aida.Types.Int32_T;
--                                 Tag_Id      : Aida.JSON.Tag_Id_T;
--                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--        begin
--           if Value /= 5 then
--              Initialize (Call_Result, "aa44346a-f25a-4c7e-907e-3d62b85ad970");
--           end if;
--        end Value_Integer;
--
--        procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
--                                                                 Root_Start_Tag,
--                                                                 Root_End_Tag,
--                                                                 Key_Name,
--                                                                 Unused_Value_String,
--                                                                 Value_Integer,
--                                                                 Unused_Array_Start,
--                                                                 Unused_Array_End);
--
--        Call_Result : Aida.JSON.Procedure_Call_Result.T;
--
--        XML : constant Aida.Types.String_T := "{""hand"" : { ""fingers"" : 5 }}";
--
--     begin
--        Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
--
--        Parse_XML (Storage, XML, Call_Result);
--
--        Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
--        Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 2, "ae7399ea-3d2a-4400-a10f-34104d439978");
--     end Test_Person_With_Hand_0;
--
--     procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       SPARK_Mode => On
--     is
--        pragma Unreferenced (T);
--
--        Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
--
--        procedure Root_Start_Tag (Result      : in out Storage_T;
--                                  Tag_Id      : Aida.JSON.Tag_Id_T;
--                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Root_Start_Tag (Result      : in out Storage_T;
--                                  Tag_Id      : Aida.JSON.Tag_Id_T;
--                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
--                               Person_Id => Person_Id);
--        end Root_Start_Tag;
--
--        procedure Root_End_Tag (Result      : in out Storage_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Root_End_Tag (Result      : in out Storage_T;
--                                Tag_Id      : Aida.JSON.Tag_Id_T;
--                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           null;
--        end Root_End_Tag;
--
--        procedure Key_Name (Result      : in out Storage_T;
--                            Name        : Aida.Types.String_T;
--                            Tag_Id      : Aida.JSON.Tag_Id_T;
--                            Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Key_Name (Result      : in out Storage_T;
--                            Name        : Aida.Types.String_T;
--                            Tag_Id      : Aida.JSON.Tag_Id_T;
--                            Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--        begin
--           if Tag_Id = Aida.JSON.Tag_Id_T'First and then Name = "vehicles" then
--              null;
--           elsif (Tag_Id = Aida.JSON.Tag_Id_T'First + 1 or Tag_Id = Aida.JSON.Tag_Id_T'First + 2) and then
--             Name = "wheels"
--           then
--              null;
--           else
--              Initialize (Call_Result, "aea59ef9-feb3-4f2a-a680-477c468da561");
--           end if;
--        end Key_Name;
--
--        procedure Value_Integer (Result      : in out Storage_T;
--                                        Value       : Aida.Types.Int32_T;
--                                        Tag_Id      : Aida.JSON.Tag_Id_T;
--                                        Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Value_Integer (Result      : in out Storage_T;
--                                 Value       : Aida.Types.Int32_T;
--                                 Tag_Id      : Aida.JSON.Tag_Id_T;
--                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--        begin
--           if Value = 4 or Value = 2 then
--              null;
--           else
--              Initialize (Call_Result, "b14aafea-3be6-4222-b5c0-05506ed37ec7");
--           end if;
--        end Value_Integer;
--
--        procedure Array_Start (Result      : in out Storage_T;
--                               Tag_Id      : Aida.JSON.Tag_Id_T;
--                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Array_Start (Result      : in out Storage_T;
--                               Tag_Id      : Aida.JSON.Tag_Id_T;
--                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           null;
--        end Array_Start;
--
--        procedure Array_End (Result      : in out Storage_T;
--                             Tag_Id      : Aida.JSON.Tag_Id_T;
--                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
--          Global => null;
--
--        procedure Array_End (Result      : in out Storage_T;
--                             Tag_Id      : Aida.JSON.Tag_Id_T;
--                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
--        is
--           pragma Unreferenced (Result);
--           pragma Unreferenced (Tag_Id);
--           pragma Unreferenced (Call_Result);
--        begin
--           null;
--        end Array_End;
--
--        procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
--                                                                 Root_Start_Tag,
--                                                                 Root_End_Tag,
--                                                                 Key_Name,
--                                                                 Unused_Value_String,
--                                                                 Value_Integer,
--                                                                 Array_Start,
--                                                                 Array_End);
--
--        Call_Result : Aida.JSON.Procedure_Call_Result.T;
--
--        XML : constant Aida.Types.String_T := "{""vehicles"" : [ {""wheels"" : 4 }, {""wheels"" : 2 } ]}";
--
--     begin
--        Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
--
--        Parse_XML (Storage, XML, Call_Result);
--
--        Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
--        Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 3, "41abd5c7-8264-4d51-a41f-a704a56ae11e");
--     end Test_Person_With_Vehicles_0;

end Aida.JSON_Parsing_Tests;
