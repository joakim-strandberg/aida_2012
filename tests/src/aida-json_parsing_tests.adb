with Aida.Types;
with Aida.JSON.Generic_Parse_JSON;
with Aida.Bounded_String;
with Aida.Json_Parsing_Tests_Model;

package body Aida.JSON_Parsing_Tests is

   use all type Aida.Types.String_T;
   use all type Aida.JSON.Procedure_Call_Result.T;
   use all type Aida.Bounded_String.T;
   use all type Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
   use all type Aida.Json_Parsing_Tests_Model.Person_Def.Name_T;
   use all type Aida.JSON.Tag_Id_T;

   use type Aida.Types.Int32_T;
   use type Aida.Json_Parsing_Tests_Model.Extended_Person_Array_Index_T;
   use type Aida.Json_Parsing_Tests_Model.Person_Def.Age_T;

   type Storage_T is record
      Person : Json_Parsing_Tests_Model.People_T := (others => (Age  => 10,
                                                                Name => Make));
   end record;

   Storage : Storage_T;

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON.Generic_Parse_JSON package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_0'Access, "Test_Person_With_Name_Adam_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_1'Access, "Test_Person_With_Name_Adam_1");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_0'Access, "Test_Person_With_Hand_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Vehicles_0'Access, "Test_Person_With_Vehicles_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_And_Age_0'Access, "Test_Person_With_Name_Adam_And_Age_0");
   end Initialize;

   procedure Unused_Value_String (Result      : in out Storage_T;
                                  Value       : Aida.Types.String_T;
                                  Tag_Id      : Aida.JSON.Tag_Id_T;
                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Value_String (Result      : in out Storage_T;
                                  Value       : Aida.Types.String_T;
                                  Tag_Id      : Aida.JSON.Tag_Id_T;
                                  Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Value);
      pragma Unreferenced (Tag_Id);
   begin
      Initialize (Call_Result, "69171640-accb-4b2c-b8d6-07d36b2e33b2");
   end Unused_Value_String;

   procedure Unused_Value_Integer (Result      : in out Storage_T;
                                   Value       : Aida.Types.Int32_T;
                                   Tag_Id      : Aida.JSON.Tag_Id_T;
                                   Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Value_Integer (Result      : in out Storage_T;
                                   Value       : Aida.Types.Int32_T;
                                   Tag_Id      : Aida.JSON.Tag_Id_T;
                                   Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Value);
      pragma Unreferenced (Tag_Id);
   begin
      Initialize (Call_Result, "baf7fe57-da60-4245-af9d-2f4a81b007e5");
   end Unused_Value_Integer;

   procedure Unused_Array_Start (Result      : in out Storage_T;
                                 Tag_Id      : Aida.JSON.Tag_Id_T;
                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Array_Start (Result      : in out Storage_T;
                                 Tag_Id      : Aida.JSON.Tag_Id_T;
                                 Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Tag_Id);
   begin
      Initialize (Call_Result, "0a4c81f4-f6e4-477e-a53d-fb1b1b9ed433");
   end Unused_Array_Start;

   procedure Unused_Array_End (Result      : in out Storage_T;
                               Tag_Id      : Aida.JSON.Tag_Id_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
     Global => null;

   procedure Unused_Array_End (Result      : in out Storage_T;
                               Tag_Id      : Aida.JSON.Tag_Id_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Tag_Id);
   begin
      Initialize (Call_Result, "149f8a60-f48a-4274-baf6-902c0cc00c89");
   end Unused_Array_End;

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
                             Person_Id => Person_Id);
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Name /= "name" then
            Initialize (Call_Result, "6f878261-0825-45af-bab1-caf33d6885b6");
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Value       : Aida.Types.String_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_String (Result      : in out Storage_T;
                              Value       : Aida.Types.String_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
            Initialize (Call_Result, "6b7c4309-53c8-4dfd-b2d6-23ff6ad0e185");
         else
            Initialize (Storage.Person (Person_Id).Name,
                        Value);
         end if;
      end Value_String;

      procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                               Root_Start_Tag,
                                                               Root_End_Tag,
                                                               Key_Name,
                                                               Value_String,
                                                               Unused_Value_Integer,
                                                               Unused_Array_Start,
                                                               Unused_Array_End);

      Call_Result : Aida.JSON.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "{""name"" : ""adam""}";

   begin
      Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

      Parse_XML (Storage, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), "5a84dd71-1bee-4e2c-b7f8-13915f953605");
      Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "87f1346a-607c-4e7a-8a3a-621365c323d9");
      Ahven.Assert (To_String (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) = "adam", "87f1346a-607c-4e7a-8a3a-621365c323d9");
   end Test_Person_With_Name_Adam_0;

   procedure Test_Person_With_Name_Adam_1 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
                             Person_Id => Person_Id);
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Name /= "name" then
            Initialize (Call_Result, "51cb9e84-49f6-404f-a3f6-eae1a33dfc5e");
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Value       : Aida.Types.String_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_String (Result      : in out Storage_T;
                              Value       : Aida.Types.String_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
            Initialize (Call_Result, "7a55eac7-6d2e-4634-9031-5d93f383b657");
         else
            Initialize (Storage.Person (Person_Id).Name,
                        Value);
         end if;
      end Value_String;

      procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                               Root_Start_Tag,
                                                               Root_End_Tag,
                                                               Key_Name,
                                                               Value_String,
                                                               Unused_Value_Integer,
                                                               Unused_Array_Start,
                                                               Unused_Array_End);

      Call_Result : Aida.JSON.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "   {""name"" : ""adam""}";

   begin
      Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

      Parse_XML (Storage, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), "db0c7615-571e-4d07-800f-aec3e5bcffa1");
      Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "61a2be0c-5f7c-40ce-9f5f-4ab8fd312c54");
      Ahven.Assert (To_String (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) = "adam", "8388aac9-2b4e-4780-8189-eb1721a6412f");
   end Test_Person_With_Name_Adam_1;

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
                             Person_Id => Person_Id);
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Name /= "age" then
            Initialize (Call_Result, "dbb5d4c5-330e-4725-8ebd-792b1a596c30");
         end if;
      end Key_Name;

      procedure Value_Integer (Result      : in out Storage_T;
                                      Value       : Aida.Types.Int32_T;
                                      Tag_Id      : Aida.JSON.Tag_Id_T;
                                      Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_Integer (Result      : in out Storage_T;
                               Value       : Aida.Types.Int32_T;
                               Tag_Id      : Aida.JSON.Tag_Id_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Storage.Person (Person_Id).Age := Json_Parsing_Tests_Model.Person_Def.Age_T (Value);
      end Value_Integer;

      procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                               Root_Start_Tag,
                                                               Root_End_Tag,
                                                               Key_Name,
                                                               Unused_Value_String,
                                                               Value_Integer,
                                                               Unused_Array_Start,
                                                               Unused_Array_End);

      Call_Result : Aida.JSON.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "{""age"" : 10}";

   begin
      Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

      Parse_XML (Storage, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
      Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
      Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
   end Test_Person_With_Age_0;

   procedure Test_Person_With_Name_Adam_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
                             Person_Id => Person_Id);
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Name = "name" or Name = "age" then
            null;
         else
            Initialize (Call_Result, "3503ee95-b4ff-46d5-bbd2-c7b695f63b0c");
         end if;
      end Key_Name;

      procedure Value_String (Result      : in out Storage_T;
                              Value       : Aida.Types.String_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_String (Result      : in out Storage_T;
                              Value        : Aida.Types.String_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
            Initialize (Call_Result, "e229c14e-950b-4019-be21-37cf5da0a750");
         else
            Initialize (Storage.Person (Person_Id).Name,
                        Value);
         end if;
      end Value_String;

      procedure Value_Integer (Result      : in out Storage_T;
                               Value       : Aida.Types.Int32_T;
                               Tag_Id      : Aida.JSON.Tag_Id_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_Integer (Result      : in out Storage_T;
                               Value       : Aida.Types.Int32_T;
                               Tag_Id      : Aida.JSON.Tag_Id_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Put_Line (To_String (Value));
         Storage.Person (Person_Id).Age := Json_Parsing_Tests_Model.Person_Def.Age_T (Value);
      end Value_Integer;

      procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                               Root_Start_Tag,
                                                               Root_End_Tag,
                                                               Key_Name,
                                                               Value_String,
                                                               Value_Integer,
                                                               Unused_Array_Start,
                                                               Unused_Array_End);

      Call_Result : Aida.JSON.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "{""name"" : ""bertil"", ""age"" : 5}";

   begin
      Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

      Parse_XML (Storage, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
      Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "41436cda-0c06-4b34-8963-32a6ea0553b6");
      Ahven.Assert (To_String (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) = "bertil", "0c7c52a3-774b-4133-9937-733c539d474f");
      Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 5, "b72edaec-bdb3-4b58-9964-2590c51617b3");
   end Test_Person_With_Name_Adam_And_Age_0;

   procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
                             Person_Id => Person_Id);
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         if Tag_Id = Aida.JSON.Tag_Id_T'First and then Name = "hand" then
            null;
         elsif Tag_Id = Aida.JSON.Tag_Id_T'First + 1 and then Name = "fingers" then
            null;
         else
            Initialize (Call_Result, "8792ce90-b45f-4c6d-a1b4-7b4a3fa22df7");
         end if;
      end Key_Name;

      procedure Value_Integer (Result      : in out Storage_T;
                                      Value       : Aida.Types.Int32_T;
                                      Tag_Id      : Aida.JSON.Tag_Id_T;
                                      Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_Integer (Result      : in out Storage_T;
                               Value       : Aida.Types.Int32_T;
                               Tag_Id      : Aida.JSON.Tag_Id_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Value /= 5 then
            Initialize (Call_Result, "aa44346a-f25a-4c7e-907e-3d62b85ad970");
         end if;
      end Value_Integer;

      procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                               Root_Start_Tag,
                                                               Root_End_Tag,
                                                               Key_Name,
                                                               Unused_Value_String,
                                                               Value_Integer,
                                                               Unused_Array_Start,
                                                               Unused_Array_End);

      Call_Result : Aida.JSON.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "{""hand"" : { ""fingers"" : 5 }}";

   begin
      Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

      Parse_XML (Storage, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
      Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 2, "ae7399ea-3d2a-4400-a10f-34104d439978");
   end Test_Person_With_Hand_0;

   procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : in out Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         Allocate_Person_Id (This      => Aida.Json_Parsing_Tests_Model.Max_Indices,
                             Person_Id => Person_Id);
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : in out Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Root_End_Tag;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Key_Name (Result      : in out Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         if Tag_Id = Aida.JSON.Tag_Id_T'First and then Name = "vehicles" then
            null;
         elsif (Tag_Id = Aida.JSON.Tag_Id_T'First + 1 or Tag_Id = Aida.JSON.Tag_Id_T'First + 2) and then
           Name = "wheels"
         then
            null;
         else
            Initialize (Call_Result, "aea59ef9-feb3-4f2a-a680-477c468da561");
         end if;
      end Key_Name;

      procedure Value_Integer (Result      : in out Storage_T;
                                      Value       : Aida.Types.Int32_T;
                                      Tag_Id      : Aida.JSON.Tag_Id_T;
                                      Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_Integer (Result      : in out Storage_T;
                               Value       : Aida.Types.Int32_T;
                               Tag_Id      : Aida.JSON.Tag_Id_T;
                               Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
      begin
         if Value = 4 or Value = 2 then
            null;
         else
            Initialize (Call_Result, "b14aafea-3be6-4222-b5c0-05506ed37ec7");
         end if;
      end Value_Integer;

      procedure Array_Start (Result      : in out Storage_T;
                             Tag_Id      : Aida.JSON.Tag_Id_T;
                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Array_Start (Result      : in out Storage_T;
                             Tag_Id      : Aida.JSON.Tag_Id_T;
                             Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Array_Start;

      procedure Array_End (Result      : in out Storage_T;
                           Tag_Id      : Aida.JSON.Tag_Id_T;
                           Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Array_End (Result      : in out Storage_T;
                           Tag_Id      : Aida.JSON.Tag_Id_T;
                           Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Array_End;

      procedure Parse_XML is new Aida.JSON.Generic_Parse_JSON (Storage_T,
                                                               Root_Start_Tag,
                                                               Root_End_Tag,
                                                               Key_Name,
                                                               Unused_Value_String,
                                                               Value_Integer,
                                                               Array_Start,
                                                               Array_End);

      Call_Result : Aida.JSON.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "{""vehicles"" : [ {""wheels"" : 4 }, {""wheels"" : 2 } ]}";

   begin
      Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

      Parse_XML (Storage, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
      Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 3, "41abd5c7-8264-4d51-a41f-a704a56ae11e");
   end Test_Person_With_Vehicles_0;

end Aida.JSON_Parsing_Tests;
