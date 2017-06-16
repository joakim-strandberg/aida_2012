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

   use type Aida.JSON.Tag_Id_T;
   use type Aida.Json_Parsing_Tests_Model.Extended_Person_Array_Index_T;

   type Storage_T is record
      Person : Json_Parsing_Tests_Model.People_T := (others => (Age  => 10,
                                                                Name => Json_Parsing_Tests_Model.Person_Def.Make));
   end record;

   Storage : Storage_T;

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON.Generic_Parse_JSON package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Initialization'Access, "Test_Initialization");
   end Initialize;

   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;

      procedure Root_Start_Tag (Result      : Storage_T;
                                Tag_Id      : Aida.JSON.Tag_Id_T;
                                Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : Storage_T;
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

      procedure Root_End_Tag (Result      : Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : Storage_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
         null;
      end Root_End_Tag;

      procedure Key_Name (Result      : Storage_T;
                          Name        : Aida.Types.String_T;
                          Tag_Id      : Aida.JSON.Tag_Id_T;
                          Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Key_Name (Result      : Storage_T;
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

      procedure Value_String (Result      : Storage_T;
                              Value        : Aida.Types.String_T;
                              Tag_Id      : Aida.JSON.Tag_Id_T;
                              Call_Result : in out Aida.JSON.Procedure_Call_Result.T) with
        Global => null;

      procedure Value_String (Result      : Storage_T;
                              Value        : Aida.Types.String_T;
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
                                                               Value_String);

      Call_Result : Aida.JSON.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "{""name"", ""adam""}";

   begin
      Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

      Parse_XML (Storage, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), "5a84dd71-1bee-4e2c-b7f8-13915f953605");
      Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "87f1346a-607c-4e7a-8a3a-621365c323d9");
      Ahven.Assert (To_String (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) = "adam", "87f1346a-607c-4e7a-8a3a-621365c323d9");
   end Test_Initialization;

end Aida.JSON_Parsing_Tests;
