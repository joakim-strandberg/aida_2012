with Aida.Bounded_String;
--with Aida.Text_IO;
with Aida.XML.Generic_Parse_XML_File;

package body Aida.XML_Parsing_Tests is

   use all type Aida.String_T;
   use all type Aida.XML.Procedure_Call_Result.T;
   use all type Aida.Bounded_String.T;
   use all type Aida.String_T;
   use all type Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
   use all type Aida.Json_Parsing_Tests_Model.Person_T;
   use all type Aida.Json_Parsing_Tests_Model.Hand_T;
   use all type Aida.Json_Parsing_Tests_Model.Vehicle_T;

   use type Aida.Int32_T;
   use type Json_Parsing_Tests_Model.Extended_Person_Id_T;
   use type Json_Parsing_Tests_Model.Extended_Hand_Id_T;
   use type Json_Parsing_Tests_Model.Extended_Vehicle_Id_T;
   use type Json_Parsing_Tests_Model.Person_Def.Age_T;
   use type Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T;
   use type Json_Parsing_Tests_Model.Vehicle_Def.Wheels_T;
   use type Json_Parsing_Tests_Model.Person_Def.Hand_Vector_Index_T;
   use type Json_Parsing_Tests_Model.Person_Def.Vehicle_Vector_Index_T;

   XML_Test_Person_With_Age_0            : constant Aida.String_T :=
     "<?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>";
   XML_Test_Person_With_Age_1            : constant Aida.String_T :=
     "   <?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>";

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.XML.Generic_Parse_XML_File package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_1'Access, "Test_Person_With_Age_1");
   end Initialize;

   use all type Person_Id_Vector.T;
   use all type Hand_Id_Vector.T;
   use all type Vehicle_Id_Vector.T;

   type Current_Ids_T is limited record
      Person_Ids  : Person_Id_Vector.T;
      Hand_Ids    : Hand_Id_Vector.T;
      Vehicle_Ids : Vehicle_Id_Vector.T;
   end record;

   package Test_Person_With_Age_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Person_Start_Tag,
                       Expecting_Age_Value,
                       Expecting_Person_End_Tag,
                       Final_State
                       );

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.XML.Procedure_Call_Result.T) with
        Global => null;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.XML.Procedure_Call_Result.T) with
        Global => null;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.XML.Procedure_Call_Result.T) with
        Global => null;

      procedure Run_Test (XML : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Utils;

   package body Test_Person_With_Age_Utils with SPARK_Mode is

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.XML.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         case State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Person_Id_Vector.Length (Current_Ids.Person_Ids) < Person_Id_Vector.Length_T'Last
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
                  end;
                  State := Expecting_Age_Value;
               else
                  Initialize (Call_Result, "8D45ECC2-ACC9-478F-8417-9BE2B351F40B");
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Initialize (Call_Result, "FA61D218-20B3-4620-8898-8DDCA6091EC6");
         end case;
      end Start_Tag;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.XML.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         case State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  State := Final_State;
               else
                  Initialize (Call_Result, "0BC27327-A8A1-433D-B035-8FE65A43972F");
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Initialize (Call_Result, "231A170D-9AFF-4C13-8490-51A80ED3A149");
         end case;
      end End_Tag;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.XML.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
      begin
         case State is
            when Expecting_Age_Value  =>
               if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
                  Initialize (Call_Result, "5B805BC2-E44C-4B38-A0F7-B78F0CB8EFBD");
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                        Initialize (Call_Result, "0308B636-FD0C-4153-BC0D-D017441556AB");
                     else
                        Initialize (Result.Person (Person_Id).Name,
                                    Value);
                     end if;
                  end;
                  State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Initialize (Call_Result, "95E7AC3C-B7A1-4376-81D1-4AC559EE8E03");
         end case;
      end Text;

      procedure Run_Test (XML : Aida.String_T) is

         procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage_T,
                                                                     Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                     State_T,
                                                                     Current_Ids_T,
                                                                     Start_Tag,
                                                                     End_Tag,
                                                                     Text);

         Call_Result : Aida.XML.Procedure_Call_Result.T;

         State : State_T := Expecting_Person_Start_Tag;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    XML,
                    Call_Result);

         Ahven.Assert (not Has_Failed (Call_Result), String (Message (Call_Result)));
         Ahven.Assert (State = Final_State, "592cbd68-ef97-4fc1-934b-80111d24fd32");
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
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_0);
   end Test_Person_With_Age_0;

   procedure Test_Person_With_Age_1 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_1);
   end Test_Person_With_Age_1;

end Aida.XML_Parsing_Tests;
