with Aida.Text_IO;
with Aida.XML.Generic_Parse_XML_File;
with Aida.Subprogram_Call_Result;

package body Aida.XML_Parsing_Tests is

   use all type Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;

   use type Json_Parsing_Tests_Model.Extended_Person_Id_T;
   use type Json_Parsing_Tests_Model.Extended_Hand_Id_T;
   use type Json_Parsing_Tests_Model.Person_Def.Age_T;
   use type Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T;

   XML_Test_Person_With_Age_0 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>";
   XML_Test_Person_With_Age_1 : constant Aida.String_T := "   <?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>";
   XML_Test_Person_With_Age_2 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?>   <person>10</person>";
   XML_Test_Person_With_Age_3 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>   ";
   XML_Test_Person_With_Age_4 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""utf-8""?><person>10</person>";

   XML_Test_Person_With_Hand_0 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers=""4""></hand></person>";
   XML_Test_Person_With_Hand_1 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers=""4""   ></hand></person>";
   XML_Test_Person_With_Hand_2 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand   fingers=""4""></hand></person>";
   XML_Test_Person_With_Hand_3 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4'></hand></person>";
   XML_Test_Person_With_Hand_4 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4'   ></hand></person>";
   XML_Test_Person_With_Hand_5 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand   fingers='4'></hand></person>";
   XML_Test_Person_With_Hand_6 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4'/></person>";
   XML_Test_Person_With_Hand_7 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4' /></person>";

   XML_Test_Comment_0 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><!-- Some comment --><person>10</person>";

   XML_Test_Person_With_Age_Pre_Comment_0 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><!-- Comment -->10</person>";

   XML_Test_Person_With_Age_Post_Comment_0 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person>10<!-- Comment --></person>";

   XML_Test_Person_With_Age_Pre_CDATA_Comment_0 : constant Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><person><![CDATA[ Important comment ]]>10</person>";

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.XML.Generic_Parse_XML_File package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_1'Access, "Test_Person_With_Age_1");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_2'Access, "Test_Person_With_Age_2");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_3'Access, "Test_Person_With_Age_3");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_4'Access, "Test_Person_With_Age_4");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_0'Access, "Test_Person_With_Hand_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_1'Access, "Test_Person_With_Hand_1");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_2'Access, "Test_Person_With_Hand_2");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_3'Access, "Test_Person_With_Hand_3");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_4'Access, "Test_Person_With_Hand_4");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_5'Access, "Test_Person_With_Hand_5");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_6'Access, "Test_Person_With_Hand_6");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_7'Access, "Test_Person_With_Hand_7");
      Ahven.Framework.Add_Test_Routine (T, Test_Comment_0'Access, "Test_Comment_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_Pre_Comment_0'Access, "Test_Person_With_Age_Pre_Comment_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_Post_Comment_0'Access, "Test_Person_With_Age_Post_Comment_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_Pre_CDATA_Comment_0'Access, "Test_Person_With_Age_Pre_CDATA_Comment_0");
   end Initialize;

   use all type Person_Id_Vector.T;
   use all type Hand_Id_Vector.T;
--   use all type Vehicle_Id_Vector.T;

   type Current_Ids_T is limited record
      Person_Ids  : Person_Id_Vector.T;
      Hand_Ids    : Hand_Id_Vector.T;
      Vehicle_Ids : Vehicle_Id_Vector.T;
   end record;

   procedure Clear (S : in out Storage_T) is
   begin
      for I in Json_Parsing_Tests_Model.Person_Id_T'Range loop
         Clear (S.Person (I).Hands);
         Clear (S.Person (I).Vehicles);
      end loop;
      S.Header_Comment := (others => ' ');
   end Clear;

--   pragma Warnings (Off, """State"" is not modified");
   generic
      type Specific_Storage_T is limited private;
      type Specific_Max_Indices_T is limited private;
      type Specific_State_T is (<>);
      type Specific_Current_Ids_T is limited private;
      Error_Code_1 : Aida.Int32_T;
      Error_Code_2 : Aida.Int32_T;
   procedure Generic_Unused_CDATA (Result      : in out Specific_Storage_T;
                                   Max_Indices : in out Specific_Max_Indices_T;
                                   State       : in out Specific_State_T;
                                   Current_Ids : in out Specific_Current_Ids_T;
                                   Value       : Aida.String_T;
                                   Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;
--   pragma Warnings (On, """State"" is not modified");

   procedure Generic_Unused_CDATA (Result      : in out Specific_Storage_T;
                                   Max_Indices : in out Specific_Max_Indices_T;
                                   State       : in out Specific_State_T;
                                   Current_Ids : in out Specific_Current_Ids_T;
                                   Value       : Aida.String_T;
                                   Call_Result : in out Aida.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Result);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Value);
      pragma Unreferenced (State);
   begin
      Call_Result.Initialize (Error_Code_1, Error_Code_2);
   end Generic_Unused_CDATA;

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
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified");
      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified");

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Utils;

   package body Test_Person_With_Age_Utils with SPARK_Mode is

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         case State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Person_Id_Vector.Last_Index (Current_Ids.Person_Ids) < Person_Id_Vector.Max_Index (Current_Ids.Person_Ids)
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
                  Call_Result.Initialize (-0541695571, 1841730510);
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-0523422474, 2105437997);
         end case;
      end Start_Tag;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
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
                  Call_Result.Initialize ("0BC27327-A8A1-433D-B035-8FE65A43972F");
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("231A170D-9AFF-4C13-8490-51A80ED3A149");
         end case;
      end End_Tag;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
      begin
         case State is
            when Expecting_Age_Value  =>
               if Is_Empty (Current_Ids.Person_Ids) then
                  Call_Result.Initialize ("5B805BC2-E44C-4B38-A0F7-B78F0CB8EFBD");
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                        Call_Result.Initialize ("0308B636-FD0C-4153-BC0D-D017441556AB");
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
               Call_Result.Initialize ("95E7AC3C-B7A1-4376-81D1-4AC559EE8E03");
         end case;
      end Text;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("5DFB9B72-5794-4749-9A69-C662CB7259AA");
      end Attribute;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("7D2ECEB3-7D76-446B-B2BC-E56985322B37");
      end Comment;

      procedure CDATA is new Generic_Unused_CDATA (Storage_T,
                                                   Max_Indices_T,
                                                   State_T,
                                                   Current_Ids_T,
                                                   "481F859F-7E3C-431C-82C1-6F2060CC4AEC");

      procedure Run_Test (XML : Aida.String_T) is

         procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage_T,
                                                                     Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                     State_T,
                                                                     Current_Ids_T,
                                                                     Start_Tag,
                                                                     End_Tag,
                                                                     Text,
                                                                     Attribute,
                                                                     Comment,
                                                                     CDATA);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Person_Start_Tag;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
         Clear (Storage);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Message (Call_Result)));
         Ahven.Assert (State = Final_State, "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Last_Index (Current_Ids.Person_Ids) >= First_Index (Current_Ids.Person_Ids), "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
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

   procedure Test_Person_With_Age_2 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_2);
   end Test_Person_With_Age_2;

   procedure Test_Person_With_Age_3 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_3);
   end Test_Person_With_Age_3;

   procedure Test_Person_With_Age_4 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_4);
   end Test_Person_With_Age_4;

   package Test_Person_With_Hand_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Person_Start_Tag,
                       Expecting_Hand_Start_Tag,
                       Expecting_Hand_Attribute_Fingers,
                       Expecting_Hand_End_Tag,
                       Expecting_Person_End_Tag,
                       End_State
                       );

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Storage         : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Hand_Utils;

   package body Test_Person_With_Hand_Utils with SPARK_Mode is

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) is
      begin
         case State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Max_Indices.Person_Id_Max < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Last_Index (Current_Ids.Person_Ids) < Max_Index (Current_Ids.Person_Ids)
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Person_Id_Vector.Append (Current_Ids.Person_Ids, Person_Id);
                  end;
                  State := Expecting_Hand_Start_Tag;
               else
                  Call_Result.Initialize ("E2CEC9DE-DFD0-4178-89AD-3A98929533BC");
               end if;
            when Expecting_Hand_Start_Tag =>
               if
                 Last_Index (Current_Ids.Person_Ids) >= First_Index (Current_Ids.Person_Ids) and then
                 (
                  Last_Index (Result.Person (Person_Id_Vector.Last_Element (Current_Ids.Person_Ids)).Hands) <
                  Max_Index (Result.Person (Person_Id_Vector.Last_Element (Current_Ids.Person_Ids)).Hands) and
                  Max_Indices.Hand_Id_Max < Json_Parsing_Tests_Model.Extended_Hand_Id_T'Last and
                  Last_Index (Current_Ids.Hand_Ids) < Max_Index (Current_Ids.Hand_Ids))
               then
                  declare
                     Hand_Id : Aida.Json_Parsing_Tests_Model.Hand_Id_T;
                  begin
                     Allocate_Hand_Id (This    => Max_Indices,
                                       Hand_Id => Hand_Id);
                     Hand_Id_Vector.Append (Current_Ids.Hand_Ids, Hand_Id);

                     Append (Result.Person (Person_Id_Vector.Last_Element (Current_Ids.Person_Ids)).Hands, Hand_Id);
                  end;

                  State := Expecting_Hand_Attribute_Fingers;
               else
                  Call_Result.Initialize ("97C2270E-E695-4F6C-83A9-5DB97DB3E3FE");
               end if;
            when Expecting_Hand_Attribute_Fingers |
                 Expecting_Hand_End_Tag |
                 Expecting_Person_End_Tag |
                 End_State =>
               Call_Result.Initialize ("83CD2BB8-346C-4BCA-A874-9093EA9B7ECD");
         end case;
      end Start_Tag;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         case State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  State := End_State;
               else
                  Call_Result.Initialize ("B315E693-8E6F-4A90-BE89-1AA305BDC0BC");
               end if;
            when Expecting_Hand_End_Tag =>
               if Tag_Name = "hand" then
                  State := Expecting_Person_End_Tag;
               else
                  Call_Result.Initialize ("D9186C55-A0A5-4565-9F19-71FFBF3869A7");
               end if;
            when Expecting_Person_Start_Tag |
                 Expecting_Hand_Start_Tag |
                 Expecting_Hand_Attribute_Fingers |
                 End_State =>
               Call_Result.Initialize ("7A269EB3-3E6E-4194-A32F-7C5BF672121C");
         end case;
      end End_Tag;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unmodified (State);
      begin
         case State is
            when Expecting_Hand_Start_Tag |
                 Expecting_Hand_End_Tag |
                 Expecting_Person_End_Tag =>
               if Value = "" then
                  null;
               else
                  Aida.Text_IO.Put_Line (Value);
                  Call_Result.Initialize ("E2FE276D-DE59-476A-8997-7E9E2AD759E5");
               end if;
            when Expecting_Person_Start_Tag |
                 Expecting_Hand_Attribute_Fingers |
                 End_State =>
               Call_Result.Initialize ("5392E79D-7017-444A-BF23-996F28003162");
         end case;
      end Text;

      procedure Attribute (Storage         : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unmodified (Current_Ids);
      begin
         case State is
            when Expecting_Hand_Attribute_Fingers =>

               if Attribute_Name = "fingers" then
                  declare
                     I : Aida.Int32_T;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Attribute_Value,
                               I,
                               Has_Failed);

                     if Has_Failed then
                        Call_Result.Initialize ("F530A269-4528-4CE7-87E7-5B67BDFC04E9");
                     else
                        if
                          Last_Index (Current_Ids.Hand_Ids) >= First_Index (Current_Ids.Hand_Ids) and
                          I >= Aida.Int32_T (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'First) and
                          I <= Aida.Int32_T (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'Last)
                        then
                           Storage.Hand (Last_Element (Current_Ids.Hand_Ids)).Number_Of_Fingers :=
                             Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T (I);
                           State := Expecting_Hand_End_Tag;
                        else
                           Call_Result.Initialize ("92400069-D574-4A3A-89CB-7D8EEA0CB900");
                        end if;
                     end if;
                  end;
               else
                  Call_Result.Initialize ("6CDC4CF7-FAAC-4584-8E15-77A25DB87550");
               end if;
            when Expecting_Hand_Start_Tag |
                 Expecting_Hand_End_Tag |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 End_State =>
               Call_Result.Initialize ("22041BE5-FF1C-4830-9627-2DB1AC172A0A");
         end case;
      end Attribute;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("DF7E7CA8-431D-4CD8-BD42-A3C1139B63B0");
      end Comment;

      procedure CDATA is new Generic_Unused_CDATA (Storage_T,
                                                   Max_Indices_T,
                                                   State_T,
                                                   Current_Ids_T,
                                                   "A8A292BB-E90C-4296-BCA8-C7DD5DB81CF1");

      procedure Run_Test (XML : Aida.String_T) is

         procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage_T,
                                                                     Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                     State_T,
                                                                     Current_Ids_T,
                                                                     Start_Tag,
                                                                     End_Tag,
                                                                     Text,
                                                                     Attribute,
                                                                     Comment,
                                                                     CDATA);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Person_Start_Tag;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
         Clear (Storage);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Message (Call_Result)));
         Ahven.Assert (State = End_State, "397d359d-2d92-462b-8b32-2a4bbdc6ce25");
         Ahven.Assert (Last_Index (Current_Ids.Person_Ids) >= First_Index (Current_Ids.Person_Ids), "810561fa-2c9f-4582-a5cf-10e5abd85113");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "ae7399ea-3d2a-4400-a10f-34104d439978");
         Ahven.Assert (Hand_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "0b77dd49-3cbd-44cd-ab53-9b65d0d75c05");
         if
           (Hand_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and
                Json_Parsing_Tests_Model.Max_Indices.Person_Id_Max > 0) and then (Last_Index (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Hands) >= Max_Index (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Hands))
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
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_0);
   end Test_Person_With_Hand_0;

   procedure Test_Person_With_Hand_1 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_1);
   end Test_Person_With_Hand_1;

   procedure Test_Person_With_Hand_2 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_2);
   end Test_Person_With_Hand_2;

   procedure Test_Person_With_Hand_3 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_3);
   end Test_Person_With_Hand_3;

   procedure Test_Person_With_Hand_4 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_4);
   end Test_Person_With_Hand_4;

   procedure Test_Person_With_Hand_5 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_5);
   end Test_Person_With_Hand_5;

   procedure Test_Person_With_Hand_6 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_6);
   end Test_Person_With_Hand_6;

   procedure Test_Person_With_Hand_7 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Hand_Utils.Run_Test (XML_Test_Person_With_Hand_7);
   end Test_Person_With_Hand_7;

   package Test_Comment_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Header_Comment,
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
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Comment_Utils;

   package body Test_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         case State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Last_Index (Current_Ids.Person_Ids) < Max_Index (Current_Ids.Person_Ids)
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Append (Current_Ids.Person_Ids, Person_Id);
                  end;
                  State := Expecting_Age_Value;
               else
                  Call_Result.Initialize ("F44C193B-512E-4526-A64D-79881E6581F0");
               end if;
            when Expecting_Header_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("9881A316-58D9-4D6E-BBA8-2A01594009DD");
         end case;
      end Start_Tag;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
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
                  Call_Result.Initialize ("183C4C24-23D5-4F75-BEF6-23A138EE7125");
               end if;
            when Expecting_Header_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("83434CB5-7AF9-48C7-90FD-E4094F99C9C3");
         end case;
      end End_Tag;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unmodified (Current_Ids);
      begin
         case State is
            when Expecting_Age_Value  =>
               if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
                  Call_Result.Initialize ("8C12D298-7AB9-47C3-8C93-68B065329AA4");
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                        Call_Result.Initialize ("042908D3-BBDB-4D91-993D-5F0C13FAE471");
                     else
                        Initialize (Result.Person (Person_Id).Name,
                                    Value);
                     end if;
                  end;
                  State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Header_Comment |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("4472243F-1BA8-4342-884A-8E60314BF1E7");
         end case;
      end Text;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("0D7C8F8E-BF4A-4194-9C3B-1A72A308C402");
      end Attribute;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         case State is
            when Expecting_Header_Comment =>
               if Result.Header_Comment'Length > Value'Length then
                  Result.Header_Comment (1..Value'Length) := Value (Value'Range);
                  State := Expecting_Person_Start_Tag;
               else
                  Call_Result.Initialize ("9191A811-251E-4446-B313-F47AD50A362E");
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("4472243F-1BA8-4342-884A-8E60314BF1E7");
         end case;
      end Comment;

      procedure CDATA is new Generic_Unused_CDATA (Storage_T,
                                                   Max_Indices_T,
                                                   State_T,
                                                   Current_Ids_T,
                                                   "AE9C3770-38B0-4850-94ED-84F41D7B62F9");

      procedure Run_Test (XML : Aida.String_T) is

         procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage_T,
                                                                     Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                     State_T,
                                                                     Current_Ids_T,
                                                                     Start_Tag,
                                                                     End_Tag,
                                                                     Text,
                                                                     Attribute,
                                                                     Comment,
                                                                     CDATA);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Header_Comment;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
         Clear (Storage);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Message (Call_Result)));
         Ahven.Assert (State = Final_State, "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Last_Index (Current_Ids.Person_Ids) >= First_Index (Current_Ids.Person_Ids), "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         if
           Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and then
           Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) <= Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name.Maximum_Length
         then
            Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
         end if;

         Ahven.Assert (Storage.Header_Comment (1..14) = " Some comment ", "1DC369DF-0657-46DE-8E45-711F49789555");
      end Run_Test;

   end Test_Comment_Utils;

   procedure Test_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Comment_Utils.Run_Test (XML_Test_Comment_0);
   end Test_Comment_0;

   package Test_Person_With_Age_Pre_Comment_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Person_Start_Tag,
                       Expecting_Pre_Age_Text,
                       Expecting_Pre_Age_Comment,
                       Expecting_Age_Value,
                       Expecting_Person_End_Tag,
                       Final_State
                       );

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Pre_Comment_Utils;

   package body Test_Person_With_Age_Pre_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         case State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Last_Index (Current_Ids.Person_Ids) < Max_Index (Current_Ids.Person_Ids)
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Append (Current_Ids.Person_Ids, Person_Id);
                  end;
                  State := Expecting_Pre_Age_Text;
               else
                  Call_Result.Initialize ("95BA939B-D685-484D-BBC3-E25A1494925D");
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("38A215E2-5587-4FDC-B8A2-69A9546718C5");
         end case;
      end Start_Tag;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
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
                  Call_Result.Initialize ("733D4C3B-5724-4A5C-9836-77B86BA348CC");
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("E8859A95-D6F9-4923-80BC-0D3F139BBD79");
         end case;
      end End_Tag;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unmodified (Current_Ids);
      begin
         case State is
            when Expecting_Age_Value  =>
               if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
                  Call_Result.Initialize ("F434F73C-825A-4B4E-A2EF-DC2A52DCA28D");
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                        Call_Result.Initialize ("A8DFF685-A883-4149-819D-9C0E039D38C3");
                     else
                        Initialize (Result.Person (Person_Id).Name,
                                    Value);
                     end if;
                  end;
                  State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Pre_Age_Text =>
               if Value = "" then
                  State := Expecting_Pre_Age_Comment;
               else
                  Call_Result.Initialize ("F98DD5FC-6225-4435-A285-850754C909DC");
               end if;
            when Expecting_Pre_Age_Comment |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("AF37FE00-80CF-4A62-BF4A-E467E2145739");
         end case;
      end Text;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("46059C70-1275-4D1E-BB61-EC6B8B87CEC5");
      end Attribute;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         case State is
            when Expecting_Pre_Age_Comment =>
               if Result.Header_Comment'Length > Value'Length then
                  Result.Header_Comment (1..Value'Length) := Value (Value'Range);
                  State := Expecting_Age_Value;
               else
                  Call_Result.Initialize ("F4DF3CE3-6BB8-4466-BB3F-EB6C2A299A5C");
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("DA8DFC69-0308-47D8-BC48-9757FAB1AB5B");
         end case;
      end Comment;

      procedure CDATA is new Generic_Unused_CDATA (Storage_T,
                                                   Max_Indices_T,
                                                   State_T,
                                                   Current_Ids_T,
                                                   "97181932-85F2-48C5-B6A0-8B49F7D8B39B");

      procedure Run_Test (XML : Aida.String_T) is

         procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage_T,
                                                                     Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                     State_T,
                                                                     Current_Ids_T,
                                                                     Start_Tag,
                                                                     End_Tag,
                                                                     Text,
                                                                     Attribute,
                                                                     Comment,
                                                                     CDATA);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Person_Start_Tag;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
         Clear (Storage);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Message (Call_Result)));
         Ahven.Assert (State = Final_State, "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Last_Index (Current_Ids.Person_Ids) >= First_Index (Current_Ids.Person_Ids), "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         if
           Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and then
           Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) <= Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name.Maximum_Length
         then
            Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
         end if;

         Ahven.Assert (Storage.Header_Comment (1..9) = " Comment ", "1DC369DF-0657-46DE-8E45-711F49789555");
      end Run_Test;

   end Test_Person_With_Age_Pre_Comment_Utils;

   procedure Test_Person_With_Age_Pre_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Pre_Comment_Utils.Run_Test (XML_Test_Person_With_Age_Pre_Comment_0);
   end Test_Person_With_Age_Pre_Comment_0;

   package Test_Person_With_Age_Post_Comment_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Person_Start_Tag,
                       Expecting_Age_Value,
                       Expecting_Post_Age_Comment,
                       Expecting_Post_Age_Text,
                       Expecting_Person_End_Tag,
                       Final_State
                       );

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Post_Comment_Utils;

   package body Test_Person_With_Age_Post_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         case State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Last_Index (Current_Ids.Person_Ids) < Max_Index (Current_Ids.Person_Ids)
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Append (Current_Ids.Person_Ids, Person_Id);
                  end;
                  State := Expecting_Age_Value;
               else
                  Call_Result.Initialize ("95BA939B-D685-484D-BBC3-E25A1494925D");
               end if;
            when Expecting_Post_Age_Text |
                 Expecting_Post_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("38A215E2-5587-4FDC-B8A2-69A9546718C5");
         end case;
      end Start_Tag;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
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
                  Call_Result.Initialize ("733D4C3B-5724-4A5C-9836-77B86BA348CC");
               end if;
            when Expecting_Post_Age_Text |
                 Expecting_Post_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("E8859A95-D6F9-4923-80BC-0D3F139BBD79");
         end case;
      end End_Tag;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unmodified (Current_Ids);
      begin
         case State is
            when Expecting_Age_Value  =>
               if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
                  Call_Result.Initialize ("F434F73C-825A-4B4E-A2EF-DC2A52DCA28D");
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                        Call_Result.Initialize ("A8DFF685-A883-4149-819D-9C0E039D38C3");
                     else
                        Initialize (Result.Person (Person_Id).Name,
                                    Value);
                     end if;
                  end;
                  State := Expecting_Post_Age_Comment;
               end if;
            when Expecting_Post_Age_Text =>
               if Value = "" then
                  State := Expecting_Person_End_Tag;
               else
                  Call_Result.Initialize ("F98DD5FC-6225-4435-A285-850754C909DC");
               end if;
            when Expecting_Post_Age_Comment |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("AF37FE00-80CF-4A62-BF4A-E467E2145739");
         end case;
      end Text;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("46059C70-1275-4D1E-BB61-EC6B8B87CEC5");
      end Attribute;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         case State is
            when Expecting_Post_Age_Comment =>
               if Result.Header_Comment'Length > Value'Length then
                  Result.Header_Comment (1..Value'Length) := Value (Value'Range);
                  State := Expecting_Post_Age_Text;
               else
                  Call_Result.Initialize ("F4DF3CE3-6BB8-4466-BB3F-EB6C2A299A5C");
               end if;
            when Expecting_Post_Age_Text |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("DA8DFC69-0308-47D8-BC48-9757FAB1AB5B");
         end case;
      end Comment;

      procedure CDATA is new Generic_Unused_CDATA (Storage_T,
                                                   Max_Indices_T,
                                                   State_T,
                                                   Current_Ids_T,
                                                   "BAB89E5C-8186-4723-9567-A707016EAFB8");
      procedure Run_Test (XML : Aida.String_T) is

         procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage_T,
                                                                     Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                     State_T,
                                                                     Current_Ids_T,
                                                                     Start_Tag,
                                                                     End_Tag,
                                                                     Text,
                                                                     Attribute,
                                                                     Comment,
                                                                     CDATA);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Person_Start_Tag;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
         Clear (Storage);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Message (Call_Result)));
         Ahven.Assert (State = Final_State, "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Last_Index (Current_Ids.Person_Ids) >= First_Index (Current_Ids.Person_Ids), "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         if
           Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and then
           Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) <= Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name.Maximum_Length
         then
            Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
         end if;

         Ahven.Assert (Storage.Header_Comment (1..9) = " Comment ", "1DC369DF-0657-46DE-8E45-711F49789555");
      end Run_Test;

   end Test_Person_With_Age_Post_Comment_Utils;

   procedure Test_Person_With_Age_Post_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Post_Comment_Utils.Run_Test (XML_Test_Person_With_Age_Post_Comment_0);
   end Test_Person_With_Age_Post_Comment_0;

   package Test_Person_With_Age_Pre_CDATA_Comment_Utils with SPARK_Mode is

      type State_T is (
                       Expecting_Person_Start_Tag,
                       Expecting_Pre_Age_Text,
                       Expecting_Pre_Age_CDATA,
                       Expecting_Age_Value,
                       Expecting_Person_End_Tag,
                       Final_State
                       );

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure CDATA (Result      : in out Storage_T;
                       Max_Indices : in out Max_Indices_T;
                       State       : in out State_T;
                       Current_Ids : in out Current_Ids_T;
                       Value       : Aida.String_T;
                       Call_Result : in out Aida.Subprogram_Call_Result.T) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : Aida.String_T) with
        Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices)),
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Pre_CDATA_Comment_Utils;

   package body Test_Person_With_Age_Pre_CDATA_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Result      : in out Storage_T;
                           Max_Indices : in out Max_Indices_T;
                           State       : in out State_T;
                           Current_Ids : in out Current_Ids_T;
                           Tag_Name    : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
      begin
         case State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Person_Id_Max (Max_Indices) < Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                 Last_Index (Current_Ids.Person_Ids) < Max_Index (Current_Ids.Person_Ids)
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Allocate_Person_Id (This      => Max_Indices,
                                         Person_Id => Person_Id);
                     Append (Current_Ids.Person_Ids, Person_Id);
                  end;
                  State := Expecting_Pre_Age_Text;
               else
                  Call_Result.Initialize ("95BA939B-D685-484D-BBC3-E25A1494925D");
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_CDATA |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("38A215E2-5587-4FDC-B8A2-69A9546718C5");
         end case;
      end Start_Tag;

      procedure End_Tag (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Tag_Name    : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
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
                  Call_Result.Initialize ("733D4C3B-5724-4A5C-9836-77B86BA348CC");
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_CDATA |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("E8859A95-D6F9-4923-80BC-0D3F139BBD79");
         end case;
      end End_Tag;

      procedure Text (Result      : in out Storage_T;
                      Max_Indices : in out Max_Indices_T;
                      State       : in out State_T;
                      Current_Ids : in out Current_Ids_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unmodified (Current_Ids);
      begin
         case State is
            when Expecting_Age_Value  =>
               if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
                  Call_Result.Initialize ("F434F73C-825A-4B4E-A2EF-DC2A52DCA28D");
               else
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
                       Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
                  begin
                     if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
                        Call_Result.Initialize ("A8DFF685-A883-4149-819D-9C0E039D38C3");
                     else
                        Initialize (Result.Person (Person_Id).Name,
                                    Value);
                     end if;
                  end;
                  State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Pre_Age_Text =>
               if Value = "" then
                  State := Expecting_Pre_Age_CDATA;
               else
                  Call_Result.Initialize ("F98DD5FC-6225-4435-A285-850754C909DC");
               end if;
            when Expecting_Pre_Age_CDATA |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize ("AF37FE00-80CF-4A62-BF4A-E467E2145739");
         end case;
      end Text;

      procedure Attribute (Result          : in out Storage_T;
                           Max_Indices     : in out Max_Indices_T;
                           State           : in out State_T;
                           Current_Ids     : in out Current_Ids_T;
                           Attribute_Name  : Aida.String_T;
                           Attribute_Value : Aida.String_T;
                           Call_Result     : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("46059C70-1275-4D1E-BB61-EC6B8B87CEC5");
      end Attribute;

      procedure Comment (Result      : in out Storage_T;
                         Max_Indices : in out Max_Indices_T;
                         State       : in out State_T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
         pragma Unreferenced (Value);
         pragma Unreferenced (State);
      begin
         Call_Result.Initialize ("DA8DFC69-0308-47D8-BC48-9757FAB1AB5B");
      end Comment;

      procedure CDATA (Result      : in out Storage_T;
                       Max_Indices : in out Max_Indices_T;
                       State       : in out State_T;
                       Current_Ids : in out Current_Ids_T;
                       Value       : Aida.String_T;
                       Call_Result : in out Aida.Subprogram_Call_Result.T)
      is
         pragma Unreferenced (Max_Indices);
         pragma Unreferenced (Current_Ids);
      begin
         case State is
            when Expecting_Pre_Age_CDATA =>
               if Result.Header_Comment'Length > Value'Length then
                  Result.Header_Comment (1..Value'Length) := Value (Value'Range);
                  State := Expecting_Age_Value;
               else
                  Call_Result.Initialize ("D087DA52-AFEB-48F9-B767-87791980B20D");
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize ("EC0B3613-D72C-42F4-A55D-4667C3B0D378");
         end case;
      end CDATA;

      procedure Run_Test (XML : Aida.String_T) is

         procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage_T,
                                                                     Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T,
                                                                     State_T,
                                                                     Current_Ids_T,
                                                                     Start_Tag,
                                                                     End_Tag,
                                                                     Text,
                                                                     Attribute,
                                                                     Comment,
                                                                     CDATA);

         Call_Result : Aida.Subprogram_Call_Result.T;

         State : State_T := Expecting_Person_Start_Tag;

         Current_Ids : Current_Ids_T;
      begin
         Clear (Aida.Json_Parsing_Tests_Model.Max_Indices);
         Clear (Storage);

         Parse_XML (Storage,
                    Aida.Json_Parsing_Tests_Model.Max_Indices,
                    State,
                    Current_Ids,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, String (Message (Call_Result)));
         Ahven.Assert (State = Final_State, "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Last_Index (Current_Ids.Person_Ids) >= First_Index (Current_Ids.Person_Ids), "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         if
           Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices) > 0 and then
           Length (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name) <= Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Name.Maximum_Length
         then
            Ahven.Assert (Storage.Person (Person_Id_Max (Json_Parsing_Tests_Model.Max_Indices)).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
         end if;

         Ahven.Assert (Storage.Header_Comment (1..19) = " Important comment ", "1DC369DF-0657-46DE-8E45-711F49789555");
      end Run_Test;

   end Test_Person_With_Age_Pre_CDATA_Comment_Utils;

   procedure Test_Person_With_Age_Pre_CDATA_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Pre_CDATA_Comment_Utils.Run_Test (XML_Test_Person_With_Age_Pre_CDATA_Comment_0);
   end Test_Person_With_Age_Pre_CDATA_Comment_0;

end Aida.XML_Parsing_Tests;
