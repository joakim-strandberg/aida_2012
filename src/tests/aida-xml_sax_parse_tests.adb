with Aida.XML_SAX_Parse;

package body Aida.XML_SAX_Parse_Tests is

   use type Json_Parsing_Tests_Model.Extended_Person_Id_T;
   use type Json_Parsing_Tests_Model.Extended_Hand_Id_T;
   use type Json_Parsing_Tests_Model.Person_Def.Age_T;
   use type Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T;

   XML_Test_Person_With_Age_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>";
   XML_Test_Person_With_Age_1 : constant String
     := "   <?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>";
   XML_Test_Person_With_Age_2 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?>   <person>10</person>";
   XML_Test_Person_With_Age_3 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person>10</person>   ";
   XML_Test_Person_With_Age_4 : constant String
     := "<?xml version=""1.0"" encoding=""utf-8""?><person>10</person>";

   XML_Test_Person_With_Hand_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers=""4""></hand></person>";
   XML_Test_Person_With_Hand_1 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers=""4""   ></hand></person>";
   XML_Test_Person_With_Hand_2 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand   fingers=""4""></hand></person>";
   XML_Test_Person_With_Hand_3 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4'></hand></person>";
   XML_Test_Person_With_Hand_4 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4'   ></hand></person>";
   XML_Test_Person_With_Hand_5 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand   fingers='4'></hand></person>";
   XML_Test_Person_With_Hand_6 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4'/></person>";
   XML_Test_Person_With_Hand_7 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><hand fingers='4' /></person>";

   XML_Test_Comment_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><!-- Some comment --><person>10</person>";

   XML_Test_Person_With_Age_Pre_Comment_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><!-- Comment -->10</person>";

   XML_Test_Person_With_Age_Post_Comment_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person>10<!-- Comment --></person>";

   XML_Test_Person_With_Age_Pre_CDATA_Comment_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person><![CDATA[ Important comment ]]>10</person>";

   XML_Test_Person_With_Single_Quote_Attribute_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person name='""joe"" or ""john""'></person>";

   XML_Test_Person_With_Double_Quote_Attribute_0 : constant String
     := "<?xml version=""1.0"" encoding=""UTF-8""?><person name=""'joe' or 'john'""></person>";

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.XML_SAX_Parse package tests");

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
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Single_Quote_Attribute_0'Access, "Test_Person_With_Single_Quote_Attribute_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Double_Quote_Attribute_0'Access, "Test_Person_With_Double_Quote_Attribute_0");
   end Initialize;

   type Current_Ids_T is limited record
      Person_Ids  : Person_Id_Vector.T;
      Hand_Ids    : Hand_Id_Vector.T;
      Vehicle_Ids : Vehicle_Id_Vector.T;
   end record;

   procedure Clear (S : in out Storage_T) is
   begin
      for I in Json_Parsing_Tests_Model.Person_Id_T'Range loop
         S.Person (I).Hands.Clear;
         S.Person (I).Vehicles.Clear;
      end loop;
      S.Header_Comment := (others => ' ');
   end Clear;

   package Test_Person_With_Age_Utils with SPARK_Mode is

      type State_T is
        (
         Expecting_Person_Start_Tag,
         Expecting_Age_Value,
         Expecting_Person_End_Tag,
         Final_State
        );

      type Argument_Type is record
         Storage     : Storage_T;
         Max_Indices : Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
         State       : State_T := Expecting_Person_Start_Tag;
         Current_Ids : Current_Ids_T;
      end record;

      procedure Start_Tag
        (Argument    : in out Argument_Type;
         Tag_Name    : String;
         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      pragma Warnings (Off, """Current_Ids"" is not modified");
      procedure Text
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;
      pragma Warnings (On, """Current_Ids"" is not modified");

      procedure Attribute
        (Argument        : in out Argument_Type;
         Attribute_Name  : String;
         Attribute_Value : String;
         Call_Result     : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : String) with
        Global => null,
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Utils;

   package body Test_Person_With_Age_Utils with SPARK_Mode is

      procedure Start_Tag
        (Argument    : in out Argument_Type;
         Tag_Name    : String;
         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Argument.Max_Indices.Person_Id_Max <
                   Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                   not Argument.Current_Ids.Person_Ids.Is_Full
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Person_Id (Person_Id);
                     Argument.Current_Ids.Person_Ids.Append (Person_Id);
                  end;
                  Argument.State := Expecting_Age_Value;
               else
                  Call_Result.Initialize (-0541695571, 1841730510);
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-0523422474, 2105437997);
         end case;
      end Start_Tag;

      procedure End_Tag
        (Argument    : in out Argument_Type;
         Tag_Name    : String;
         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  Argument.State := Final_State;
               else
                  Call_Result.Initialize (1597610772, -2107719936);
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (0945956380, -0903409252);
         end case;
      end End_Tag;

      procedure Text
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Age_Value  =>
               if Argument.Current_Ids.Person_Ids.Is_Empty then
                  Call_Result.Initialize (0358232752, -1204136542);
               else
                  if
                    Value'Length > Argument.Storage.Person
                      (Argument.Current_Ids.Person_Ids.Last_Element).Max_Name_Size
                  then
                     Call_Result.Initialize (1091656843, 0458821974);
                  else
                     Argument.Storage.Person
                       (
                        Argument.Current_Ids.Person_Ids.Last_Element
                       ).Set_Name (Value);
                  end if;
                  Argument.State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (1306774674, 1255183792);
         end case;
      end Text;

      procedure Attribute
        (Argument        : in out Argument_Type;
         Attribute_Name  : String;
         Attribute_Value : String;
         Call_Result     : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
      begin
         Call_Result.Initialize (0652491626, 0551170323);
      end Attribute;

      procedure Comment
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0343657120, 1539002226);
      end Comment;

      procedure Unused_CDATA
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (0724097972, -0357024737);
      end Unused_CDATA;

      procedure Run_Test (XML : String) is

         procedure Parse_XML is new Aida.XML_SAX_Parse
           (Argument_Type,
            Start_Tag,
            End_Tag,
            Text,
            Attribute,
            Comment,
            Unused_CDATA);

         Call_Result : Aida.Call_Result;

         Argument : Argument_Type;
      begin
         Argument.Max_Indices.Clear;
         Argument.Storage.Clear;

         Parse_XML (Argument,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);
         Ahven.Assert (Argument.State = Final_State,
                       "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Argument.Current_Ids.Person_Ids.Is_Non_Empty,
                       "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Argument.Max_Indices.Person_Id_Max = 1,
                       "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed,
                       "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         Ahven.Assert (Argument.Storage.Person
                       (Argument.Max_Indices.Person_Id_Max).Age = 10,
                       "1d10d12b-c726-40aa-881b-8374801f539e");
      end Run_Test;

   end Test_Person_With_Age_Utils;

   procedure Test_Person_With_Age_0
     (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_0);
   end Test_Person_With_Age_0;

   procedure Test_Person_With_Age_1
     (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_1);
   end Test_Person_With_Age_1;

   procedure Test_Person_With_Age_2
     (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_2);
   end Test_Person_With_Age_2;

   procedure Test_Person_With_Age_3
     (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_3);
   end Test_Person_With_Age_3;

   procedure Test_Person_With_Age_4
     (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Utils.Run_Test (XML_Test_Person_With_Age_4);
   end Test_Person_With_Age_4;

   package Test_Person_With_Hand_Utils with SPARK_Mode is

      type State_T is
        (
         Expecting_Person_Start_Tag,
         Expecting_Hand_Start_Tag,
         Expecting_Hand_Attribute_Fingers,
         Expecting_Hand_End_Tag,
         Expecting_Person_End_Tag,
         End_State
        );

      type Argument_Type is record
         Storage     : Storage_T;
         Max_Indices : Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
         State       : State_T := Expecting_Person_Start_Tag;
         Current_Ids : Current_Ids_T;
      end record;

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : String) with
        Global => null,
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Hand_Utils;

   package body Test_Person_With_Hand_Utils with SPARK_Mode is

      procedure Start_Tag
        (Argument    : in out Argument_Type;
         Tag_Name    : String;
         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Argument.Max_Indices.Person_Id_Max <
                   Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                   (not Argument.Current_Ids.Person_Ids.Is_Full)
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Person_Id (Person_Id);
                     Argument.Current_Ids.Person_Ids.Append (Person_Id);
                  end;
                  Argument.State := Expecting_Hand_Start_Tag;
               else
                  Call_Result.Initialize (-1986289250, -0911494611);
               end if;
            when Expecting_Hand_Start_Tag =>
               if
                 Argument.Current_Ids.Person_Ids.Is_Non_Empty and then
                 (not Argument.Storage.Person
                    (
                     Argument.Current_Ids.Person_Ids.Last_Element
                    ).Hands.Is_Full and
                      Argument.Max_Indices.Hand_Id_Max <
                        Json_Parsing_Tests_Model.Extended_Hand_Id_T'Last and
                          not Argument.Current_Ids.Hand_Ids.Is_Full)
               then
                  declare
                     Hand_Id : Aida.Json_Parsing_Tests_Model.Hand_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Hand_Id (Hand_Id);
                     Hand_Id_Vector.Append
                       (Argument.Current_Ids.Hand_Ids, Hand_Id);

                     Argument.Storage.Person
                       (
                        Argument.Current_Ids.Person_Ids.Last_Element
                       ).Hands.Append (Hand_Id);
                  end;

                  Argument.State := Expecting_Hand_Attribute_Fingers;
               else
                  Call_Result.Initialize (-1424208380, -0059734477);
               end if;
            when Expecting_Hand_Attribute_Fingers |
                 Expecting_Hand_End_Tag |
                 Expecting_Person_End_Tag |
                 End_State =>
               Call_Result.Initialize (-2065336582, -1689631337);
         end case;
      end Start_Tag;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  Argument.State := End_State;
               else
                  Call_Result.Initialize (-1177661661, 0292248373);
               end if;
            when Expecting_Hand_End_Tag =>
               if Tag_Name = "hand" then
                  Argument.State := Expecting_Person_End_Tag;
               else
                  Call_Result.Initialize (0923544354, 1403232525);
               end if;
            when Expecting_Person_Start_Tag |
                 Expecting_Hand_Start_Tag |
                 Expecting_Hand_Attribute_Fingers |
                 End_State =>
               Call_Result.Initialize (-1782791758, 0355780636);
         end case;
      end End_Tag;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result)
      is
         pragma Unmodified (Argument);
      begin
         case Argument.State is
            when Expecting_Hand_Start_Tag |
                 Expecting_Hand_End_Tag |
                 Expecting_Person_End_Tag =>
               if Value = "" then
                  null;
               else
                  Call_Result.Initialize (-0239718217, 2014666842);
               end if;
            when Expecting_Person_Start_Tag |
                 Expecting_Hand_Attribute_Fingers |
                 End_State =>
               Call_Result.Initialize (-0303993979, 0911831101);
         end case;
      end Text;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Hand_Attribute_Fingers =>

               if Attribute_Name = "fingers" then
                  declare
                     I : Integer;
                     Has_Failed : Boolean;
                  begin
                     Aida.To_Integer (Attribute_Value,
                                      I,
                                      Has_Failed);

                     if Has_Failed then
                        Call_Result.Initialize (-1415717893, 1464395596);
                     else
                        if
                          Argument.Current_Ids.Hand_Ids.Is_Non_Empty and
                          I >= Integer (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'First) and
                          I <= Integer (Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T'Last)
                        then
                           Argument.Storage.Hand
                             (
                              Argument.Current_Ids.Hand_Ids.Last_Element
                             ).Number_Of_Fingers :=
                               Json_Parsing_Tests_Model.Hand_Def.Number_Of_Fingers_T (I);
                           Argument.State := Expecting_Hand_End_Tag;
                        else
                           Call_Result.Initialize (1981747900, 0463476847);
                        end if;
                     end if;
                  end;
               else
                  Call_Result.Initialize (-1645434181, -0649226499);
               end if;
            when Expecting_Hand_Start_Tag |
                 Expecting_Hand_End_Tag |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 End_State =>
               Call_Result.Initialize (-0413044075, -0334413386);
         end case;
      end Attribute;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0010790613, -1154048092);
      end Comment;

      procedure Unused_CDATA
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-1905595456, -0073330819);
      end Unused_CDATA;

      procedure Run_Test (XML : String) is

         procedure Parse_XML is new Aida.XML_SAX_Parse
           (Argument_Type,
            Start_Tag,
            End_Tag,
            Text,
            Attribute,
            Comment,
            Unused_CDATA);

         Call_Result : Aida.Call_Result;

         Argument : Argument_Type;
      begin
         Argument.Max_Indices.Clear;
         Argument.Storage.Clear;

         Parse_XML (Argument,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);
         Ahven.Assert (Argument.State = End_State,
                       "397d359d-2d92-462b-8b32-2a4bbdc6ce25");
         Ahven.Assert (Argument.Current_Ids.Person_Ids.Is_Non_Empty,
                       "810561fa-2c9f-4582-a5cf-10e5abd85113");
         Ahven.Assert (Argument.Max_Indices.Person_Id_Max = 1,
                       "ae7399ea-3d2a-4400-a10f-34104d439978");
         Ahven.Assert (Argument.Max_Indices.Hand_Id_Max = 1,
                       "0b77dd49-3cbd-44cd-ab53-9b65d0d75c05");
         if
           Argument.Storage.Person
             (Argument.Max_Indices.Person_Id_Max).Hands.Is_Non_Empty
         then
            declare
               Hand_Id : constant Json_Parsing_Tests_Model.Hand_Id_T :=
                 Argument.Storage.Person
                   (Argument.Max_Indices.Person_Id_Max).Hands.Last_Element;
            begin
               Ahven.Assert
                 (Argument.Storage.Hand (Hand_Id).Number_Of_Fingers = 4,
                  "bf757f75-1d4a-425c-9842-27e1f6de2841");
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

      type State_T is
        (
         Expecting_Header_Comment,
         Expecting_Person_Start_Tag,
         Expecting_Age_Value,
         Expecting_Person_End_Tag,
         Final_State
        );

      type Argument_Type is record
         Storage     : Storage_T;
         Max_Indices : Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
         State       : State_T := Expecting_Header_Comment;
         Current_Ids : Current_Ids_T;
      end record;

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : String) with
        Global => null,
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Comment_Utils;

   package body Test_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Argument.Max_Indices.Person_Id_Max <
                   Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                   not Argument.Current_Ids.Person_Ids.Is_Full
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Person_Id (Person_Id);
                     Argument.Current_Ids.Person_Ids.Append (Person_Id);
                  end;
                  Argument.State := Expecting_Age_Value;
               else
                  Call_Result.Initialize (1641034536, -0168331702);
               end if;
            when Expecting_Header_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (1151263009, 0745658434);
         end case;
      end Start_Tag;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  Argument.State := Final_State;
               else
                  Call_Result.Initialize (-0515223676, -0964032298);
               end if;
            when Expecting_Header_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (1261506323, -1295836419);
         end case;
      end End_Tag;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Age_Value  =>
               if Argument.Current_Ids.Person_Ids.Is_Empty then
                  Call_Result.Initialize (-1774337066, -1727962634);
               else
                  if Value'Length >
                    Argument.Storage.Person
                      (
                       Argument.Current_Ids.Person_Ids.Last_Element
                      ).Max_Name_Size
                  then
                     Call_Result.Initialize (-1815221318, -0049022358);
                  else
                     Argument.Storage.Person
                       (
                        Argument.Current_Ids.Person_Ids.Last_Element
                       ).Set_Name (Value);
                  end if;
                  Argument.State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Header_Comment |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (2029897928, -1645278456);
         end case;
      end Text;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
      begin
         Call_Result.Initialize (0903072198, -1070053815);
      end Attribute;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Header_Comment =>
               if Argument.Storage.Header_Comment'Length > Value'Length then
                  Argument.Storage.Header_Comment (1..Value'Length)
                    := Value (Value'Range);
                  Argument.State := Expecting_Person_Start_Tag;
               else
                  Call_Result.Initialize (1393880170, -1219176638);
               end if;
            when Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (0968907604, -1718083384);
         end case;
      end Comment;

      procedure Unused_CDATA
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0933625590, 1250442975);
      end Unused_CDATA;

      procedure Run_Test (XML : String) is

         procedure Parse_XML is new Aida.XML_SAX_Parse
           (Argument_Type,
            Start_Tag,
            End_Tag,
            Text,
            Attribute,
            Comment,
            Unused_CDATA);

         Call_Result : Aida.Call_Result;

         Argument : Argument_Type;
      begin
         Argument.Max_Indices.Clear;
         Clear (Argument.Storage);

         Parse_XML (Argument,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);
         Ahven.Assert (Argument.State = Final_State,
                       "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Argument.Current_Ids.Person_Ids.Is_Non_Empty,
                       "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Argument.Max_Indices.Person_Id_Max = 1,
                       "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         Ahven.Assert
           (Argument.Storage.Person
              (Argument.Max_Indices.Person_Id_Max).Age = 10,
            "1d10d12b-c726-40aa-881b-8374801f539e");

         Ahven.Assert
           (Argument.Storage.Header_Comment (1..14) = " Some comment ",
            "1DC369DF-0657-46DE-8E45-711F49789555");
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

      type State_T is
        (
         Expecting_Person_Start_Tag,
         Expecting_Pre_Age_Text,
         Expecting_Pre_Age_Comment,
         Expecting_Age_Value,
         Expecting_Person_End_Tag,
         Final_State
        );

      type Argument_Type is record
         Storage     : Storage_T;
         Max_Indices : Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
         State       : State_T := Expecting_Person_Start_Tag;
         Current_Ids : Current_Ids_T;
      end record;

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Argument        : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : String) with
        Global => null,
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Pre_Comment_Utils;

   package body Test_Person_With_Age_Pre_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Argument.Max_Indices.Person_Id_Max <
                   Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                   not Argument.Current_Ids.Person_Ids.Is_Full
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Person_Id (Person_Id);
                     Argument.Current_Ids.Person_Ids.Append (Person_Id);
                  end;
                  Argument.State := Expecting_Pre_Age_Text;
               else
                  Call_Result.Initialize (-0206264299, 0284887201);
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (0156529524, -1682928334);
         end case;
      end Start_Tag;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  Argument.State := Final_State;
               else
                  Call_Result.Initialize (0512431155, -0532051458);
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (-1825682706, 0079776466);
         end case;
      end End_Tag;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Age_Value  =>
               if Argument.Current_Ids.Person_Ids.Is_Empty then
                  Call_Result.Initialize (-1324547445, -1309193195);
               else
                  if Value'Length >
                    Argument.Storage.Person
                      (
                       Argument.Current_Ids.Person_Ids.Last_Element
                      ).Max_Name_Size
                  then
                     Call_Result.Initialize (-0153689476, 0042874247);
                  else
                     Argument.Storage.Person
                       (
                        Argument.Current_Ids.Person_Ids.Last_Element
                       ).Set_Name (Value);
                  end if;
                  Argument.State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Pre_Age_Text =>
               if Value = "" then
                  Argument.State := Expecting_Pre_Age_Comment;
               else
                  Call_Result.Initialize (1236802941, -1747027568);
               end if;
            when Expecting_Pre_Age_Comment |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-0851733196, 0749207954);
         end case;
      end Text;

      procedure Attribute (Argument        : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
      begin
         Call_Result.Initialize (-0398933500, 0131921054);
      end Attribute;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Pre_Age_Comment =>
               if Argument.Storage.Header_Comment'Length > Value'Length then
                  Argument.Storage.Header_Comment
                    (1..Value'Length) := Value (Value'Range);
                  Argument.State := Expecting_Age_Value;
               else
                  Call_Result.Initialize (2095599376, 0300091204);
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (-1990355837, -1187464962);
         end case;
      end Comment;

      procedure Unused_CDATA
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0994849533, 1267702157);
      end Unused_CDATA;

      procedure Run_Test (XML : String) is

         procedure Parse_XML is new Aida.XML_SAX_Parse
           (Argument_Type,
            Start_Tag,
            End_Tag,
            Text,
            Attribute,
            Comment,
            Unused_CDATA);

         Call_Result : Aida.Call_Result;

         Argument : Argument_Type;
      begin
         Argument.Max_Indices.Clear;
         Clear (Argument.Storage);

         Parse_XML (Argument,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);
         Ahven.Assert (Argument.State = Final_State,
                       "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Argument.Current_Ids.Person_Ids.Is_Non_Empty,
                       "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Argument.Max_Indices.Person_Id_Max = 1,
                       "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed,
                       "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         Ahven.Assert
           (Argument.Storage.Person
              (Argument.Max_Indices.Person_Id_Max).Age = 10,
            "1d10d12b-c726-40aa-881b-8374801f539e");

         Ahven.Assert
           (Argument.Storage.Header_Comment (1..9) = " Comment ",
            "1DC369DF-0657-46DE-8E45-711F49789555");
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

      type State_T is
        (
         Expecting_Person_Start_Tag,
         Expecting_Age_Value,
         Expecting_Post_Age_Comment,
         Expecting_Post_Age_Text,
         Expecting_Person_End_Tag,
         Final_State
        );

      type Argument_Type is record
         Storage     : Storage_T;
         Max_Indices : Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
         State       : State_T := Expecting_Person_Start_Tag;
         Current_Ids : Current_Ids_T;
      end record;

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Argument        : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : String) with
        Global => null,
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Post_Comment_Utils;

   package body Test_Person_With_Age_Post_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Argument.Max_Indices.Person_Id_Max <
                   Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                   not Argument.Current_Ids.Person_Ids.Is_Full
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Person_Id (Person_Id);
                     Argument.Current_Ids.Person_Ids.Append (Person_Id);
                  end;
                  Argument.State := Expecting_Age_Value;
               else
                  Call_Result.Initialize (1060981302, -0369276835);
               end if;
            when Expecting_Post_Age_Text |
                 Expecting_Post_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-1264879145, 0351870433);
         end case;
      end Start_Tag;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  Argument.State := Final_State;
               else
                  Call_Result.Initialize (2005019188, -1671506567);
               end if;
            when Expecting_Post_Age_Text |
                 Expecting_Post_Age_Comment |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (0565775576, -0152492210);
         end case;
      end End_Tag;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Age_Value  =>
               if Argument.Current_Ids.Person_Ids.Is_Empty then
                  Call_Result.Initialize (1873536403, 1672121597);
               else
                  if Value'Length >
                    Argument.Storage.Person
                      (
                       Argument.Current_Ids.Person_Ids.Last_Element
                      ).Max_Name_Size
                  then
                     Call_Result.Initialize (0652236873, -0431737624);
                  else
                     Argument.Storage.Person
                       (
                        Argument.Current_Ids.Person_Ids.Last_Element
                       ).Set_Name (Value);
                  end if;
                  Argument.State := Expecting_Post_Age_Comment;
               end if;
            when Expecting_Post_Age_Text =>
               if Value = "" then
                  Argument.State := Expecting_Person_End_Tag;
               else
                  Call_Result.Initialize (1404486087, 1338851999);
               end if;
            when Expecting_Post_Age_Comment |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-0506190056, 2066536020);
         end case;
      end Text;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
      begin
         Call_Result.Initialize (1685075415, 0079136961);
      end Attribute;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Post_Age_Comment =>
               if Argument.Storage.Header_Comment'Length > Value'Length then
                  Argument.Storage.Header_Comment
                    (1..Value'Length) := Value (Value'Range);
                  Argument.State := Expecting_Post_Age_Text;
               else
                  Call_Result.Initialize (-1848128882, -2103490421);
               end if;
            when Expecting_Post_Age_Text |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (-0578317557, -2024566314);
         end case;
      end Comment;

      procedure Unused_CDATA
        (Argument    : in out Argument_Type;
         Value       : String;
         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-1494886622, -1435235821);
      end Unused_CDATA;

      procedure Run_Test (XML : String) is

         procedure Parse_XML is new Aida.XML_SAX_Parse
           (Argument_Type,
            Start_Tag,
            End_Tag,
            Text,
            Attribute,
            Comment,
            Unused_CDATA);

         Call_Result : Aida.Call_Result;

         Argument : Argument_Type;
      begin
         Argument.Max_Indices.Clear;
         Clear (Argument.Storage);

         Parse_XML (Argument,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);
         Ahven.Assert (Argument.State = Final_State,
                       "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Argument.Current_Ids.Person_Ids.Is_Non_Empty,
                       "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Argument.Max_Indices.Person_Id_Max = 1,
                       "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         Ahven.Assert
           (Argument.Storage.Person
              (Argument.Max_Indices.Person_Id_Max).Age = 10,
            "1d10d12b-c726-40aa-881b-8374801f539e");

         Ahven.Assert
           (Argument.Storage.Header_Comment (1..9) = " Comment ",
            "1DC369DF-0657-46DE-8E45-711F49789555");
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

      type State_T is
        (
         Expecting_Person_Start_Tag,
         Expecting_Pre_Age_Text,
         Expecting_Pre_Age_CDATA,
         Expecting_Age_Value,
         Expecting_Person_End_Tag,
         Final_State
        );

      type Argument_Type is record
         Storage     : Storage_T;
         Max_Indices : Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
         State       : State_T := Expecting_Person_Start_Tag;
         Current_Ids : Current_Ids_T;
      end record;

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure CDATA (Argument    : in out Argument_Type;
                       Value       : String;
                       Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML : String) with
        Global => null,
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Age_Pre_CDATA_Comment_Utils;

   package body Test_Person_With_Age_Pre_CDATA_Comment_Utils with SPARK_Mode is

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Argument.Max_Indices.Person_Id_Max <
                   Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                   not Argument.Current_Ids.Person_Ids.Is_Full
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Person_Id (Person_Id);
                     Argument.Current_Ids.Person_Ids.Append (Person_Id);
                  end;
                  Argument.State := Expecting_Pre_Age_Text;
               else
                  Call_Result.Initialize (-1514498303, 1742243882);
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_CDATA |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-1916263923, -1891215598);
         end case;
      end Start_Tag;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  Argument.State := Final_State;
               else
                  Call_Result.Initialize (1757113648, -2065026636);
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Pre_Age_CDATA |
                 Expecting_Age_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (1987615793, 0718894067);
         end case;
      end End_Tag;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Age_Value  =>
               if Argument.Current_Ids.Person_Ids.Is_Empty then
                  Call_Result.Initialize (-1824053881, 2061413792);
               else
                  if Value'Length >
                    Argument.Storage.Person
                      (
                       Argument.Current_Ids.Person_Ids.Last_Element
                      ).Max_Name_Size then
                     Call_Result.Initialize (-1672668363, 0966305970);
                  else
                     Argument.Storage.Person
                       (
                        Argument.Current_Ids.Person_Ids.Last_Element
                       ).Set_Name (Value);
                  end if;
                  Argument.State := Expecting_Person_End_Tag;
               end if;
            when Expecting_Pre_Age_Text =>
               if Value = "" then
                  Argument.State := Expecting_Pre_Age_CDATA;
               else
                  Call_Result.Initialize (-0353024501, 0095044560);
               end if;
            when Expecting_Pre_Age_CDATA |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-0440932756, 1176624933);
         end case;
      end Text;

      procedure Attribute (Argument        : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result)
      is
         pragma Unreferenced (Attribute_Name);
         pragma Unreferenced (Attribute_Value);
         pragma Unreferenced (Argument);
      begin
         Call_Result.Initialize (1506567129, -1557041959);
      end Attribute;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (1733782299, 1310378793);
      end Comment;

      procedure CDATA (Argument    : in out Argument_Type;
                       Value       : String;
                       Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Pre_Age_CDATA =>
               if Argument.Storage.Header_Comment'Length > Value'Length then
                  Argument.Storage.Header_Comment (1..Value'Length) := Value (Value'Range);
                  Argument.State := Expecting_Age_Value;
               else
                  Call_Result.Initialize (0339578717, -0632456392);
               end if;
            when Expecting_Pre_Age_Text |
                 Expecting_Age_Value |
                 Expecting_Person_End_Tag |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (-0580058624, 0796480717);
         end case;
      end CDATA;

      procedure Run_Test (XML : String) is

         procedure Parse_XML is new Aida.XML_SAX_Parse
           (Argument_Type,
            Start_Tag,
            End_Tag,
            Text,
            Attribute,
            Comment,
            CDATA);

         Call_Result : Aida.Call_Result;

         Argument : Argument_Type;
      begin
         Argument.Max_Indices.Clear;
         Clear (Argument.Storage);

         Parse_XML (Argument,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);
         Ahven.Assert (Argument.State = Final_State,
                       "592cbd68-ef97-4fc1-934b-80111d24fd32");
         Ahven.Assert (Argument.Current_Ids.Person_Ids.Is_Non_Empty,
                       "1f861507-695e-458b-836e-aa9fe7f131e2");
         Ahven.Assert (Argument.Max_Indices.Person_Id_Max = 1,
                       "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
         Ahven.Assert (not Call_Result.Has_Failed,
                       "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
         Ahven.Assert
           (Argument.Storage.Person
              (Argument.Max_Indices.Person_Id_Max).Age = 10,
            "1d10d12b-c726-40aa-881b-8374801f539e");

         Ahven.Assert
           (Argument.Storage.Header_Comment (1..19) = " Important comment ",
            "1DC369DF-0657-46DE-8E45-711F49789555");
      end Run_Test;

   end Test_Person_With_Age_Pre_CDATA_Comment_Utils;

   procedure Test_Person_With_Age_Pre_CDATA_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);
   begin
      Test_Person_With_Age_Pre_CDATA_Comment_Utils.Run_Test (XML_Test_Person_With_Age_Pre_CDATA_Comment_0);
   end Test_Person_With_Age_Pre_CDATA_Comment_0;

   package Test_Person_With_Single_Quote_Attribute_Utils with SPARK_Mode is

      type State_T is
        (
         Expecting_Person_Start_Tag,
         Expecting_Attribute,
         Expecting_Empty_Value,
         Expecting_Person_End_Tag,
         Final_State
        );

      type Argument_Type is record
         Storage     : Storage_T;
         Max_Indices : Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;
         State       : State_T := Expecting_Person_Start_Tag;
         Current_Ids : Current_Ids_T;
      end record;

      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure CDATA (Argument    : in out Argument_Type;
                       Value       : String;
                       Call_Result : in out Aida.Call_Result) with
        Global => null,
        Pre    => not Call_Result.Has_Failed;

      procedure Run_Test (XML                : String;
                          Expected_Attribute : String) with
        Global => null,
        Pre    => XML'Length > 0 and XML'Last < Integer'Last - 4;

   end Test_Person_With_Single_Quote_Attribute_Utils;

   package body Test_Person_With_Single_Quote_Attribute_Utils with
      SPARK_Mode => On
   is
      procedure Start_Tag (Argument    : in out Argument_Type;
                           Tag_Name    : String;
                           Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_Start_Tag =>
               if
                 Tag_Name = "person" and
                 Argument.Max_Indices.Person_Id_Max <
                   Json_Parsing_Tests_Model.Extended_Person_Id_T'Last and
                   not Argument.Current_Ids.Person_Ids.Is_Full
               then
                  declare
                     Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T;
                  begin
                     Argument.Max_Indices.Allocate_Person_Id (Person_Id);
                     Argument.Current_Ids.Person_Ids.Append (Person_Id);
                  end;
                  Argument.State := Expecting_Attribute;
               else
                  Call_Result.Initialize (-1778052921, 0742206668);
               end if;
            when Expecting_Attribute |
                 Expecting_Empty_Value |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (-1982292661, 0204153609);
         end case;
      end Start_Tag;

      procedure End_Tag (Argument    : in out Argument_Type;
                         Tag_Name    : String;
                         Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Person_End_Tag =>
               if Tag_Name = "person" then
                  Argument.State := Final_State;
               else
                  Call_Result.Initialize (0573296931, -0661012874);
               end if;
            when Expecting_Attribute |
                 Expecting_Empty_Value |
                 Expecting_Person_Start_Tag |
                 Final_State =>
               Call_Result.Initialize (-1434939725, -1569639445);
         end case;
      end End_Tag;

      procedure Text (Argument    : in out Argument_Type;
                      Value       : String;
                      Call_Result : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Empty_Value =>
               if Argument.Current_Ids.Person_Ids.Is_Empty then
                  Call_Result.Initialize (-1166822625, 0624736144);
               else
                  if Value'Length = 0 then
                     Argument.State := Expecting_Person_End_Tag;
                  else
                     Call_Result.Initialize (-1739311526, -1879999007);
                  end if;
               end if;
            when Expecting_Attribute |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (0165541118, 1487181886);
         end case;
      end Text;

      procedure Attribute (Argument    : in out Argument_Type;
                           Attribute_Name  : String;
                           Attribute_Value : String;
                           Call_Result     : in out Aida.Call_Result) is
      begin
         case Argument.State is
            when Expecting_Attribute =>
               if Argument.Current_Ids.Person_Ids.Is_Empty then
                  Call_Result.Initialize (-0236136172, -0180683845);
               elsif Attribute_Name /= "name" then
                  Call_Result.Initialize (-1993277347, -1612320261);
               else
                  if Attribute_Value'Length >
                    Argument.Storage.Person
                      (
                       Argument.Current_Ids.Person_Ids.Last_Element
                      ).Max_Name_Size then
                     Call_Result.Initialize (-0691993277, -0832249817);
                  else
                     Argument.Storage.Person
                       (
                        Argument.Current_Ids.Person_Ids.Last_Element
                       ).Set_Name (Attribute_Value);
                  end if;
                  Argument.State := Expecting_Empty_Value;
               end if;
            when Expecting_Empty_Value |
                 Expecting_Person_Start_Tag |
                 Expecting_Person_End_Tag |
                 Final_State =>
               Call_Result.Initialize (2027137067, 0850453105);
         end case;
      end Attribute;

      procedure Comment (Argument    : in out Argument_Type;
                         Value       : String;
                         Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (-0915178087, -1332045495);
      end Comment;

      procedure CDATA (Argument    : in out Argument_Type;
                       Value       : String;
                       Call_Result : in out Aida.Call_Result)
      is
         pragma Unreferenced (Argument);
         pragma Unreferenced (Value);
      begin
         Call_Result.Initialize (1245600917, -0131738197);
      end CDATA;

      procedure Run_Test (XML                : String;
                          Expected_Attribute : String) is

         procedure Parse_XML is new Aida.XML_SAX_Parse
           (Argument_Type,
            Start_Tag,
            End_Tag,
            Text,
            Attribute,
            Comment,
            CDATA);

         Call_Result : Aida.Call_Result;

         Argument : Argument_Type;
      begin
         Argument.Max_Indices.Clear;
         Argument.Storage.Clear;

         Parse_XML (Argument,
                    XML,
                    Call_Result);

         Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);
         Ahven.Assert (Argument.State = Final_State, "1");
         Ahven.Assert (Argument.Current_Ids.Person_Ids.Is_Non_Empty, "2");
         Ahven.Assert (Argument.Max_Indices.Person_Id_Max = 1, "3");
         Ahven.Assert
           (Argument.Storage.Person
              (
               Argument.Max_Indices.Person_Id_Max
              ).Name = Expected_Attribute, "4",
            Argument.Storage.Person (Argument.Max_Indices.Person_Id_Max).Name);
      end Run_Test;

   end Test_Person_With_Single_Quote_Attribute_Utils;

   procedure Test_Person_With_Single_Quote_Attribute_0 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma SPARK_Mode;
      pragma Unreferenced (T);
   begin
      Test_Person_With_Single_Quote_Attribute_Utils.Run_Test (XML_Test_Person_With_Single_Quote_Attribute_0, """joe"" or ""john""");
   end Test_Person_With_Single_Quote_Attribute_0;

   procedure Test_Person_With_Double_Quote_Attribute_0 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma SPARK_Mode;
      pragma Unreferenced (T);
   begin
      Test_Person_With_Single_Quote_Attribute_Utils.Run_Test (XML_Test_Person_With_Double_Quote_Attribute_0, "'joe' or 'john'");
   end Test_Person_With_Double_Quote_Attribute_0;

end Aida.XML_SAX_Parse_Tests;
