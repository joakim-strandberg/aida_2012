with Aida.XML_DOM_Parser;
with Aida.Subprogram_Call_Result;

package body Aida.XML_DOM_Parser_Tests is

   XML_Test_Person_With_Vehicles_0 : constant Aida.String_T :=
     "<?xml version=""1.0"" encoding=""UTF-8""?><person><!-- Comment --><![CDATA[ Some info ]]><vehicles><vehicle wheels='4'/><vehicle wheels='2'/></vehicles></person>";

   overriding procedure Initialize (T : in out Test) is
      pragma SPARK_Mode (Off);
   begin
      Set_Name (T, "Aida.XML_DOM_Parser package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
   end Initialize;

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package DOM_Parser is new Aida.XML_DOM_Parser (Max_Chars      => 1000,
                                                     Max_Strings    => 50,
                                                     Max_Nodes      => 20,
                                                     Max_Attributes => 10);

      use type DOM_Parser.Node_Kind_Id_T;

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Parser.Parse (XML_Test_Person_With_Vehicles_0, Call_Result);

      Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));
      Ahven.Assert (Parser.Nodes (1).Id = DOM_Parser.XML_Tag, "1");
--        Ahven.Assert (Current_Ids.Person_Ids.Is_Non_Empty, "1f861507-695e-458b-836e-aa9fe7f131e2");
--        Ahven.Assert (Max_Indices.Person_Id_Max = 1, "949ca5e3-1353-47e6-90fc-b0aa21d398a6");
--        Ahven.Assert (not Call_Result.Has_Failed, "4ed49d34-b03a-4251-ab05-dc9cb794bd91");
--        Ahven.Assert (Storage.Person (Max_Indices.Person_Id_Max).Age = 10, "1d10d12b-c726-40aa-881b-8374801f539e");
   end Test_Person_With_Age_0;

end Aida.XML_DOM_Parser_Tests;
