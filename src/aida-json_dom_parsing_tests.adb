with Aida.JSON.Generic_DOM_Parser;
with Aida.Subprogram_Call_Result;

package body Aida.JSON_DOM_Parsing_Tests is

   -- The trailing numbers are to differentiate between the same json except different number of spaces
   JSON_Test_Person_With_Age_0            : constant Aida.String_T := "{""age"" : 10}";
--     JSON_Test_Person_With_Age_1            : constant Aida.String_T := "{""age"" : 10 }";
--     JSON_Test_Person_With_Hand_0           : constant Aida.String_T := "{""hand"" : { ""fingers"" : 4 }}";
--     JSON_Test_Person_With_Name_Adam_0      : constant Aida.String_T := "{""name"" : ""adam""}";
--     JSON_Test_Person_With_Name_Adam_1      : constant Aida.String_T := "   {""name"" : ""adam""}";
--     JSON_Test_Person_With_Name_And_Age_0   : constant Aida.String_T := "{""name"" : ""bertil"", ""age"" : 5}";
--     JSON_Test_Person_With_Vehicles_0       : constant Aida.String_T := "{""vehicles"" : [ {""wheels"" : 4 }, {""wheels"" : 2 } ]}";
--     JSON_Test_Person_With_Length_0         : constant Aida.String_T := "{""length"" : 1.98}";
--     JSON_Test_Person_With_Is_Happy_True_0  : constant Aida.String_T := "{""isHappy"" : true}";
--     JSON_Test_Person_With_Is_Happy_False_0 : constant Aida.String_T := "{""isHappy"" : false}";
--     JSON_Test_Person_With_Is_Happy_Null_0  : constant Aida.String_T := "{""isHappy"" : null}";

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON.Generic_DOM_Parser package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_1'Access, "Test_Person_With_Age_1");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_0'Access, "Test_Person_With_Name_Adam_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_1'Access, "Test_Person_With_Name_Adam_1");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Length_0'Access, "Test_Person_With_Length_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_0'Access, "Test_Person_With_Hand_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Vehicles_0'Access, "Test_Person_With_Vehicles_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_And_Age_0'Access, "Test_Person_With_Name_And_Age_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_True_0'Access, "Test_Person_With_Is_Happy_True_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_False_0'Access, "Test_Person_With_Is_Happy_False_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_Null_0'Access, "Test_Person_With_Is_Happy_Null_0");
   end Initialize;

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package DOM_Parser is new Aida.JSON.Generic_DOM_Parser (Max_Chars   => 100,
                                                              Max_Strings => 2,
                                                              Max_Nodes   => 100);

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;

   begin
      Parser.Parse (JSON_Message => JSON_Test_Person_With_Age_0,
                    Call_Result  => Call_Result);

      Ahven.Assert (Parser.Nodes (DOM_Parser.Node_Index_T'First).JSON_Key);
      --      Test_Person_With_Name_Adam_Utils.Run_Test (JSON_Test_Person_With_Name_Adam_0);
      null;
   end Test_Person_With_Age_0;

end Aida.JSON_DOM_Parsing_Tests;
