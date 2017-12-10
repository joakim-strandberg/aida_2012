with Aida.JSON.Generic_DOM_Parser;
with Aida.Subprogram_Call_Result;

package body Aida.JSON_DOM_Parsing_Tests is

   -- The trailing numbers are to differentiate between the same json except different number of spaces
   JSON_Test_Person_With_Age_0            : constant Aida.String_T := "{""age"" : 10}";
   JSON_Test_Person_With_Hand_0           : constant Aida.String_T := "{""hand"" : { ""fingers"" : 4 }}";
   JSON_Test_Person_With_Name_Adam_0      : constant Aida.String_T := "{""name"" : ""adam""}";
   JSON_Test_Person_With_Name_And_Age_0   : constant Aida.String_T := "{""name"" : ""bertil"", ""age"" : 5}";
   JSON_Test_Person_With_Vehicles_0       : constant Aida.String_T := "{""vehicles"" : [ {""wheels"" : 4 }, {""wheels"" : 2 } ]}";
--     JSON_Test_Person_With_Length_0         : constant Aida.String_T := "{""length"" : 1.98}";
--     JSON_Test_Person_With_Is_Happy_True_0  : constant Aida.String_T := "{""isHappy"" : true}";
--     JSON_Test_Person_With_Is_Happy_False_0 : constant Aida.String_T := "{""isHappy"" : false}";
--     JSON_Test_Person_With_Is_Happy_Null_0  : constant Aida.String_T := "{""isHappy"" : null}";

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON.Generic_DOM_Parser package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Age_0'Access, "Test_Person_With_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Hand_0'Access, "Test_Person_With_Hand_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_Adam_0'Access, "Test_Person_With_Name_Adam_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Name_And_Age_0'Access, "Test_Person_With_Name_And_Age_0");
      Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Vehicles_0'Access, "Test_Person_With_Vehicles_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Length_0'Access, "Test_Person_With_Length_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_True_0'Access, "Test_Person_With_Is_Happy_True_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_False_0'Access, "Test_Person_With_Is_Happy_False_0");
--        Ahven.Framework.Add_Test_Routine (T, Test_Person_With_Is_Happy_Null_0'Access, "Test_Person_With_Is_Happy_Null_0");
   end Initialize;

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package DOM_Parser is new Aida.JSON.Generic_DOM_Parser (Max_Chars        => 100,
                                                              Max_Strings      => 2,
                                                              Max_Nodes        => 10,
                                                              Max_Array_Values => 1);

      use all type DOM_Parser.JSON_Value_Id_T;

      F : constant DOM_Parser.Node_Index_T := DOM_Parser.Node_Index_T'First;

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Parser.Parse (JSON_Message => JSON_Test_Person_With_Age_0,
                    Call_Result  => Call_Result);

      Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Key) = "age", "was ", Parser.Map.Value (Parser.Nodes (F).JSON_Key));
      Ahven.Assert (Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Integer, "was ");--, Parser.Nodes (DOM_Parser.Node_Index_T'First).JSON_Value.Id'Image);
      if Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Integer then
         Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Value.Key) = "10", "was ", Parser.Map.Value (Parser.Nodes (F).JSON_Value.Key));
      end if;
   end Test_Person_With_Age_0;

   procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package DOM_Parser is new Aida.JSON.Generic_DOM_Parser (Max_Chars        => 100,
                                                              Max_Strings      => 10,
                                                              Max_Nodes        => 10,
                                                              Max_Array_Values => 1);

      use all type DOM_Parser.JSON_Value_Id_T;
      use all type DOM_Parser.Node_Index_T;

      F : constant DOM_Parser.Node_Index_T := DOM_Parser.Node_Index_T'First;

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Parser.Parse (JSON_Message => JSON_Test_Person_With_Hand_0,
                    Call_Result  => Call_Result);

      Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Key) = "hand", "1 was ", Parser.Map.Value (Parser.Nodes (F).JSON_Key));
      Ahven.Assert (Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Object, "2 was ");--, Parser.Nodes (DOM_Parser.Node_Index_T'First).JSON_Value.Id'Image);
      if Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Object then
         Ahven.Assert (Parser.Nodes (F).JSON_Value.Node_Id = F + 1, "3 was ");
      end if;
      Ahven.Assert (not Parser.Nodes (F).Has_Next_Node, "7 was ");

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 1).JSON_Key) = "fingers", "4 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Key));
      Ahven.Assert (Parser.Nodes (F + 1).JSON_Value.Id = DOM_Parser.JSON_Integer, "5 was ");
      if Parser.Nodes (F + 1).JSON_Value.Id = DOM_Parser.JSON_Integer then
         Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 1).JSON_Value.Key) = "4", "6 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Value.Key));
      end if;
      Ahven.Assert (not Parser.Nodes (F + 1).Has_Next_Node, "8 was ");
   end Test_Person_With_Hand_0;

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package DOM_Parser is new Aida.JSON.Generic_DOM_Parser (Max_Chars        => 100,
                                                              Max_Strings      => 2,
                                                              Max_Nodes        => 10,
                                                              Max_Array_Values => 1);

      use all type DOM_Parser.JSON_Value_Id_T;

      F : constant DOM_Parser.Node_Index_T := DOM_Parser.Node_Index_T'First;

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Parser.Parse (JSON_Message => JSON_Test_Person_With_Name_Adam_0,
                    Call_Result  => Call_Result);

      Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Key) = "name", "was ", Parser.Map.Value (Parser.Nodes (F).JSON_Key));
      Ahven.Assert (Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Text, "was ");--, Parser.Nodes (DOM_Parser.Node_Index_T'First).JSON_Value.Id'Image);
      if Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Text then
         Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Value.Key) = "adam", "was ", Parser.Map.Value (Parser.Nodes (F).JSON_Value.Key));
      end if;
   end Test_Person_With_Name_Adam_0;

   procedure Test_Person_With_Name_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package DOM_Parser is new Aida.JSON.Generic_DOM_Parser (Max_Chars        => 100,
                                                              Max_Strings      => 10,
                                                              Max_Nodes        => 10,
                                                              Max_Array_Values => 10);

      use all type DOM_Parser.JSON_Value_Id_T;
      use all type DOM_Parser.Node_Index_T;

      F : constant DOM_Parser.Node_Index_T := DOM_Parser.Node_Index_T'First;

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Parser.Parse (JSON_Message => JSON_Test_Person_With_Name_And_Age_0,
                    Call_Result  => Call_Result);

      Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Key) = "name", "1 was ", Parser.Map.Value (Parser.Nodes (F).JSON_Key));
      Ahven.Assert (Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Text, "2 was ");--, Parser.Nodes (DOM_Parser.Node_Index_T'First).JSON_Value.Id'Image);
      if Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Text then
         Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Value.Key) = "bertil", "3 was ", Parser.Map.Value (Parser.Nodes (F).JSON_Value.Key));
      end if;
      Ahven.Assert (Parser.Nodes (F).Has_Next_Node, "4 was ", Boolean'Image (Parser.Nodes (F).Has_Next_Node));
      Ahven.Assert (Parser.Nodes (F).Next_Node = F + 1, "5 was ", DOM_Parser.Node_Index_T'Image (Parser.Nodes (F).Next_Node));

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 1).JSON_Key) = "age", "6 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Key));
      Ahven.Assert (Parser.Nodes (F + 1).JSON_Value.Id = DOM_Parser.JSON_Integer, "7 was ");--, Parser.Nodes (DOM_Parser.Node_Index_T'First).JSON_Value.Id'Image);
      if Parser.Nodes (F + 1).JSON_Value.Id = DOM_Parser.JSON_Integer then
         Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 1).JSON_Value.Key) = "5", "8 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Value.Key));
      end if;
      Ahven.Assert (not Parser.Nodes (F + 1).Has_Next_Node, "9 was ", Boolean'Image (Parser.Nodes (F + 1).Has_Next_Node));
   end Test_Person_With_Name_And_Age_0;

   procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package DOM_Parser is new Aida.JSON.Generic_DOM_Parser (Max_Chars        => 100,
                                                              Max_Strings      => 10,
                                                              Max_Nodes        => 10,
                                                              Max_Array_Values => 10);

      use all type DOM_Parser.JSON_Value_Id_T;
      use all type DOM_Parser.Array_Index_T;

      F  : constant DOM_Parser.Node_Index_T := DOM_Parser.Node_Index_T'First;
      AF : constant DOM_Parser.Array_Index_T := DOM_Parser.Array_Index_T'First;

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Parser.Parse (JSON_Message => JSON_Test_Person_With_Vehicles_0,
                    Call_Result  => Call_Result);

      Ahven.Assert (not Call_Result.Has_Failed, String (Call_Result.Message));

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Key) = "vehicles", "was ", Parser.Map.Value (Parser.Nodes (F).JSON_Key));
      Ahven.Assert (Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Array, "was ");--, Parser.Nodes (DOM_Parser.Node_Index_T'First).JSON_Value.Id'Image);
      if Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Array then
         Ahven.Assert (Parser.Nodes (F).JSON_Value.Array_Id = AF, "was ", DOM_Parser.Array_Index_T'Image (Parser.Nodes (F).JSON_Value.Array_Id));
      end if;
   end Test_Person_With_Vehicles_0;

end Aida.JSON_DOM_Parsing_Tests;
