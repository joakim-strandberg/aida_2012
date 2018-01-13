with Aida.JSON_DOM_Parser;
with Aida.Subprogram_Call_Result;

package body Aida.JSON_DOM_Parser_Tests is

   -- The trailing numbers are to differentiate between the same json except different number of spaces
   JSON_Test_Person_With_Age_0            : constant Standard.String := "{""age"" : 10}";
   JSON_Test_Person_With_Hand_0           : constant Standard.String := "{""hand"" : { ""fingers"" : 4 }}";
   JSON_Test_Person_With_Name_Adam_0      : constant Standard.String := "{""name"" : ""adam""}";
   JSON_Test_Person_With_Name_And_Age_0   : constant Standard.String := "{""name"" : ""bertil"", ""age"" : 5}";
   JSON_Test_Person_With_Vehicles_0       : constant Standard.String := "{""vehicles"" : [ {""wheels"" : 4 }, {""wheels"" : 2 } ]}";
   --     JSON_Test_Person_With_Length_0         : constant Standard.String := "{""length"" : 1.98}";
   --     JSON_Test_Person_With_Is_Happy_True_0  : constant Standard.String := "{""isHappy"" : true}";
   --     JSON_Test_Person_With_Is_Happy_False_0 : constant Standard.String := "{""isHappy"" : false}";
   --     JSON_Test_Person_With_Is_Happy_Null_0  : constant Standard.String := "{""isHappy"" : null}";

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.JSON_DOM_Parser package tests");

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

      package DOM_Parser is new Aida.JSON_DOM_Parser (Max_Chars        => 100,
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

      Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);

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

      package DOM_Parser is new Aida.JSON_DOM_Parser (Max_Chars        => 100,
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

      Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);

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

      package DOM_Parser is new Aida.JSON_DOM_Parser (Max_Chars        => 100,
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

      Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);

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

      package DOM_Parser is new Aida.JSON_DOM_Parser (Max_Chars        => 100,
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

      Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);

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

      package DOM_Parser is new Aida.JSON_DOM_Parser (Max_Chars        => 100,
                                                      Max_Strings      => 10,
                                                      Max_Nodes        => 10,
                                                      Max_Array_Values => 10);

      use all type DOM_Parser.JSON_Value_Id_T;
      use all type DOM_Parser.Array_Index_T;
      use all type DOM_Parser.Node_Index_T;

      F  : constant DOM_Parser.Node_Index_T := DOM_Parser.Node_Index_T'First;
      AF : constant DOM_Parser.Array_Index_T := DOM_Parser.Array_Index_T'First;

      Parser : DOM_Parser.T;

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Parser.Parse (JSON_Message => JSON_Test_Person_With_Vehicles_0,
                    Call_Result  => Call_Result);

      Ahven.Assert (not Call_Result.Has_Failed, Call_Result.Message);

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F).JSON_Key)     = "vehicles",            "1 was ", Parser.Map.Value (Parser.Nodes (F).JSON_Key));
      Ahven.Assert (                  Parser.Nodes (F).JSON_Value.Id = DOM_Parser.JSON_Array, "2 was ", DOM_Parser.JSON_Value_Id_T'Image (Parser.Nodes (F).JSON_Value.Id));
      Ahven.Assert (not Parser.Nodes (F).Has_Next_Node, "3 was ");
      Ahven.Assert (Parser.Nodes (F).JSON_Value.Array_Id = AF, "4 was ", DOM_Parser.Array_Index_T'Image (Parser.Nodes (F).JSON_Value.Array_Id));

      Ahven.Assert (Parser.Arrays (AF).JSON_Value.Id      = DOM_Parser.JSON_Object, "5 was ",  DOM_Parser.JSON_Value_Id_T'Image (Parser.Arrays (AF).JSON_Value.Id));
      Ahven.Assert (Parser.Arrays (AF).JSON_Value.Node_Id = F + 1,                  "6 was ",  DOM_Parser.Node_Index_T'Image (Parser.Arrays (AF).JSON_Value.Node_Id));
      Ahven.Assert (Parser.Arrays (AF).Has_Next,                                    "15 was ", DOM_Parser.Node_Index_T'Image (Parser.Arrays (AF).JSON_Value.Node_Id));
      Ahven.Assert (Parser.Arrays (AF).Next = AF + 1,                               "16 was ", DOM_Parser.Node_Index_T'Image (Parser.Arrays (AF).JSON_Value.Node_Id));

      Ahven.Assert (    Parser.Arrays (AF + 1).JSON_Value.Id      = DOM_Parser.JSON_Object, "17 was ", DOM_Parser.JSON_Value_Id_T'Image (Parser.Arrays (AF).JSON_Value.Id));
      Ahven.Assert (    Parser.Arrays (AF + 1).JSON_Value.Node_Id = F + 2,                  "18 was ", DOM_Parser.Node_Index_T'Image (Parser.Arrays (AF).JSON_Value.Node_Id));
      Ahven.Assert (not Parser.Arrays (AF + 1).Has_Next,                                    "19 was ", DOM_Parser.Node_Index_T'Image (Parser.Arrays (AF).JSON_Value.Node_Id));

      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 1).JSON_Key)       = "wheels"               , "7 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Key));
      Ahven.Assert (                  Parser.Nodes (F + 1).JSON_Value.Id   = DOM_Parser.JSON_Integer, "8 was ", DOM_Parser.JSON_Value_Id_T'Image (Parser.Nodes (F + 1).JSON_Value.Id));
      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 1).JSON_Value.Key) = "4"                    , "9 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Value.Key));
      Ahven.Assert (              not Parser.Nodes (F + 1).Has_Next_Node,                             "10 was ");
      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 2).JSON_Key)       = "wheels",                "11 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Key));
      Ahven.Assert (                  Parser.Nodes (F + 2).JSON_Value.Id   = DOM_Parser.JSON_Integer, "12 was ", DOM_Parser.JSON_Value_Id_T'Image (Parser.Nodes (F + 1).JSON_Value.Id));
      Ahven.Assert (Parser.Map.Value (Parser.Nodes (F + 2).JSON_Value.Key) = "2",                     "13 was ", Parser.Map.Value (Parser.Nodes (F + 1).JSON_Value.Key));
      Ahven.Assert (              not Parser.Nodes (F + 2).Has_Next_Node,                             "14 was ");
   end Test_Person_With_Vehicles_0;

end Aida.JSON_DOM_Parser_Tests;
