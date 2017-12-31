with GNAT.Source_Info;

package body Aida.Tests is

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Conversion package tests");

      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_String_0_To_Integer'Access, "SHOULD_Successfully_Convert_String_0_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_String_100_To_Integer'Access, "SHOULD_Successfully_Convert_String_100_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_String_2147483647_To_Integer'Access, "SHOULD_Successfully_Convert_String_2147483647_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_String_2147483648_To_Integer'Access, "SHOULD_Successfully_Convert_String_2147483648_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_String_Minus_1_To_Integer'Access, "SHOULD_Successfully_Convert_String_Minus_1_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_String_Minus_100_To_Integer'Access, "SHOULD_Successfully_Convert_String_Minus_100_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_String_Minus_2147483648_To_Integer'Access, "SHOULD_Successfully_Convert_String_Minus_2147483648_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Fail_To_Convert_String_Minus_2147483649_To_Integer'Access, "SHOULD_Fail_To_Convert_String_Minus_2147483649_To_Integer");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_Int32_0_To_String'Access, "SHOULD_Successfully_Convert_Int32_0_To_String");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_Int32_7_To_String'Access, "SHOULD_Successfully_Convert_Int32_7_To_String");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_Int32_100_To_String'Access, "SHOULD_Successfully_Convert_Int32_100_To_String");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_Int32_2147483647_To_String'Access, "SHOULD_Successfully_Convert_Int32_2147483647_To_String");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_Int32_Minus_1_To_String'Access, "SHOULD_Successfully_Convert_Int32_Minus_1_To_String");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_Int32_Minus_100_To_String'Access, "SHOULD_Successfully_Convert_Int32_Minus_100_To_String");
      Ahven.Framework.Add_Test_Routine (T, SHOULD_Successfully_Convert_Int32_Minus_2147483647_To_String'Access, "SHOULD_Successfully_Convert_Int32_Minus_2147483647_To_String");
   end Initialize;

   procedure SHOULD_Successfully_Convert_Int32_Minus_2147483647_To_String (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      I : constant Aida.Int32_T := -2147483647;

      Result : constant Aida.String_T := To_String (I);
   begin
      Ahven.Assert (Result = "-2147483647", "");
   end SHOULD_Successfully_Convert_Int32_Minus_2147483647_To_String;

   procedure SHOULD_Successfully_Convert_Int32_Minus_100_To_String (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      I : constant Aida.Int32_T := -100;

      Result : constant Aida.String_T := To_String (I);
   begin
      Ahven.Assert (Result = "-100", "");
   end SHOULD_Successfully_Convert_Int32_Minus_100_To_String;

   procedure SHOULD_Successfully_Convert_Int32_Minus_1_To_String (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      I : constant Aida.Int32_T := -1;

      Result : constant Aida.String_T := To_String (I);
   begin
      Ahven.Assert (Result = "-1", "");
   end SHOULD_Successfully_Convert_Int32_Minus_1_To_String;

   procedure SHOULD_Successfully_Convert_Int32_2147483647_To_String (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      I : constant Aida.Int32_T := 2147483647;

      Result : constant Aida.String_T := To_String (I);
   begin
      Ahven.Assert (Result = "2147483647", "");
   end SHOULD_Successfully_Convert_Int32_2147483647_To_String;

   procedure SHOULD_Successfully_Convert_Int32_100_To_String (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      I : constant Aida.Int32_T := 100;

      Result : constant Aida.String_T := To_String (I);
   begin
      Ahven.Assert (Result = "100", "");
   end SHOULD_Successfully_Convert_Int32_100_To_String;

   procedure SHOULD_Successfully_Convert_Int32_7_To_String (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      I : constant Aida.Int32_T := 7;

      Result : constant Aida.String_T := To_String (I);
   begin
      Ahven.Assert (Result = "7", "");
   end SHOULD_Successfully_Convert_Int32_7_To_String;

   procedure SHOULD_Successfully_Convert_Int32_0_To_String (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      I : constant Aida.Int32_T := 0;

      Result : constant Aida.String_T := To_String (I);
   begin
      Ahven.Assert (Result = "0", "55cb46b2-8b59-4f5a-8e64-548b9ce67fc1");
   end SHOULD_Successfully_Convert_Int32_0_To_String;

   procedure SHOULD_Successfully_Convert_String_0_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "0";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (not Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
      Ahven.Assert (Target = 0, GNAT.Source_Info.Source_Location & ", was " & Target'Img);
   end SHOULD_Successfully_Convert_String_0_To_Integer;

   procedure SHOULD_Successfully_Convert_String_100_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "100";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (not Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
      Ahven.Assert (Target = 100, GNAT.Source_Info.Source_Location & ", was " & Target'Img);
   end SHOULD_Successfully_Convert_String_100_To_Integer;

   procedure SHOULD_Successfully_Convert_String_2147483647_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "2147483647";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (not Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
      Ahven.Assert (Target = 2147483647, GNAT.Source_Info.Source_Location & ", was " & Target'Img);
   end SHOULD_Successfully_Convert_String_2147483647_To_Integer;

   procedure SHOULD_Successfully_Convert_String_2147483648_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "2147483648";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
   end SHOULD_Successfully_Convert_String_2147483648_To_Integer;

   procedure SHOULD_Successfully_Convert_String_Minus_1_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "-1";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (not Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
      Ahven.Assert (Target = -1, GNAT.Source_Info.Source_Location & ", was " & Target'Img);
   end SHOULD_Successfully_Convert_String_Minus_1_To_Integer;

   procedure SHOULD_Successfully_Convert_String_Minus_100_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "-100";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (not Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
      Ahven.Assert (Target = -100, GNAT.Source_Info.Source_Location & ", was " & Target'Img);
   end SHOULD_Successfully_Convert_String_Minus_100_To_Integer;

   procedure SHOULD_Successfully_Convert_String_Minus_2147483648_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "-2147483648";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (not Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
      Ahven.Assert (Target = -2147483648, GNAT.Source_Info.Source_Location & ", was " & Target'Img);
   end SHOULD_Successfully_Convert_String_Minus_2147483648_To_Integer;

   procedure SHOULD_Fail_To_Convert_String_Minus_2147483649_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.String_T := "-2147483649";
      Target     : Aida.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
   end SHOULD_Fail_To_Convert_String_Minus_2147483649_To_Integer;

end Aida.Tests;
