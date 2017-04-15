with GNAT.Source_Info;

package body Aida.Types.Tests is

   use all type Aida.Types.String_T;

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
   end Initialize;

   procedure SHOULD_Successfully_Convert_String_0_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.Types.String_T := "0";
      Target     : Aida.Types.Int32_T;
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

      Text       : constant Aida.Types.String_T := "100";
      Target     : Aida.Types.Int32_T;
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

      Text       : constant Aida.Types.String_T := "2147483647";
      Target     : Aida.Types.Int32_T;
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

      Text       : constant Aida.Types.String_T := "2147483648";
      Target     : Aida.Types.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
   end SHOULD_Successfully_Convert_String_2147483648_To_Integer;

   procedure SHOULD_Successfully_Convert_String_Minus_1_To_Integer (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Text       : constant Aida.Types.String_T := "-1";
      Target     : Aida.Types.Int32_T;
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

      Text       : constant Aida.Types.String_T := "-100";
      Target     : Aida.Types.Int32_T;
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

      Text       : constant Aida.Types.String_T := "-2147483648";
      Target     : Aida.Types.Int32_T;
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

      Text       : constant Aida.Types.String_T := "-2147483649";
      Target     : Aida.Types.Int32_T;
      Has_Failed : Boolean;
   begin
      To_Int32 (Source     => Text,
                Target     => Target,
                Has_Failed => Has_Failed);
      Ahven.Assert (Has_Failed, GNAT.Source_Info.Source_Location & ", was " & Has_Failed'Img);
   end SHOULD_Fail_To_Convert_String_Minus_2147483649_To_Integer;

end Aida.Types.Tests;
