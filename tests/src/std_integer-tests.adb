with GNAT.Source_Info;

package body Std_Integer.Tests is

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Std_Integer package tests");

--        Ahven.Framework.Add_Test_Routine (T, To_String_Test_1'Access, "To_String_Test_1");
--        Ahven.Framework.Add_Test_Routine (T, To_String_Test_10'Access, "To_String_Test_10");
--        Ahven.Framework.Add_Test_Routine (T, To_String_Test_2_147_483_647'Access, "To_String_Test_2_147_483_647");
--        Ahven.Framework.Add_Test_Routine (T, To_String_Test_Minus_1'Access, "To_String_Test_Minus_1");
--        Ahven.Framework.Add_Test_Routine (T, To_String_Test_Minus_198'Access, "To_String_Test_Minus_198");
--        Ahven.Framework.Add_Test_Routine (T, To_String_Test_Minus_2_147_483_648'Access, "To_String_Test_Minus_2_147_483_648");
      Ahven.Framework.Add_Test_Routine (T, Convert_Base_10_To_Integer_Test_32'Access, "Convert_Base_10_To_Integer_Test_32");
      Ahven.Framework.Add_Test_Routine (T, Convert_Base_10_To_Integer_Test_Minus_32'Access, "Convert_Base_10_To_Integer_Test_Minus_32");
      Ahven.Framework.Add_Test_Routine (T, Convert_Base_10_To_Integer_Test_Minus_2_147_483_648'Access, "Convert_Base_10_To_Integer_Test_Minus_2_147_483_648");
   end Initialize;

--     procedure To_String_Test_1 (T : in out Ahven.Framework.Test_Case'Class) is
--        pragma Unreferenced (T);
--
--        Result : constant String := Std_Integer.To_String (1);
--     begin
--        Ahven.Assert (Result = "1", GNAT.Source_Info.Source_Location & ", was " & Result);
--     end To_String_Test_1;
--
--     procedure To_String_Test_10 (T : in out Ahven.Framework.Test_Case'Class) is
--        pragma Unreferenced (T);
--
--        Result : constant String := Std_Integer.To_String (10);
--     begin
--        Ahven.Assert (Result = "10", GNAT.Source_Info.Source_Location & ", was " & Result);
--     end To_String_Test_10;
--
--     procedure To_String_Test_2_147_483_647 (T : in out Ahven.Framework.Test_Case'Class) is
--        pragma Unreferenced (T);
--
--        Result : constant String := Std_Integer.To_String (2_147_483_647);
--     begin
--        Ahven.Assert (Result = "2147483647", GNAT.Source_Info.Source_Location & ", was " & Result);
--     end To_String_Test_2_147_483_647;
--
--     procedure To_String_Test_Minus_1 (T : in out Ahven.Framework.Test_Case'Class) is
--        pragma Unreferenced (T);
--        Result : constant String := Std_Integer.To_String (-1);
--     begin
--        Ahven.Assert (Result = "-1", GNAT.Source_Info.Source_Location & ", was " & Result);
--     end To_String_Test_Minus_1;
--
--     procedure To_String_Test_Minus_198 (T : in out Ahven.Framework.Test_Case'Class) is
--        pragma Unreferenced (T);
--        Result : constant String := Std_Integer.To_String (-198);
--     begin
--        Ahven.Assert (Result = "-198", GNAT.Source_Info.Source_Location & ", was " & Result);
--     end To_String_Test_Minus_198;
--
--     procedure To_String_Test_Minus_2_147_483_648 (T : in out Ahven.Framework.Test_Case'Class) is
--        pragma Unreferenced (T);
--        Result : constant String := Std_Integer.To_String (-2_147_483_648);
--     begin
--        Ahven.Assert (Result = "-2147483648", GNAT.Source_Info.Source_Location & ", was " & Result);
--     end To_String_Test_Minus_2_147_483_648;

   procedure Convert_Base_10_To_Integer_Test_32 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Base_10 : Base_10_Integer_Type;
   begin
      Base_10.Is_Negative := False;
      Base_10_Digits.Append (Base_10.Integer_Digits, 2);
      Base_10_Digits.Append (Base_10.Integer_Digits, 3);

      declare
         I : constant Integer := Convert (Base_10);
      begin
         Ahven.Assert (I = 32, GNAT.Source_Info.Source_Location & ", was " & I'Img);
      end;
   end Convert_Base_10_To_Integer_Test_32;

   procedure Convert_Base_10_To_Integer_Test_Minus_32 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Base_10 : Base_10_Integer_Type;
   begin
      Base_10.Is_Negative := True;
      Base_10_Digits.Append (Base_10.Integer_Digits, 2);
      Base_10_Digits.Append (Base_10.Integer_Digits, 3);

      declare
         I : constant Integer := Convert (Base_10);
      begin
         Ahven.Assert (I = -32, GNAT.Source_Info.Source_Location & ", was " & I'Img);
      end;
   end Convert_Base_10_To_Integer_Test_Minus_32;

   procedure Convert_Base_10_To_Integer_Test_Minus_2_147_483_648 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Base_10 : Base_10_Integer_Type;
   begin
      Base_10.Is_Negative := True;
      Base_10_Digits.Append (Base_10.Integer_Digits, 8);
      Base_10_Digits.Append (Base_10.Integer_Digits, 4);
      Base_10_Digits.Append (Base_10.Integer_Digits, 6);
      Base_10_Digits.Append (Base_10.Integer_Digits, 3);
      Base_10_Digits.Append (Base_10.Integer_Digits, 8);
      Base_10_Digits.Append (Base_10.Integer_Digits, 4);
      Base_10_Digits.Append (Base_10.Integer_Digits, 7);
      Base_10_Digits.Append (Base_10.Integer_Digits, 4);
      Base_10_Digits.Append (Base_10.Integer_Digits, 1);
      Base_10_Digits.Append (Base_10.Integer_Digits, 2);

      declare
         I : constant Integer := Convert (Base_10);
      begin
         Ahven.Assert (I = -2_147_483_648, GNAT.Source_Info.Source_Location & ", was " & I'Img);
      end;
   end Convert_Base_10_To_Integer_Test_Minus_2_147_483_648;

end Std_Integer.Tests;
