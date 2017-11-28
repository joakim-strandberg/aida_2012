with Aida.Subprogram_Call_Result;

package body Aida.Subprogram_Call_Result_Tests is

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Subprogram_Call_Result_Tests package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_0'Access, "Test_0");
      Ahven.Framework.Add_Test_Routine (T, Test_1'Access, "Test_1");
      Ahven.Framework.Add_Test_Routine (T, Test_2'Access, "Test_2");
      Ahven.Framework.Add_Test_Routine (T, Test_3'Access, "Test_3");
      Ahven.Framework.Add_Test_Routine (T, Test_4'Access, "Test_4");
      Ahven.Framework.Add_Test_Routine (T, Test_5'Access, "Test_5");
      Ahven.Framework.Add_Test_Routine (T, Test_6'Access, "Test_6");
   end Initialize;

   procedure Test_0 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Call_Result.Initialize (1600190451, 1176306482);
      Ahven.Assert (Call_Result.Message = "1600190451, 1176306482", "was ", String (Call_Result.Message));
   end Test_0;

   procedure Test_1 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Call_Result.Initialize (-1600190451, 1176306482);
      Ahven.Assert (Call_Result.Message = "-1600190451, 1176306482", "was ", String (Call_Result.Message));
   end Test_1;

   procedure Test_2 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Call_Result.Initialize (1600190451, -1176306482);
      Ahven.Assert (Call_Result.Message = "1600190451, -1176306482", "was ", String (Call_Result.Message));
   end Test_2;

   procedure Test_3 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Call_Result.Initialize (-1600190451, -1176306482);
      Ahven.Assert (Call_Result.Message = "-1600190451, -1176306482", "was ", String (Call_Result.Message));
   end Test_3;

   procedure Test_4 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Call_Result.Initialize (-0719940489, 0872953166);
      Ahven.Assert (Call_Result.Message = "-0719940489, 0872953166", "was ", String (Call_Result.Message));
   end Test_4;

   procedure Test_5 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Call_Result.Initialize (-0719940489, -0872953166);
      Ahven.Assert (Call_Result.Message = "-0719940489, -0872953166", "was ", String (Call_Result.Message));
   end Test_5;

   procedure Test_6 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      Call_Result.Initialize (0719940489, -0872953166);
      Ahven.Assert (Call_Result.Message = "0719940489, -0872953166", "was ", String (Call_Result.Message));
   end Test_6;

end Aida.Subprogram_Call_Result_Tests;
