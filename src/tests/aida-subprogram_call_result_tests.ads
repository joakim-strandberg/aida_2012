with Ahven.Framework;

package Aida.Subprogram_Call_Result_Tests is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test) with
     SPARK_Mode => Off;

private

   procedure Test_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_1 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_2 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_3 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_4 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_5 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_6 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

end Aida.Subprogram_Call_Result_Tests;
