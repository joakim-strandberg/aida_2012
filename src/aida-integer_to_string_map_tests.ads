with Ahven.Framework;

package Aida.Integer_To_String_Map_Tests with SPARK_Mode is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test) with
     SPARK_Mode => Off;

private

   procedure Test_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

end Aida.Integer_To_String_Map_Tests;
