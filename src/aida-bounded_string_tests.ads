with Ahven.Framework;

package Aida.Bounded_String_Tests with SPARK_Mode is
   pragma Elaborate_Body;

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test) with
     SPARK_Mode => Off;

private

   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

end Aida.Bounded_String_Tests;
