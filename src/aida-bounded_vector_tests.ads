with Ahven.Framework;

package Aida.Bounded_Vector_Tests with SPARK_Mode is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

end Aida.Bounded_Vector_Tests;
