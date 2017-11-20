with Ahven.Framework;

package Aida.Bounded_Hash_Map_Tests with SPARK_Mode is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test) with
     SPARK_Mode => Off;

private

   procedure Test_Basic_Functionality (T : in out Ahven.Framework.Test_Case'Class);

end Aida.Bounded_Hash_Map_Tests;
