with Ahven.Framework;

package Aida.JSON_Parsing_Tests with SPARK_Mode is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Name_Adam_1 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

end Aida.JSON_Parsing_Tests;
