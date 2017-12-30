with Ahven.Framework;
--  with Aida.Json_Parsing_Tests_Model;
--  with Aida.Bounded_Vector;

package Aida.JSON_DOM_Parser_Tests with SPARK_Mode is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test) with
     SPARK_Mode => Off;

private

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

--     procedure Test_Person_With_Length_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;
--
   procedure Test_Person_With_Name_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

--     procedure Test_Person_With_Is_Happy_True_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;
--
--     procedure Test_Person_With_Is_Happy_False_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;
--
--     procedure Test_Person_With_Is_Happy_Null_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;

end Aida.JSON_DOM_Parser_Tests;
