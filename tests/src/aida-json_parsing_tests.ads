with Ahven.Framework;
with Aida.Json_Parsing_Tests_Model;

package Aida.JSON_Parsing_Tests with SPARK_Mode is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   use all type Aida.Json_Parsing_Tests_Model.Person_Def.Name_T;

   subtype Max_Indices_T is Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;

   type Storage_T is record
      Person : Json_Parsing_Tests_Model.People_T := (others => (Age  => 10,
                                                                Name => Make));
   end record;

   Storage : Storage_T;

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

--     procedure Test_Person_With_Name_Adam_1 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;
--
--     procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;
--
--     procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;
--
--     procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;
--
--     procedure Test_Person_With_Name_Adam_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
--       Global => null;

end Aida.JSON_Parsing_Tests;
