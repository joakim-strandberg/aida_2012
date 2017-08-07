with Ahven.Framework;
with Aida.Json_Parsing_Tests_Model;
with Aida.Bounded_Vector;

package Aida.JSON_Parsing_Tests with SPARK_Mode is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   use all type Aida.Json_Parsing_Tests_Model.Person_Def.Name_T;
   use all type Aida.Json_Parsing_Tests_Model.Person_Def.Hand_Vector.T;
   use all type Aida.Json_Parsing_Tests_Model.Person_Def.Vehicle_Vector.T;
   use all type Aida.Json_Parsing_Tests_Model.Person_Def.Is_Happy_T;

   subtype Max_Indices_T is Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;

   type Storage_T is record
      Person : Json_Parsing_Tests_Model.People_T := (others => (Age      => 10,
                                                                Name     => Make,
                                                                Length   => 0.0,
                                                                Hands    => Default_Vector (Json_Parsing_Tests_Model.Person_Def.HANDS_MAX),
                                                                Vehicles => Default_Vector (Json_Parsing_Tests_Model.Person_Def.VEHICLES_MAX),
                                                                Is_Happy => (Exists => False)));
      Hand    : Json_Parsing_Tests_Model.Hands_T;
      Vehicle : Json_Parsing_Tests_Model.Vehicles_T;
   end record;

   Storage : Storage_T;

   function Default_Person_Id return Json_Parsing_Tests_Model.Person_Id_T is (1);

   package Person_Id_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                        Element_T       => Json_Parsing_Tests_Model.Person_Id_T,
                                                        Default_Element => Default_Person_Id);

   package Hand_Id_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                      Element_T       => Json_Parsing_Tests_Model.Hand_Id_T,
                                                      Default_Element => Json_Parsing_Tests_Model.Person_Def.Default_Hand_Id);

   package Vehicle_Id_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                         Element_T       => Json_Parsing_Tests_Model.Vehicle_Id_T,
                                                         Default_Element => Json_Parsing_Tests_Model.Person_Def.Default_Vehicle_Id);

   procedure Test_Person_With_Name_Adam_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Name_Adam_1 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Age_1 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Length_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Vehicles_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Name_And_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Is_Happy_True_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Is_Happy_False_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

   procedure Test_Person_With_Is_Happy_Null_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => (In_Out => (Storage, Aida.Json_Parsing_Tests_Model.Max_Indices));

end Aida.JSON_Parsing_Tests;
