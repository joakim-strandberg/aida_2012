with Ahven.Framework;
with Aida.Json_Parsing_Tests_Model;
with Aida.Tagged_Bounded_Vector;

pragma Elaborate_All (Aida.Json_Parsing_Tests_Model);
pragma Elaborate_All (Aida.Tagged_Bounded_Vector);

package Aida.XML_SAX_Parse_Tests with SPARK_Mode is
   pragma Elaborate_Body;

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test) with
     SPARK_Mode => Off;

private

   subtype Max_Indices_T is Aida.Json_Parsing_Tests_Model.Max_Indices_Def.T;

   type Storage_T is tagged limited record
      Header_Comment : Standard.String (1..100) := (others => ' '); -- Not using Aida.Bounded_String at the moment.
      Person  : Json_Parsing_Tests_Model.People_T := (others => Json_Parsing_Tests_Model.Person_Def.Make);
      Hand    : Json_Parsing_Tests_Model.Hands_T;
      Vehicle : Json_Parsing_Tests_Model.Vehicles_T;
   end record;

   procedure Clear (S : in out Storage_T) with
     Global => null;

   function Default_Person_Id return Json_Parsing_Tests_Model.Person_Id_T is (1);

   MAX_IDS : constant := 10;

   package Person_Id_Vector is new Aida.Tagged_Bounded_Vector
     (Max_Last_Index  => Integer'First + MAX_IDS,
      Element_T       => Json_Parsing_Tests_Model.Person_Id_T,
      Default_Element => Default_Person_Id);

   package Hand_Id_Vector is new Aida.Tagged_Bounded_Vector
     (Max_Last_Index  => Integer'First + MAX_IDS,
      Element_T       => Json_Parsing_Tests_Model.Hand_Id_T,
      Default_Element => Json_Parsing_Tests_Model.Person_Def.Default_Hand_Id);

   package Vehicle_Id_Vector is new Aida.Tagged_Bounded_Vector
     (Max_Last_Index  => Integer'First + MAX_IDS,
      Element_T       => Json_Parsing_Tests_Model.Vehicle_Id_T,
      Default_Element => Json_Parsing_Tests_Model.Person_Def.Default_Vehicle_Id);

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_1 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_2 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_3 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_4 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_1 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_2 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_3 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_4 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_5 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_6 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Hand_7 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_Pre_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_Post_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Age_Pre_CDATA_Comment_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Single_Quote_Attribute_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

   procedure Test_Person_With_Double_Quote_Attribute_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

end Aida.XML_SAX_Parse_Tests;
