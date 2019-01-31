with Ahven.Framework;

package Aida.XML_DOM_Parser_Tests is
   pragma SPARK_Mode (On);
   pragma Elaborate_Body;

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);
   pragma SPARK_Mode (Off);

private

   procedure Test_Person_With_Age_0 (T : in out Ahven.Framework.Test_Case'Class) with
     Global => null;

end Aida.XML_DOM_Parser_Tests;
