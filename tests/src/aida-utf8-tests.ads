with Ahven.Framework;

package Aida.UTF8.Tests is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_24 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_C2A2 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_E0AA81 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_E282AC (T : in out Ahven.Framework.Test_Case'Class);

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_F0908D88 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_F4808081 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_F3B09081 (T : in out Ahven.Framework.Test_Case'Class);

end Aida.UTF8.Tests;
