with Ahven.Framework;

package Std_Integer.Tests is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

--     procedure To_String_Test_1 (T : in out Ahven.Framework.Test_Case'Class);
--
--     procedure To_String_Test_10 (T : in out Ahven.Framework.Test_Case'Class);
--
--     procedure To_String_Test_2_147_483_647 (T : in out Ahven.Framework.Test_Case'Class);
--
--     procedure To_String_Test_Minus_1 (T : in out Ahven.Framework.Test_Case'Class);
--
--     procedure To_String_Test_Minus_198 (T : in out Ahven.Framework.Test_Case'Class);
--
--     procedure To_String_Test_Minus_2_147_483_648 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Convert_Base_10_To_Integer_Test_32 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Convert_Base_10_To_Integer_Test_Minus_32 (T : in out Ahven.Framework.Test_Case'Class);

   procedure Convert_Base_10_To_Integer_Test_Minus_2_147_483_648 (T : in out Ahven.Framework.Test_Case'Class);

end Std_Integer.Tests;
