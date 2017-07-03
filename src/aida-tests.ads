with Ahven.Framework;

package Aida.Tests is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   procedure SHOULD_Successfully_Convert_String_0_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

   procedure SHOULD_Successfully_Convert_String_100_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

   procedure SHOULD_Successfully_Convert_String_2147483647_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

   procedure SHOULD_Successfully_Convert_String_2147483648_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

   procedure SHOULD_Successfully_Convert_String_Minus_1_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

   procedure SHOULD_Successfully_Convert_String_Minus_100_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

   procedure SHOULD_Successfully_Convert_String_Minus_2147483648_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

   procedure SHOULD_Fail_To_Convert_String_Minus_2147483649_To_Integer (T : in out Ahven.Framework.Test_Case'Class);

end Aida.Tests;
