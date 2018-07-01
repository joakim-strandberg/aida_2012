with Ahven.Framework;
with Aida.Bounded_Hash_Map;

package Aida.Bounded_Hash_Map_Tests with SPARK_Mode is
   pragma Elaborate_Body;

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test) with
     SPARK_Mode => Off;

private

   function Default_Key     return Aida.Int32 is (0);
   function Default_Element return Aida.Int32 is (0);

   package Int_To_Int_Hash_Map is new Aida.Bounded_Hash_Map
     (Key_T                   => Aida.Int32,
      Element_T               => Aida.Int32,
      Default_Key             => Default_Key,
      Default_Element         => Default_Element,
      Hash                    => Aida.To_Hash32,
      Equivalent_Keys         => "=",
      Max_Hash_Map_Size       => 10,
      Max_Collision_List_Size => 10);

   procedure Test_Basic_Functionality (T : in out Ahven.Framework.Test_Case'Class);

end Aida.Bounded_Hash_Map_Tests;
