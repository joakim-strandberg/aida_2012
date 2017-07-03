with Aida.Containers.Bounded_Hash_Map;
with Aida.Types;
with GNAT.Source_Info;

package body Aida.Bounded_Hash_Map_Tests is

   use all type Aida.Types.Int32_T;

   function Default_Key     return Aida.Types.Int32_T is (0);
   function Default_Element return Aida.Types.Int32_T is (0);

   package Int_To_Int_Hash_Map is new Aida.Containers.Bounded_Hash_Map (Key_T                   => Aida.Types.Int32_T,
                                                                        Element_T               => Aida.Types.Int32_T,
                                                                        Default_Key             => Default_Key,
                                                                        Default_Element         => Default_Element,
                                                                        Hash                    => Aida.Types.Hash32,
                                                                        Equivalent_Keys         => Aida.Types."=",
                                                                        Max_Hash_Map_Size       => 10,
                                                                        Max_Collision_List_Size => 10);

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Bounded_Hash_Map package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Basic_Functionality'Access, "Test_Basic_Functionality");
   end Initialize;

   use all type Int_To_Int_Hash_Map.T;

   procedure Test_Basic_Functionality (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Int_To_Int_Map : Int_To_Int_Hash_Map.T;
   begin
      Ahven.Assert (not Exists (Int_To_Int_Map, 1), GNAT.Source_Info.Source_Location);

      declare
         FR : constant Int_To_Int_Hash_Map.Find_Element_Result_T := Find_Element (Int_To_Int_Map, 1);
      begin
         Ahven.Assert (not FR.Exists, GNAT.Source_Info.Source_Location);
      end;

      Insert (This        => Int_To_Int_Map,
              Key         => 1,
              New_Element => 10);
      Ahven.Assert (Exists (Int_To_Int_Map, 1), GNAT.Source_Info.Source_Location);

      declare
         FR : constant Int_To_Int_Hash_Map.Find_Element_Result_T := Find_Element (Int_To_Int_Map, 1);
      begin
         Ahven.Assert (FR.Exists, GNAT.Source_Info.Source_Location);
         Ahven.Assert (FR.Element = 10, GNAT.Source_Info.Source_Location);
      end;

      Ahven.Assert (Element (Int_To_Int_Map, 1) = 10, GNAT.Source_Info.Source_Location);

      Delete (Int_To_Int_Map, 1);

      Ahven.Assert (not Exists (Int_To_Int_Map, 1), GNAT.Source_Info.Source_Location);

      declare
         FR : constant Int_To_Int_Hash_Map.Find_Element_Result_T := Find_Element (Int_To_Int_Map, 1);
      begin
         Ahven.Assert (not FR.Exists, GNAT.Source_Info.Source_Location);
      end;
   end Test_Basic_Functionality;

end Aida.Bounded_Hash_Map_Tests;
