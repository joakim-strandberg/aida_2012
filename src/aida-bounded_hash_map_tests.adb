with GNAT.Source_Info;

package body Aida.Bounded_Hash_Map_Tests is

   procedure Initialize (T : in out Test) is
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
