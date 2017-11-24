with Aida.Integer_To_String_Map;

package body Aida.Integer_To_String_Map_Tests is

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Integer_To_String_Map_Tests tests");

      Ahven.Framework.Add_Test_Routine (T, Test_0'Access, "Test_0");
   end Initialize;

   procedure Test_0 (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package Int_To_String_Map is new Aida.Integer_To_String_Map (Capacity    => 100,
                                                                   Max_Strings => 10,
                                                                   Element_T   => String);

      Map : Int_To_String_Map.T;

      I : Int_To_String_Map.Index_T;
   begin
      for J in Int_To_String_Map.Index_T'Range loop
         Ahven.Assert (Map.Element (J) = "", "was ", Map.Element (J));
         pragma Loop_Variant (Increases => J);
      end loop;

      Map.Append (New_Item => "Hej",
                  Index    => I);

      Ahven.Assert (Map.Element (I) = "Hej", "was ", Map.Element (I));
   end Test_0;

end Aida.Integer_To_String_Map_Tests;
