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

      package Int_To_String_Map is new Aida.Integer_To_String_Map (Max_Chars   => 100,
                                                                   Max_Strings => 10,
                                                                   Value_T     => String);

      Map : Int_To_String_Map.T;

      I : Int_To_String_Map.Key_T;
   begin
      Ahven.Assert (Map.Available_Chars = 100,  "was ", String (Aida.String_T'(Aida.To_String (Map.Available_Chars))));
      Ahven.Assert (Map.Available_Keys = 10, "was ", String (Aida.String_T'(Aida.To_String (Map.Available_Keys))));
      for J in Int_To_String_Map.Key_T'Range loop
         Ahven.Assert (Map.Value (J) = "", "was ", Map.Value (J));
         pragma Loop_Variant (Increases => J);
      end loop;

      Map.Append (Value => "Hej",
                  Key    => I);

      Ahven.Assert (Map.Value (I) = "Hej", "was ", Map.Value (I));
   end Test_0;

end Aida.Integer_To_String_Map_Tests;
