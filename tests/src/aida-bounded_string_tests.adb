with Aida.Bounded_String;
with Aida.Types;
with GNAT.Source_Info;

package body Aida.Bounded_String_Tests is

   use all type Aida.Types.String_T;

   package Bounded_String_20 is new Aida.Bounded_String (20);

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Bounded_String package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Initialization'Access, "Test_Initialization");
   end Initialize;

   use all type Bounded_String_20.T;

   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant String := "Hej";

      Is_Success : Boolean := False;

      procedure Check_Expected (Text : String) is
      begin
         Is_Success := Equivalent (Expected, Aida.Types.String_T (Text));
      end Check_Expected;

      procedure Check_Expected is new Bounded_String_20.Act_On_Immutable_Text (Check_Expected);

      S : Bounded_String_20.T;
   begin
      Initialize (This => S,
                  Text => Expected);
      Check_Expected (S);
      Ahven.Assert (Is_Success, GNAT.Source_Info.Source_Location & ", was " & Is_Success'Img);
   end Test_Initialization;

end Aida.Bounded_String_Tests;
