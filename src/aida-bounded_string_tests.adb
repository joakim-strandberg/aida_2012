with Aida.Bounded_String;

package body Aida.Bounded_String_Tests is

   type Bounded_String_20_T is new Aida.Bounded_String.T (20);

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Bounded_String package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Initialization'Access, "Test_Initialization");
   end Initialize;


   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      Expected : constant Aida.String_T := "Hej";

      Is_Success : Boolean;

      S : Bounded_String_20_T;
   begin
      Initialize (This => S,
                  Text => Expected);
      Is_Success := To_String (S) = Expected;
      Ahven.Assert (Is_Success, "CODE A, was ", Boolean'Image (Is_Success));
   end Test_Initialization;

end Aida.Bounded_String_Tests;
