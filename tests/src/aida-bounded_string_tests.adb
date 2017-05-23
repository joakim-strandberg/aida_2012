with Aida.Bounded_String;
with Aida.Types;

package body Aida.Bounded_String_Tests is

   use all type Aida.Types.String_T;

   package Bounded_String_20 is new Aida.Bounded_String (20);

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Bounded_String package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Initialization'Access, "Test_Initialization");
   end Initialize;

   use all type Bounded_String_20.T;

   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      function Check_Expected (Text     : Aida.Types.String_T;
                               Expected : Aida.Types.String_T) return Boolean with
        Global => null;

      function Check_Expected (Text     : Aida.Types.String_T;
                               Expected : Aida.Types.String_T) return Boolean is
      begin
         return Expected = Text;
      end Check_Expected;

      Expected : constant Aida.Types.String_T := "Hej";

      subtype Expected_T is Aida.Types.String_T (Expected'First..Expected'Last);

      Is_Success : Boolean;

      function Check_Expected is new Bounded_String_20.Check_Something_On_Immutable_Text (Return_T        => Boolean,
                                                                                          Arg_T           => Expected_T,
                                                                                          Check_Something => Check_Expected);

      S : Bounded_String_20.T;
   begin
      Initialize (This => S,
                  Text => Expected);
      Is_Success := Check_Expected (S, Expected);
      Ahven.Assert (Is_Success, "CODE A, was ", Boolean'Image (Is_Success));
      Ahven.Assert (Are_Equivalent (S, Expected), "CODE A, was ", Boolean'Image (Is_Success));
   end Test_Initialization;

end Aida.Bounded_String_Tests;
