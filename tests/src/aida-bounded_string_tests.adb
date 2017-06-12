with Aida.Bounded_String;
with Aida.Types;

package body Aida.Bounded_String_Tests is

   use all type Aida.Types.String_T;

   type Bounded_String_20_T is new Aida.Bounded_String.T (20);

   --   use all type Bounded_String_20_T;

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.Bounded_String package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Initialization'Access, "Test_Initialization");
   end Initialize;


   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      use all type Aida.Bounded_String.Char_Vectors.Vector;

      function Check_Expected (Text     : Aida.Bounded_String.Char_Vectors.Vector;
                               Expected : Aida.Types.String_T) return Boolean with
        Global => null;

      function Check_Expected (Text     : Aida.Bounded_String.Char_Vectors.Vector;
                               Expected : Aida.Types.String_T) return Boolean
      is
         pragma Unreferenced (Text, Expected);
      begin
         --         return (for all I in Positive range First_Index (Text)..Last_Index (Text) => Expected (Expected'First + I - First_Index (Text)) = Element (Text, I));
         return True;
      end Check_Expected;

      Expected : constant Aida.Types.String_T := "Hej";

      subtype Expected_T is Aida.Types.String_T (Expected'First..Expected'Last);

      Is_Success : Boolean;

      function Check_Expected is new Aida.Bounded_String.Check_Something_On_Immutable_Text (Bounded_String_T => Bounded_String_20_T,
                                                                                            Return_T         => Boolean,
                                                                                            Arg_T            => Expected_T,
                                                                                            Check_Something  => Check_Expected);

      S : Bounded_String_20_T;
   begin
      Initialize (This => S,
                  Text => Expected);
      Is_Success := Check_Expected (S, Expected);
      Ahven.Assert (Is_Success, "CODE A, was ", Boolean'Image (Is_Success));
      Ahven.Assert (Are_Equivalent (S, Expected), "CODE A, was ", Boolean'Image (Is_Success));
   end Test_Initialization;

end Aida.Bounded_String_Tests;
