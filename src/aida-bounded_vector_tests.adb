with Aida.Bounded_Vector;

package body Aida.Bounded_Vector_Tests is

--   type Bounded_String_20_T is new Aida.Bounded_String.T (20);

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

      function Default_Int return Integer is (0);
      pragma Annotate (GNATprove, Terminating, Default_Int);

      package Vector is new Aida.Bounded_Vector
        (Max_Last_Index  => 10,
         Element_T       => Integer,
         Default_Element => Default_Int);

      V : Vector.T;

      pragma Unreferenced (V);


--        function Check_Expected (Text     : Aida.String_T;
--                                 Expected : Aida.String_T) return Boolean with
--          Global => null;
--
--        function Check_Expected (Text     : Aida.String_T;
--                                 Expected : Aida.String_T) return Boolean is
--        begin
--           return Expected = Text;
--        end Check_Expected;
--
--        Expected : constant Aida.String_T := "Hej";
--
--        subtype Expected_T is Aida.String_T (Expected'First..Expected'Last);
--
--        Is_Success : Boolean;
--
--        function Check_Expected is new Bounded_String.Check_Something_On_Immutable_Text (Bounded_String_T => Bounded_String_20_T,
--                                                                                         Return_T         => Boolean,
--                                                                                         Arg_T            => Expected_T,
--                                                                                         Check_Something  => Check_Expected);
--
--        S : Bounded_String_20_T;
   begin
--        Initialize (This => S,
--                    Text => Expected);
--        Is_Success := Check_Expected (S, Expected);
--        Ahven.Assert (Is_Success, "CODE A, was ", Boolean'Image (Is_Success));
--        Ahven.Assert (S = Expected, "CODE A, was ", Boolean'Image (Is_Success));
      null;
   end Test_Initialization;

end Aida.Bounded_Vector_Tests;
