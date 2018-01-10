with GNAT.Source_Info;

package body Aida.UTF8.Tests is

   overriding procedure Initialize (T : in out Test)
   is
   begin
      Set_Name (T, "Aida.UTF8 package tests");

      Ahven.Framework.Add_Test_Routine (T, Extract_UTF8_Code_Point_Hexadecimal_Case_24'Access, "Extract_UTF8_Code_Point_Hexadecimal_Case_24");
      Ahven.Framework.Add_Test_Routine (T, Extract_UTF8_Code_Point_Hexadecimal_Case_C2A2'Access, "Extract_UTF8_Code_Point_Hexadecimal_Case_C2A2");
      Ahven.Framework.Add_Test_Routine (T, Extract_UTF8_Code_Point_Hexadecimal_Case_E0AA81'Access, "Extract_UTF8_Code_Point_Hexadecimal_Case_E0AA81");
      Ahven.Framework.Add_Test_Routine (T, Extract_UTF8_Code_Point_Hexadecimal_Case_E282AC'Access, "Extract_UTF8_Code_Point_Hexadecimal_Case_E282AC");
      Ahven.Framework.Add_Test_Routine (T, Extract_UTF8_Code_Point_Hexadecimal_Case_F0908D88'Access, "Extract_UTF8_Code_Point_Hexadecimal_Case_F0908D88");
      Ahven.Framework.Add_Test_Routine (T, Extract_UTF8_Code_Point_Hexadecimal_Case_F4808081'Access, "Extract_UTF8_Code_Point_Hexadecimal_Case_F4808081");
      Ahven.Framework.Add_Test_Routine (T, Extract_UTF8_Code_Point_Hexadecimal_Case_F3B09081'Access, "Extract_UTF8_Code_Point_Hexadecimal_Case_F3B09081");
   end Initialize;

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_24 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Source : constant String_T (1..1) := (1 => Character_T'Val (16#24#));
      Pointer : Integer := 1;
      Value : Aida.UTF8_Code_Point.T;
   begin
      Aida.UTF8.Get (Source      => Source,
                     Pointer     => Pointer,
                     Value       => Value);
      Ahven.Assert (Value = 2#010_0100#, GNAT.Source_Info.Source_Location & ", was " & Value'Img);
   end Extract_UTF8_Code_Point_Hexadecimal_Case_24;

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_C2A2 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Source : constant String_T (1..2) := (1 => Character_T'Val (16#C2#), 2 => Character_T'Val (16#A2#));
      Pointer : Integer := 1;
      Value : Aida.UTF8_Code_Point.T;
   begin
      Aida.UTF8.Get (Source      => Source,
                     Pointer     => Pointer,
                     Value       => Value);
      Ahven.Assert (Value = 2#000_1010_0010#, GNAT.Source_Info.Source_Location & ", was " & Value'Img);
   end Extract_UTF8_Code_Point_Hexadecimal_Case_C2A2;

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_E282AC (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Source : constant String_T (1..3) := (1 => Character_T'Val (16#E2#), 2 => Character_T'Val (16#82#), 3 => Character_T'Val (16#AC#));
      Pointer : Integer := 1;
      Value : Aida.UTF8_Code_Point.T;
   begin
      Aida.UTF8.Get (Source      => Source,
                     Pointer     => Pointer,
                     Value       => Value);
      Ahven.Assert (Value = 2#0010_0000_1010_1100#, GNAT.Source_Info.Source_Location & ", was " & Value'Img);
   end Extract_UTF8_Code_Point_Hexadecimal_Case_E282AC;

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_E0AA81 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Source : constant String_T (1..3) := (1 => Character_T'Val (16#E0#), 2 => Character_T'Val (16#AA#), 3 => Character_T'Val (16#81#));
      Pointer : Integer := 1;
      Value : Aida.UTF8_Code_Point.T;
   begin
      Aida.UTF8.Get (Source      => Source,
                     Pointer     => Pointer,
                     Value       => Value);
      Ahven.Assert (Value = 16#0A81#, GNAT.Source_Info.Source_Location & ", was " & Value'Img);
   end Extract_UTF8_Code_Point_Hexadecimal_Case_E0AA81;


   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_F0908D88 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Source : constant String_T (1..4) := (1 => Character_T'Val (16#F0#), 2 => Character_T'Val (16#90#), 3 => Character_T'Val (16#8D#), 4 => Character_T'Val (16#88#));
      Pointer : Integer := 1;
      Value : Aida.UTF8_Code_Point.T;
   begin
      Aida.UTF8.Get (Source      => Source,
                     Pointer     => Pointer,
                     Value       => Value);
      Ahven.Assert (Value = 2#0_0001_0000_0011_0100_1000#, GNAT.Source_Info.Source_Location & ", was " & Value'Img);
   end Extract_UTF8_Code_Point_Hexadecimal_Case_F0908D88;

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_F4808081 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Source : constant String_T (1..4) := (1 => Character_T'Val (16#F4#), 2 => Character_T'Val (16#80#), 3 => Character_T'Val (16#80#), 4 => Character_T'Val (16#81#));
      Pointer : Integer := 1;
      Value : Aida.UTF8_Code_Point.T;
   begin
      Aida.UTF8.Get (Source      => Source,
                     Pointer     => Pointer,
                     Value       => Value);
      Ahven.Assert (Value = 16#100001#, GNAT.Source_Info.Source_Location & ", was " & Value'Img);
   end Extract_UTF8_Code_Point_Hexadecimal_Case_F4808081;

   procedure Extract_UTF8_Code_Point_Hexadecimal_Case_F3B09081 (T : in out Ahven.Framework.Test_Case'Class) is
      pragma Unreferenced (T);

      Source : constant String_T (1..4) := (1 => Character_T'Val (16#F3#), 2 => Character_T'Val (16#B0#), 3 => Character_T'Val (16#90#), 4 => Character_T'Val (16#81#));
      Pointer : Integer := 1;
      Value : Aida.UTF8_Code_Point.T;
   begin
      Aida.UTF8.Get (Source      => Source,
                     Pointer     => Pointer,
                     Value       => Value);
      Ahven.Assert (Value = 16#F0401#, GNAT.Source_Info.Source_Location & ", was " & Value'Img);
   end Extract_UTF8_Code_Point_Hexadecimal_Case_F3B09081;

end Aida.UTF8.Tests;
