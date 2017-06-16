with Aida.Bounded_String;
--with Aida.Text_IO;
with Aida.Types;
with Aida.XML.Generic_Parse_XML_File;

package body Aida.XML_Parsing_Tests is

   use all type Aida.Types.String_T;
   use all type Aida.XML.Procedure_Call_Result.T;
   use all type Aida.Bounded_String.T;

   overriding procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Aida.XML.Generic_Parse_XML_File package tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Initialization'Access, "Test_Initialization");
   end Initialize;

   procedure Test_Initialization (T : in out Ahven.Framework.Test_Case'Class) with
     SPARK_Mode => On
   is
      pragma Unreferenced (T);

      package A is

         package Z is

            Max_Number_Of_A : constant := 2;

            MAX_VALUE_LENGTH : constant := 10;

            type Value_T is new Aida.Bounded_String.T (MAX_VALUE_LENGTH);

         end Z;

         type Id_T (<>) is private;

         type T is limited private;

         procedure Allocate_Memory (This  : in out T;
                                    Value : Aida.Types.String_T) with
           Global => null,
           Pre    => Is_Memory_Available (This) and Value'Length <= Z.MAX_VALUE_LENGTH;
         pragma Unreferenced (Allocate_Memory);

         function Value_Equals_String (This : T;
                                       Id   : Id_T;
                                       S    : Aida.Types.String_T) return Boolean with
           Global => null;

         function Is_Memory_Available (This : T) return Boolean with
           Global => null;

      private

         use all type Z.Value_T;

         type Hidden_A_Id_T is range 1..Z.Max_Number_Of_A;

         type Id_T is
            record
               My_Id : Hidden_A_Id_T := Hidden_A_Id_T'First;
            end record;

         type Hidden_A_T is record
            My_Id    : Id_T;
            My_Value : Z.Value_T;
         end record;

         type A_Array_T is array (Hidden_A_Id_T) of Hidden_A_T;

         type Extended_Hidden_A_Id_T is range Hidden_A_Id_T'First .. Hidden_A_Id_T'Last + 1;

         type T is limited record
            As                  : A_Array_T;
            Next_Available_A_Id : Extended_Hidden_A_Id_T := Extended_Hidden_A_Id_T'First;
         end record;

         function Is_Memory_Available (This : T) return Boolean is (This.Next_Available_A_Id <= Extended_Hidden_A_Id_T (Hidden_A_Id_T'Last));

         function Value_Equals_String (This : T;
                                       Id   : Id_T;
                                       S    : Aida.Types.String_T) return Boolean is (Aida.Bounded_String.Are_Equivalent (Aida.Bounded_String.T (This.As (Id.My_Id).My_Value), S));

      end A;

      package body A is

         procedure Allocate_Memory (This  : in out T;
                                    Value : Aida.Types.String_T)
         is
            Id : constant Hidden_A_Id_T := Hidden_A_Id_T (This.Next_Available_A_Id);
         begin
            This.As (Id).My_Id := (My_Id => Id);
            Initialize (This.As (Id).My_Value, Value);
            This.Next_Available_A_Id := This.Next_Available_A_Id + 1;
         end Allocate_Memory;

      end A;

      subtype A_T is A.T;

      package Result is

         type T is limited record
            A : A_T;
         end record;

      end Result;

      subtype Result_T is Result.T;

      procedure Root_Start_Tag (Result      : Result_T;
                                Tag_Name    : Aida.Types.String_T;
                                Tag_Id      : Aida.XML.Tag_Id_T;
                                Call_Result : in out Aida.XML.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_Start_Tag (Result      : Result_T;
                                Tag_Name    : Aida.Types.String_T;
                                Tag_Id      : Aida.XML.Tag_Id_T;
                                Call_Result : in out Aida.XML.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Call_Result);
      begin
  --       Aida.Text_IO.Put ("start tag: ");
  --           Put_Line (Tag_Name);
         null;
      end Root_Start_Tag;

      procedure Root_End_Tag (Result      : Result_T;
                              Tag_Name    : Aida.Types.String_T;
                              Tag_Value   : Aida.Types.String_T;
                              Tag_Id      : Aida.XML.Tag_Id_T;
                              Call_Result : in out Aida.XML.Procedure_Call_Result.T) with
        Global => null;

      procedure Root_End_Tag (Result      : Result_T;
                              Tag_Name    : Aida.Types.String_T;
                              Tag_Value   : Aida.Types.String_T;
                              Tag_Id      : Aida.XML.Tag_Id_T;
                              Call_Result : in out Aida.XML.Procedure_Call_Result.T)
      is
         pragma Unreferenced (Result);
         pragma Unreferenced (Tag_Id);
         pragma Unreferenced (Tag_Value);
         pragma Unreferenced (Call_Result);
      begin
--         Aida.Text_IO.Put ("end tag: ");
--         Put_Line (Tag_Name);
         null;
      end Root_End_Tag;

      procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Result_T,
                                                                  Root_Start_Tag,
                                                                  Root_End_Tag);

      Call_Result : Aida.XML.Procedure_Call_Result.T;

      XML : constant Aida.Types.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?><a>b</a>";

      R : Result_T;
   begin
      Parse_XML (R, XML, Call_Result);

      Ahven.Assert (not Has_Failed (Call_Result), "Code B");
   end Test_Initialization;

end Aida.XML_Parsing_Tests;
