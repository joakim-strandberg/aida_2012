with Aida.Subprogram_Call_Result;
with Aida.Containers.Bounded_Vector;

package Aida.JSON with SPARK_Mode is

   type Tag_Id_T is new Aida.Int32_T;

   package Procedure_Call_Result is new Aida.Subprogram_Call_Result (1_000);

private

   MAX_DEPTH : constant := 50;

   function Default_Tag_Id return Tag_Id_T with
     Global => null;

   function Default_Tag_Id return Tag_Id_T is (0);

   type Tag_Id_Index_T is new Aida.Pos32_T range 1..MAX_DEPTH;

   package Tag_Id_Vector is new Aida.Containers.Bounded_Vector (Index_T         => Tag_Id_Index_T,
                                                                Element_T       => Tag_Id_T,
                                                                "="             => "=",
                                                                Default_Element => Default_Tag_Id);

   type State_Id_Type is (
                          Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket, -- NL = New Line
                          Found_Left_Curly_Bracket, -- or will search for key
                          Extracting_Key_Name,
                          Expecting_Colon_Sign_After_Key_Name,
                          Expecting_Value, -- Can be string, number, array, boolean value or null
                          Extracting_Value_String,
                          Expecting_Comma_Sign_Or_Right_Bracket,
                          Found_End_Of_The_Very_Last_Object,
                          Extracting_Value_Integer,
                          Found_End_Of_Object,
                          Found_Array_Start, -- Extracting elements
                          Found_End_Of_Element_In_Array
                         );

end Aida.JSON;
