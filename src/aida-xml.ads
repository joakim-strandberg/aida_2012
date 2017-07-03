with Aida.UTF8_Code_Point;
with Aida.Subprogram_Call_Result;
with Aida.Containers.Bounded_Vector;

pragma Elaborate_All (Aida.Containers.Bounded_Vector);
pragma Elaborate_All (Aida.Subprogram_Call_Result);

package Aida.XML with SPARK_Mode is

   use all type Aida.UTF8_Code_Point.T;

   type Tag_Id_T is new Aida.Int32_T;

   package Procedure_Call_Result is new Aida.Subprogram_Call_Result (1_000);

private

   MAX_DEPTH : constant := 50;

   function Default_Boolean return Boolean with
     Global => null;

   function Default_Boolean return Boolean is (False);

   type Boolean_Index_T is new Aida.Pos32_T range 1..MAX_DEPTH;

   package Boolean_Vector is new Aida.Containers.Bounded_Vector (Index_T         => Boolean_Index_T,
                                                                 Element_T       => Boolean,
                                                                 "="             => "=",
                                                                 Default_Element => Default_Boolean);

   type Tag_T is record
      Id : Tag_Id_T;
      Name_First_Index : Positive;
      Name_Last_Index  : Positive;
   end record;

   function Default_Tag_Id return Tag_T with
     Global => null;

   function Default_Tag_Id return Tag_T is (Id               => 0,
                                            Name_First_Index => 1,
                                            Name_Last_Index  => 1);

   type Tag_Id_Index_T is new Aida.Pos32_T range 1..MAX_DEPTH;

   package Tag_Id_Vector is new Aida.Containers.Bounded_Vector (Index_T         => Tag_Id_Index_T,
                                                                Element_T       => Tag_T,
                                                                "="             => "=",
                                                                Default_Element => Default_Tag_Id);

   type State_Id_Type is (
                          Expecting_NL_Sign_Or_Space_Or_Less_Sign, -- NL = New Line
                          Found_Less_Sign,
--                            Found_Less_Followed_By_Exclamation_Sign,
--                            Found_Less_Followed_By_Exclamation_And_Dash_Sign,
                          Extracting_Root_Start_Tag_Name,
                          Extracting_Start_Tag_Name,
                          Expecting_G_Sign_Or_Extracting_Attributes,
                          Expecting_G_Sign_Or_Extracting_Attributes_For_Root,
--                          Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash,
--                            Extracting_Attribute_Name,
--                            Expecting_Attribute_Value_Quotation_Mark,
--                            Extracting_Attribute_Value,
                          Expecting_New_Tag_Or_Extracting_Tag_Value, -- Or start of comment or start- tag or end-tag
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L,
                          Extracting_Root_End_Tag_Name,
                          Expecting_Only_Trailing_Spaces,
                          Extracting_End_Tag_Name--,
--                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash,
                          -- Enumeration values introduced to handle <!CDATA[--]]>
--                            Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation,
--                            Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C,
--                            Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD,
--                            Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA,
--                            Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT,
--                            Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA,
--                            Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket,
--                            Extracting_CDATA,
--                            Extracting_CDATA_Found_Square_Bracket,
--                            Extracting_CDATA_Found_Two_Square_Brackets,
--                            Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign,
--                            Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign,
--                            Extracting_Comment,
--                            Extracting_Comment_And_Found_Dash,
--                            Extracting_Comment_And_Found_Dash_Dash
                         );

   type Expected_Quotation_Symbol_T is (
                                        Single_Quotes, -- Example: 'hello'
                                        Double_Quotes  -- Example: "hello"
                                       );

   function Is_Special_Symbol (CP : Aida.UTF8_Code_Point.T) return Boolean is (if CP = Character'Pos ('<') then
                                                                                  True
                                                                               elsif CP = Character'Pos ('>') then
                                                                                  True
                                                                               elsif CP = Character'Pos ('/') then
                                                                                  True
                                                                               elsif CP = Character'Pos ('"') then
                                                                                  True
                                                                               else
                                                                                  False);

end Aida.XML;
