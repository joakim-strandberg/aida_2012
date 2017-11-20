with Aida.UTF8_Code_Point;
with Aida.Bounded_Vector;

pragma Elaborate_All (Aida.Bounded_Vector);

package Aida.XML with SPARK_Mode is

   use all type Aida.UTF8_Code_Point.T;

private

   type Initial_State_Id_T is (Initial_State_Expecting_Less_Sign,
                               Initial_State_Expecting_Question_Mark,
                               Initial_State_Expecting_X,
                               Initial_State_Expecting_XM,
                               Initial_State_Expecting_XML,
                               Initial_State_Expecting_XML_S,
                               Initial_State_Expecting_XML_S_V,
                               Initial_State_Expecting_XML_S_VE,
                               Initial_State_Expecting_XML_S_VER,
                               Initial_State_Expecting_XML_S_VERS,
                               Initial_State_Expecting_XML_S_VERSI,
                               Initial_State_Expecting_XML_S_VERSIO,
                               Initial_State_Expecting_XML_S_VERSION,
                               Initial_State_Expecting_XML_S_VERSION_E,
                               Initial_State_Expecting_XML_S_VERSION_E_Q,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_E,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_EN,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENC,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q,
                               Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark,
                               End_State
                              );

   type State_Id_Type is (
                          Expecting_NL_Sign_Or_Space_Or_Less_Sign, -- NL = New Line
                          Init_Found_Less_Sign,                                  -- First start tag has not yet been found
                          Init_Found_Less_Followed_By_Exclamation_Sign,          -- First start tag has not yet been found
                          Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign, -- First start tag has not yet been found
                          Extracting_Start_Tag_Name,
                          Expecting_G_Sign_Or_Extracting_Attributes,
                          Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash,
                          Extracting_Attribute_Name,
                          Expecting_Attribute_Value_Quotation_Mark,
                          Extracting_Attribute_Value,
                          Expecting_New_Tag_Or_Extracting_Tag_Value, -- Or start of comment or start- tag or end-tag
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L,
                          Expecting_Only_Trailing_Spaces,
                          Extracting_End_Tag_Name,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash,
                          -- Enumeration values introduced to handle <!CDATA[--]]>
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket,
                          Extracting_CDATA,
                          Extracting_CDATA_Found_Square_Bracket,
                          Extracting_CDATA_Found_Two_Square_Brackets,
                          Init_Extracting_Comment,                     -- First start tag has not yet been found
                          Init_Extracting_Comment_And_Found_Dash,      -- First start tag has not yet been found
                          Init_Extracting_Comment_And_Found_Dash_Dash, -- First start tag has not yet been found
                          Extracting_Comment,
                          Extracting_Comment_And_Found_Dash,
                          Extracting_Comment_And_Found_Dash_Dash
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

   XML_IDENTIFIER_ERROR_1 : constant Aida.Int32_T := -1913564897;
   XML_IDENTIFIER_ERROR_2 : constant Aida.Int32_T := -0537097086;

end Aida.XML;
