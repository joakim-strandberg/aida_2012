with Aida.UTF8;
with Aida.UTF8_Code_Point;
with Ada.Characters.Latin_1;
with Aida.Text_IO;

-- Known unsupported issues: Escaping of text (for example &amp;)
-- The stack roof may be hit if the comments and texts in the XML are HUGE.
-- It should not be an issue in general.
procedure Aida.XML_SAX_Parse (Arg1        : in out Arg1_T;
                              Arg2        : in out Arg2_T;
                              Arg3        : in out Arg3_T;
                              Arg4        : in out Arg4_T;
                              Contents    : Aida.String_T;
                              Call_Result : in out Subprogram_Call_Result.T)
is
   use all type Aida.String_T;
   use all type Aida.Int32_T;
   use all type Aida.UTF8_Code_Point.T;

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

   subtype P_T      is Integer range Contents'First..Contents'Last + 4;
   subtype Prev_P_T is Integer range Contents'First + 1..Contents'Last;

   procedure Analyze_XML (P : in out P_T) with
     Global => (In_Out => (Call_Result, Arg1, Arg2, Arg3, Arg4),
                Input  => Contents),
     Pre    => not Call_Result.Has_Failed and P > Contents'First and P <= Contents'Last and Contents'Last < Integer'Last - 4;

   procedure Analyze_XML (P : in out P_T)
   is
      Depth : Aida.Nat32_T := 0;

      State_Id : State_Id_Type := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

      subtype Prev_Prev_P_T is Integer range Contents'First + 0..Contents'Last;

      subtype Contents_Index_T is Integer range Contents'First..Contents'Last;

      CP : Aida.UTF8_Code_Point.T;

      Prev_P : Prev_P_T := P;
      Prev_Prev_P : Prev_Prev_P_T;-- := Prev_P;

      Start_Tag_Name_First_Index : Contents_Index_T := Prev_P;
      Start_Tag_Name_Last_Index  : Contents_Index_T := Prev_P;

      Tag_Value_First_Index : Contents_Index_T := Contents'First;
      Tag_Value_Last_Index  : Contents_Index_T := Contents'First;

      End_Tag_Name_First_Index : Contents_Index_T := Contents'First;
      End_Tag_Name_Last_Index  : Contents_Index_T;

      Attribute_First_Index : Contents_Index_T := Prev_P;
      Attribute_Last_Index  : Contents_Index_T := Prev_P;

      Attribute_Value_First_Index : Contents_Index_T := Prev_P;
      Attribute_Value_Last_Index  : Contents_Index_T;

      Comment_First_Index : Contents_Index_T := Prev_P;

      --                  Shall_Ignore_Tag_Value : Boolean := False;
      --                  Shall_Ignore_Tag_Value : Boolean;

      --                    Shall_Ignore_Until_Next_Quotation_Mark : Boolean := False;

      Expected_Quotation_Symbol : Expected_Quotation_Symbol_T := Double_Quotes;
   begin
      if Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => String (Contents),
                                             Pointer => P)
      then
         Aida.UTF8.Get (Source  => String (Contents),
                        Pointer => P,
                        Value   => CP);

         if CP = Character'Pos ('>') then
            while P <= Contents'Last loop
               Prev_Prev_P := Prev_P;

               Prev_P := P;

               if not Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => String (Contents),
                                                          Pointer => P)
               then
                  Call_Result.Initialize (1434797854, -0068724898);
                  exit;
               end if;

               Aida.UTF8.Get (Source  => String (Contents),
                              Pointer => P,
                              Value   => CP);

               pragma Loop_Variant (Increases => P);
               pragma Loop_Invariant (not Call_Result.Has_Failed);
               pragma Loop_Invariant (P <= Contents'Last + 4);
               pragma Loop_Invariant (Prev_Prev_P < Prev_P and Prev_P < P);
               pragma Loop_Invariant (State_Id /= Extracting_Attribute_Name or
                                        (State_Id = Extracting_Attribute_Name and then (Attribute_First_Index < P)));
--                 pragma Loop_Invariant (State_Id /= Expecting_G_Sign_Or_Extracting_Attributes or
--                                          (State_Id = Expecting_G_Sign_Or_Extracting_Attributes and then (Start_Tag_Name_Last_Index < P)));
--                 pragma Loop_Invariant (State_Id /= Extracting_Attribute_Value or
--                                          (State_Id = Extracting_Attribute_Value and then (Attribute_Last_Index < P and Attribute_Value_First_Index < P)));
--                 pragma Loop_Invariant (State_Id /= Init_Extracting_Comment or
--                                          (State_Id = Init_Extracting_Comment and then (Comment_First_Index < P)));
--                 pragma Loop_Invariant (State_Id /= Init_Extracting_Comment_And_Found_Dash_Dash or
--                                          (State_Id = Init_Extracting_Comment_And_Found_Dash_Dash and then (Comment_First_Index < P)));
--                 pragma Loop_Invariant (State_Id /= Extracting_Comment_And_Found_Dash_Dash or
--                                          (State_Id = Extracting_Comment_And_Found_Dash_Dash and then (Comment_First_Index < P)));

               --                       Aida.Text_IO.Put ("Extracted:");
               --                       Aida.Text_IO.Put (Image (CP));
               --                       Aida.Text_IO.Put (", state ");
               --                       Aida.Text_IO.Put_Line (String_T (State_Id_Type'Image (State_Id)));
               --                       Aida.Text_IO.Put (Image (CP));

               case State_Id is
                  when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
                     if
                       CP = Character'Pos (' ') or
                       CP = Character'Pos (Ada.Characters.Latin_1.LF) or
                       CP = Character'Pos (Ada.Characters.Latin_1.CR) or
                       CP = Character'Pos (Ada.Characters.Latin_1.HT)
                     then
                        null; -- Normal
                     elsif CP = Character'Pos ('<') then
                        State_Id := Init_Found_Less_Sign;
                     else
                        Call_Result.Initialize (1003548980, 1714289304);
                        exit;
                     end if;
                  when Init_Found_Less_Sign =>
                     if CP = Character'Pos ('!') then
                        State_Id := Init_Found_Less_Followed_By_Exclamation_Sign;
                     elsif CP = Character'Pos ('/') then
                        if Depth = 0 then
                           Call_Result.Initialize (-1797161339, -1801650669);
                           exit;
                        end if;

                        if P > Contents'Last then
                           Call_Result.Initialize (0386434633, -1112825058);
                           exit;
                        end if;

                        Text (Arg1,
                              Arg2,
                              Arg3,
                              Arg4,
                              "",
                              Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        State_Id := Extracting_End_Tag_Name;
                        End_Tag_Name_First_Index := P;
                     elsif not Is_Special_Symbol (CP) then
                        State_Id := Extracting_Start_Tag_Name;
                        Start_Tag_Name_First_Index := Prev_P;

                        pragma Assert (Start_Tag_Name_First_Index < P);
                     else
                        Call_Result.Initialize (1448645964, 0183871387);
                        exit;
                     end if;
                  when Init_Found_Less_Followed_By_Exclamation_Sign =>
                     if CP = Character'Pos ('-') then
                        State_Id := Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign;
                     else
                        Call_Result.Initialize (1915807131, 1377704704);
                        exit;
                     end if;
                  when Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
                     if CP = Character'Pos ('-') then
                        State_Id := Init_Extracting_Comment;

                        Comment_First_Index := (if P <= Contents'Last then
                                                   P
                                                else
                                                   Contents'Last);
                     else
                        Call_Result.Initialize (-1302785225, -0551230956);
                        exit;
                     end if;
                  when Extracting_Start_Tag_Name =>
                     if CP = Character'Pos (' ') then
                        Start_Tag_Name_Last_Index := Prev_Prev_P;

                        Start_Tag (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                   Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth < Aida.Int32_T'Last then
                           Depth := Depth + 1;
                        else
                           Call_Result.Initialize (-0197127393, -1788002976);
                           exit;
                        end if;

                        State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
                     elsif CP = Character'Pos ('>') then
                        Start_Tag_Name_Last_Index := Prev_Prev_P;

                        Start_Tag (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                   Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth < Aida.Int32_T'Last then
                           Depth := Depth + 1;
                        else
                           Call_Result.Initialize (0133265230, -0905163379);
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);

                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (-0192291225, -1709997324);
                        exit;
                     end if;
                  when Expecting_G_Sign_Or_Extracting_Attributes =>
                     if
                       CP = Character'Pos (' ') or
                       CP = Character'Pos (Ada.Characters.Latin_1.LF) or
                       CP = Character'Pos (Ada.Characters.Latin_1.CR) or
                       CP = Character'Pos (Ada.Characters.Latin_1.HT)
                     then
                        null; -- Normal
                     elsif CP = Character'Pos ('>') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;

                        if P > Contents'Last then
                           Call_Result.Initialize (-1521899768, -0725554341);
                           exit;
                        end if;

                        Tag_Value_First_Index := P;
                     elsif CP = Character'Pos ('/') then
                        State_Id := Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash;
                     elsif not Is_Special_Symbol (CP) then
                        Attribute_First_Index := Prev_P;
                        State_Id := Extracting_Attribute_Name;
                     else
                        Call_Result.Initialize (-0429878843, 1344381718);
                        exit;
                     end if;
                  when Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash =>
                     if CP = Character'Pos ('>') then
                        State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

                        Text (Arg1,
                              Arg2,
                              Arg3,
                              Arg4,
                              "",
                              Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        --                              End_Tag_Name_Last_Index := Prev_Prev_P;

                        End_Tag (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                 Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth > 0 then
                           Depth := Depth - 1;
                        else
                           Call_Result.Initialize (0766893447, -0197942014);
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                     else
                        Call_Result.Initialize (1180086532, 1745903660);
                        exit;
                     end if;
                  when Extracting_Attribute_Name =>
                     if CP = Character'Pos ('=') then
                        Attribute_Last_Index := Prev_Prev_P;
                        State_Id := Expecting_Attribute_Value_Quotation_Mark;
                     elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) or CP = Character'Pos (Ada.Characters.Latin_1.CR) then
                        Call_Result.Initialize (-0986469701, -0000005525);
                        exit;
                     elsif not Is_Special_Symbol (CP) then
                        null; -- Normal
                     else
                        Call_Result.Initialize (0819713752, 1428867079);
                        exit;
                     end if;
                  when Expecting_Attribute_Value_Quotation_Mark =>
                     if CP = Character'Pos ('"') then
                        Expected_Quotation_Symbol := Double_Quotes;

                        Attribute_Value_First_Index := (if P <= Contents'Last then
                                                           P
                                                        else
                                                           Contents'Last);
                        State_Id := Extracting_Attribute_Value;
                     elsif CP = Character'Pos (''') then
                        Expected_Quotation_Symbol := Single_Quotes;
                        Attribute_Value_First_Index := (if P <= Contents'Last then
                                                           P
                                                        else
                                                           Contents'Last);
                        State_Id := Extracting_Attribute_Value;
                     else
                        Call_Result.Initialize (0240833721, 0455771309);
                        exit;
                     end if;
                  when Extracting_Attribute_Value =>
                     if
                       (CP = Character'Pos ('"') and Expected_Quotation_Symbol = Double_Quotes) or
                       (CP = Character'Pos (''') and Expected_Quotation_Symbol = Single_Quotes)
                     then
                        Attribute_Value_Last_Index := Prev_Prev_P;
                        State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
                        declare
                           Name : Aida.String_T := Contents (Attribute_First_Index..Attribute_Last_Index);
                           Value : Aida.String_T := Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index);
                        begin
                           Attribute (Arg1,
                                      Arg2,
                                      Arg3,
                                      Arg4,
                                      Name,
                                      Value,
                                      Call_Result);
                        end;

                        if Call_Result.Has_Failed then
                           exit;
                        end if;
                     elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) or CP = Character'Pos (Ada.Characters.Latin_1.CR) then
                        Call_Result.Initialize (0587945467, 1683764896);
                        exit;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value =>
                     --                    if CP = Character'Pos ('"') then
                     --                       Shall_Ignore_Until_Next_Quotation_Mark := not Shall_Ignore_Until_Next_Quotation_Mark;
                     if CP = Character'Pos ('<') then
                        --                     if not Shall_Ignore_Until_Next_Quotation_Mark then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L;
                        Tag_Value_Last_Index := Prev_Prev_P;

                        Text (Arg1,
                              Arg2,
                              Arg3,
                              Arg4,
                              Contents (Tag_Value_First_Index..Tag_Value_Last_Index),
                              Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        --                     end if;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L =>
                     if CP = Character'Pos ('/') then
                        if P > Contents'Last then
                           Call_Result.Initialize (-1635958681, 2091153567);
                           exit;
                        end if;

                        State_Id := Extracting_End_Tag_Name;

                        End_Tag_Name_First_Index := P;
                     elsif CP = Character'Pos ('!') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (-0115323975, -1084437773);
                        exit;
                     else
                        -- Will start parsing child tag!
                        State_Id := Extracting_Start_Tag_Name;
                        Start_Tag_Name_First_Index := Prev_P;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation =>
                     if CP = Character'Pos ('[') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C;
                     elsif CP = Character'Pos ('-') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C =>
                     if CP = Character'Pos ('C') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD =>
                     if CP = Character'Pos ('D') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA =>
                     if CP = Character'Pos ('A') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT =>
                     if CP = Character'Pos ('T') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA =>
                     if CP = Character'Pos ('A') then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket =>
                     if CP = Character'Pos ('[') then
                        State_Id := Extracting_CDATA;
                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Extracting_CDATA =>
                     if CP = Character'Pos (']') then
                        Tag_Value_Last_Index := Prev_Prev_P;
                        State_Id := Extracting_CDATA_Found_Square_Bracket;
                     end if;
                  when Extracting_CDATA_Found_Square_Bracket =>
                     if CP = Character'Pos (']') then
                        State_Id := Extracting_CDATA_Found_Two_Square_Brackets;
                     else
                        State_Id := Extracting_CDATA;
                     end if;
                  when Extracting_CDATA_Found_Two_Square_Brackets =>
                     if CP = Character'Pos ('>') then
                        CDATA (Arg1,
                               Arg2,
                               Arg3,
                               Arg4,
                               Contents (Tag_Value_First_Index..Tag_Value_Last_Index),
                               Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     else
                        State_Id := Extracting_CDATA;
                     end if;
                  when Extracting_End_Tag_Name =>
                     if CP = Character'Pos ('>') then

                        End_Tag_Name_Last_Index := Prev_Prev_P;

                        End_Tag (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index),
                                 Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth > 0 then
                           Depth := Depth - 1;
                        else
                           Call_Result.Initialize (-0534201701, -0614895498);
                           exit;
                        end if;

                        if Depth = 0 then
                           State_Id := Expecting_Only_Trailing_Spaces;
                        else
                           State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);

                     elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) or CP = Character'Pos (Ada.Characters.Latin_1.CR) then
                        Call_Result.Initialize (-1658791000, 1638125646);
                        exit;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (1726646144, -0779212513);
                        exit;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash =>
                     if CP = Character'Pos ('-') then
                        Comment_First_Index := (if P <= Contents'Last then
                                                   P
                                                else
                                                   Contents'Last);
                        State_Id := Extracting_Comment;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Init_Extracting_Comment =>
                     if CP = Character'Pos ('-') then
                        State_Id := Init_Extracting_Comment_And_Found_Dash;
                     end if;
                  when Init_Extracting_Comment_And_Found_Dash =>
                     if CP = Character'Pos ('-') then
                        State_Id := Init_Extracting_Comment_And_Found_Dash_Dash;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Init_Extracting_Comment_And_Found_Dash_Dash =>
                     if CP = Character'Pos ('>') then
                        Comment (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Value       => Contents (Comment_First_Index..(P - 4)),
                                 Call_Result => Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                        State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Extracting_Comment =>
                     if CP = Character'Pos ('-') then
                        State_Id := Extracting_Comment_And_Found_Dash;
                     end if;
                  when Extracting_Comment_And_Found_Dash =>
                     if CP = Character'Pos ('-') then
                        State_Id := Extracting_Comment_And_Found_Dash_Dash;
                     else
                        State_Id := Extracting_Comment;
                     end if;
                  when Extracting_Comment_And_Found_Dash_Dash =>
                     if CP = Character'Pos ('>') then
                        Comment (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Value       => Contents (Comment_First_Index..(P - 4)),
                                 Call_Result => Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Expecting_Only_Trailing_Spaces =>
                     if CP = Character'Pos (' ') or CP = 10 or CP = 13 then
                        null; -- Trailing spaces are OK
                     else
                        Call_Result.Initialize (1777504526, -1635825641);
                        exit;
                     end if;
               end case;
            end loop;

            if
            not Call_Result.Has_Failed and then
              State_Id /= Expecting_Only_Trailing_Spaces
            then
               Call_Result.Initialize (-1968500370, -1627762655);
            end if;
         else
            Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
         end if;
      else
         Call_Result.Initialize (-1672429119, -1233854200);
      end if;
   end Analyze_XML;

   Initial_State_Id : Initial_State_Id_T := Initial_State_Expecting_Less_Sign;

   P : P_T := Contents'First;

   CP : Aida.UTF8_Code_Point.T;
begin
   while P <= Contents'Last loop
      exit when Initial_State_Id = End_State;

      if not Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => String (Contents),
                                                 Pointer => P)
      then
         Call_Result.Initialize (-0356399774, -0280059910);
         exit;
      end if;

      Aida.UTF8.Get (Source  => String (Contents),
                     Pointer => P,
                     Value   => CP);

      pragma Loop_Variant (Increases => P);
      pragma Loop_Invariant (not Call_Result.Has_Failed);
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_Less_Sign or
                               (Initial_State_Id = Initial_State_Expecting_Less_Sign and then (P > Contents'First and Contents'Last >= Contents'First)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_Question_Mark or
                               (Initial_State_Id = Initial_State_Expecting_Question_Mark and then (P > Contents'First + 1 and Contents'Last >= Contents'First + 1)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_X or
                               (Initial_State_Id = Initial_State_Expecting_X and then (P > Contents'First + 2 and Contents'Last >= Contents'First + 2)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XM or
                               (Initial_State_Id = Initial_State_Expecting_XM and then (P > Contents'First + 3 and Contents'Last >= Contents'First + 3)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML or
                               (Initial_State_Id = Initial_State_Expecting_XML and then (P > Contents'First + 4 and Contents'Last >= Contents'First + 4)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S or
                               (Initial_State_Id = Initial_State_Expecting_XML_S and then (P > Contents'First + 5 and Contents'Last >= Contents'First + 5)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_V or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_V and then (P > Contents'First + 6 and Contents'Last >= Contents'First + 6)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VE or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VE and then (P > Contents'First + 7 and Contents'Last >= Contents'First + 7)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VER or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VER and then (P > Contents'First + 8 and Contents'Last >= Contents'First + 8)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERS or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERS and then (P > Contents'First + 9 and Contents'Last >= Contents'First + 9)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSI or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSI and then (P > Contents'First + 10 and Contents'Last >= Contents'First + 10)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSIO or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSIO and then (P > Contents'First + 11 and Contents'Last >= Contents'First + 11)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION and then (P > Contents'First + 12 and Contents'Last >= Contents'First + 12)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E and then (P > Contents'First + 13 and Contents'Last >= Contents'First + 13)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q and then (P > Contents'First + 14 and Contents'Last >= Contents'First + 14)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1 or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1 and then (P > Contents'First + 15 and Contents'Last >= Contents'First + 15)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P and then (P > Contents'First + 16 and Contents'Last >= Contents'First + 16)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0 or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0 and then (P > Contents'First + 17 and Contents'Last >= Contents'First + 17)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q and then (P > Contents'First + 18 and Contents'Last >= Contents'First + 18)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S and then (P > Contents'First + 19 and Contents'Last >= Contents'First + 19)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_E or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_E and then (P > Contents'First + 20 and Contents'Last >= Contents'First + 20)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_EN or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_EN and then (P > Contents'First + 21 and Contents'Last >= Contents'First + 21)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENC or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENC and then (P > Contents'First + 22 and Contents'Last >= Contents'First + 22)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO and then (P > Contents'First + 23 and Contents'Last >= Contents'First + 23)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD and then (P > Contents'First + 24 and Contents'Last >= Contents'First + 24)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI and then (P > Contents'First + 25 and Contents'Last >= Contents'First + 25)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN and then (P > Contents'First + 26 and Contents'Last >= Contents'First + 26)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING and then (P > Contents'First + 27 and Contents'Last >= Contents'First + 27)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E and then (P > Contents'First + 28 and Contents'Last >= Contents'First + 28)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q and then (P > Contents'First + 29 and Contents'Last >= Contents'First + 29)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U and then (P > Contents'First + 30 and Contents'Last >= Contents'First + 30)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT and then (P > Contents'First + 31 and Contents'Last >= Contents'First + 31)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF and then (P > Contents'First + 32 and Contents'Last >= Contents'First + 32)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D and then (P > Contents'First + 33 and Contents'Last >= Contents'First + 33)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8 or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8 and then (P > Contents'First + 34 and Contents'Last >= Contents'First + 34)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q and then (P > Contents'First + 35 and Contents'Last >= Contents'First + 35)));
      pragma Loop_Invariant (Initial_State_Id /= Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark or
                               (Initial_State_Id = Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark and then (P > Contents'First + 36 and Contents'Last >= Contents'First + 36)));

      --        Aida.Text_IO.Put ("Extracted:");
      --        Aida.Text_IO.Put (Image (CP));
      --        Aida.Text_IO.Put (", state ");
      --        Aida.Text_IO.Put_Line (String_T (Initial_State_Id_T'Image (Initial_State_Id)));
      --        Aida.Text_IO.Put (Image (CP));

      case Initial_State_Id is
         when End_State => null;
         when Initial_State_Expecting_Less_Sign =>
            if CP = Character'Pos (' ') then
               null;
            elsif CP = Character'Pos ('<') then
               Initial_State_Id := Initial_State_Expecting_Question_Mark;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_Question_Mark =>
            if CP = Character'Pos ('?') then
               Initial_State_Id := Initial_State_Expecting_X;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_X =>
            if CP = Character'Pos ('x') then
               Initial_State_Id := Initial_State_Expecting_XM;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XM =>
            if CP = Character'Pos ('m') then
               Initial_State_Id := Initial_State_Expecting_XML;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML =>
            if CP = Character'Pos ('l') then
               Initial_State_Id := Initial_State_Expecting_XML_S;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S =>
            if CP = Character'Pos (' ') then
               Initial_State_Id := Initial_State_Expecting_XML_S_V;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_V =>
            if CP = Character'Pos ('v') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VE;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VE =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VER;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VER =>
            if CP = Character'Pos ('r') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERS;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERS =>
            if CP = Character'Pos ('s') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSI;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSI =>
            if CP = Character'Pos ('i') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSIO;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSIO =>
            if CP = Character'Pos ('o') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E =>
            if CP = Character'Pos ('=') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1 =>
            if CP = Character'Pos ('1') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P =>
            if CP = Character'Pos ('.') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0 =>
            if CP = Character'Pos ('0') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S =>
            if CP = Character'Pos (' ') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_E;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_E =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_EN;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_EN =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENC;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENC =>
            if CP = Character'Pos ('c') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO =>
            if CP = Character'Pos ('o') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD =>
            if CP = Character'Pos ('d') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI =>
            if CP = Character'Pos ('i') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING =>
            if CP = Character'Pos ('g') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E =>
            if CP = Character'Pos ('=') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U =>
            if CP = Character'Pos ('u') or CP = Character'Pos ('U') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT =>
            if CP = Character'Pos ('t') or CP = Character'Pos ('T') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF =>
            if CP = Character'Pos ('f') or CP = Character'Pos ('F') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D =>
            if CP = Character'Pos ('-') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8 =>
            if CP = Character'Pos ('8') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark =>
            if CP = Character'Pos ('?') then
               if P <= Contents'Last then
                  Initial_State_Id := End_State;

                  pragma Assert (P > Contents'First + 36);

                  Analyze_XML (P);
               else
                  Call_Result.Initialize (-0645831530, 1132432555);
                  exit;
               end if;
            else
               Call_Result.Initialize (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
      end case;
   end loop;
end Aida.XML_SAX_Parse;
