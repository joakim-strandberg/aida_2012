with Aida.UTF8;
with Aida.UTF8_Code_Point;
with Ada.Characters.Latin_1;
with Aida.Text_IO;
with Aida.Bounded_Vector;

pragma Elaborate_All (Aida.Bounded_Vector);

procedure Aida.XML.Generic_Parse_XML_File (Arg1        : in out Arg1_T;
                                           Arg2        : in out Arg2_T;
                                           Arg3        : in out Arg3_T;
                                           Arg4        : in out Arg4_T;
                                           Contents    : Aida.String_T;
                                           Call_Result : in out Procedure_Call_Result.T)
is
   use all type Aida.String_T;
   use all type Aida.Int32_T;
   use all type Aida.UTF8_Code_Point.T;
   use all type Procedure_Call_Result.T;

   subtype P_T is Integer range Contents'First..Contents'Last + 4;

   Initial_State_Id : Initial_State_Id_T := Initial_State_Expecting_Less_Sign;

   P : P_T := Contents'First;

   F : Natural := Contents'First;

   CP : Aida.UTF8_Code_Point.T;
begin
   while P <= Contents'Last loop
      exit when Initial_State_Id = End_State;

      if not Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => String (Contents),
                                                 Pointer => P)
      then
         Initialize (Call_Result, "Found invalid UTF-8 character.");
         exit;
      end if;

      Aida.UTF8.Get (Source  => String (Contents),
                     Pointer => P,
                     Value   => CP);

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
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_Question_Mark =>
            if CP = Character'Pos ('?') then
               Initial_State_Id := Initial_State_Expecting_X;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_X =>
            if CP = Character'Pos ('x') then
               Initial_State_Id := Initial_State_Expecting_XM;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XM =>
            if CP = Character'Pos ('m') then
               Initial_State_Id := Initial_State_Expecting_XML;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML =>
            if CP = Character'Pos ('l') then
               Initial_State_Id := Initial_State_Expecting_XML_S;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S =>
            if CP = Character'Pos (' ') then
               Initial_State_Id := Initial_State_Expecting_XML_S_V;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_V =>
            if CP = Character'Pos ('v') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VE;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VE =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VER;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VER =>
            if CP = Character'Pos ('r') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERS;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERS =>
            if CP = Character'Pos ('s') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSI;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSI =>
            if CP = Character'Pos ('i') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSIO;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSIO =>
            if CP = Character'Pos ('o') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E =>
            if CP = Character'Pos ('=') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1 =>
            if CP = Character'Pos ('1') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P =>
            if CP = Character'Pos ('.') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0 =>
            if CP = Character'Pos ('0') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S =>
            if CP = Character'Pos (' ') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_E;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_E =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_EN;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_EN =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENC;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENC =>
            if CP = Character'Pos ('c') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO =>
            if CP = Character'Pos ('o') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD =>
            if CP = Character'Pos ('d') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI =>
            if CP = Character'Pos ('i') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING =>
            if CP = Character'Pos ('g') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E =>
            if CP = Character'Pos ('=') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U =>
            if CP = Character'Pos ('u') or CP = Character'Pos ('U') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT =>
            if CP = Character'Pos ('t') or CP = Character'Pos ('T') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF =>
            if CP = Character'Pos ('f') or CP = Character'Pos ('F') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D =>
            if CP = Character'Pos ('-') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8 =>
            if CP = Character'Pos ('8') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark =>
            if CP = Character'Pos ('?') then
               Initial_State_Id := Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark_Greater_Sign;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
         when Initial_State_Expecting_XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UTF_D_8_Q_Question_Mark_Greater_Sign =>
            if CP = Character'Pos ('>') then
               Initial_State_Id := End_State;

               declare
                  Depth : Aida.Nat32_T := 0;

                  State_Id : State_Id_Type := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

                  subtype Prev_P_T      is Integer range Contents'First + 1..Contents'Last;
                  subtype Prev_Prev_P_T is Integer range Contents'First + 0..Contents'Last;

                  subtype Contents_Index_T is Integer range Contents'First..Contents'Last;

                  Prev_P      : Prev_P_T := P - 1;
                  Prev_Prev_P : Prev_Prev_P_T;-- := Prev_P;

                  Start_Tag_Name_First_Index : Contents_Index_T := Prev_P;
                  Start_Tag_Name_Last_Index  : Contents_Index_T;

                  Tag_Value_First_Index : Contents_Index_T := Contents'First;
                  Tag_Value_Last_Index  : Contents_Index_T := Contents'First;

                  End_Tag_Name_First_Index : Contents_Index_T := Contents'First;
                  End_Tag_Name_Last_Index  : Contents_Index_T;

                  Attribute_First_Index : Integer;
                  Attribute_Last_Index  : Integer;

                  Attribute_Value_First_Index : Integer;
                  Attribute_Value_Last_Index  : Integer;

                  Comment_First_Index : Integer;

                  Shall_Ignore_Tag_Value : Boolean := False;

                  --                    Shall_Ignore_Until_Next_Quotation_Mark : Boolean := False;

                  Expected_Quotation_Symbol : Expected_Quotation_Symbol_T := Double_Quotes;
               begin
                  while P <= Contents'Last loop
                     Prev_Prev_P := Prev_P;

                     Prev_P := P;

                     if not Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => String (Contents),
                                                                Pointer => P)
                     then
                        Initialize (Call_Result, "Found invalid UTF-8 character.");
                        exit;
                     end if;

                     Aida.UTF8.Get (Source  => String (Contents),
                                    Pointer => P,
                                    Value   => CP);

                     pragma Loop_Variant (Increases => P);
                     pragma Loop_Invariant (not Has_Failed (Call_Result));
                     pragma Loop_Invariant (P <= Contents'Last + 4);
                     pragma Loop_Invariant (Prev_Prev_P < Prev_P and Prev_P < P);
                     --              pragma Loop_Invariant (Aida.Nat32_T (Length (Tag_Ids)) = Aida.Nat32_T(Length (Shall_Ignore_Tag_Value_List)));
                     --              pragma Loop_Invariant (State_Id /= Expecting_NL_Sign_Or_Space_Or_Less_Sign or
                     --                                       (State_Id = Expecting_NL_Sign_Or_Space_Or_Less_Sign and then (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));
                     --              pragma Loop_Invariant (State_Id /= Found_Less_Sign or
                     --                                       (State_Id = Found_Less_Sign and then (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));
                     --              pragma Loop_Invariant (State_Id /= Extracting_Root_Start_Tag_Name or
                     --                                       (State_Id = Extracting_Root_Start_Tag_Name and then (Start_Tag_Name_First_Index <= Prev_Prev_P and Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));
                     --              pragma Loop_Invariant (State_Id /= Extracting_Start_Tag_Name or
                     --                                       (State_Id = Extracting_Start_Tag_Name and then (Start_Tag_Name_First_Index <= Prev_Prev_P and Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0)));
                     --              pragma Loop_Invariant (State_Id /= Extracting_End_Tag_Name or
                     --                                       (State_Id = Extracting_End_Tag_Name and then (Length (Tag_Ids) > 1 and Length (Shall_Ignore_Tag_Value_List) > 1)));
                     --              pragma Loop_Invariant (State_Id /= Extracting_Root_End_Tag_Name or
                     --                                       (State_Id = Extracting_Root_End_Tag_Name and then (Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1)));
                     --              pragma Loop_Invariant (State_Id /= Expecting_New_Tag_Or_Extracting_Tag_Value or
                     --                                       (State_Id = Expecting_New_Tag_Or_Extracting_Tag_Value and then (Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0)));
                     --              pragma Loop_Invariant (State_Id /= Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L or
                     --                                       (State_Id = Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L and then (Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0)));
                     --              pragma Loop_Invariant (State_Id /= Expecting_G_Sign_Or_Extracting_Attributes_For_Root or
                     --                                       (State_Id = Expecting_G_Sign_Or_Extracting_Attributes_For_Root and then (Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1)));
                     --              pragma Loop_Invariant (State_Id /= Expecting_G_Sign_Or_Extracting_Attributes or
                     --                                       (State_Id = Expecting_G_Sign_Or_Extracting_Attributes and then (Length (Tag_Ids) > 1 and Length (Shall_Ignore_Tag_Value_List) > 1)));
                     --              pragma Loop_Invariant (State_Id /= Expecting_Only_Trailing_Spaces or
                     --                                       (State_Id = Expecting_Only_Trailing_Spaces and then (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));



                     --                       Aida.Text_IO.Put ("Extracted:");
                     --                       Aida.Text_IO.Put (Image (CP));
                     --                       Aida.Text_IO.Put (", state ");
                     --                       Aida.Text_IO.Put_Line (String_T (State_Id_Type'Image (State_Id)));
                     --                       Aida.Text_IO.Put (Image (CP));

                     case State_Id is
                        when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
                           if CP = Character'Pos (Ada.Characters.Latin_1.LF) or CP = Character'Pos (' ') then
                              null; -- Normal
                           elsif CP = Character'Pos ('<') then
                              State_Id := Init_Found_Less_Sign;
                           else
                              Initialize (Call_Result, "Unexpected UTF8 symbol");
                              return;
                           end if;
                        when Init_Found_Less_Sign =>
                           if CP = Character'Pos ('!') then
                              State_Id := Init_Found_Less_Followed_By_Exclamation_Sign;
                           elsif CP = Character'Pos ('/') then
                              if Depth = 0 then
                                 Initialize (Call_Result, "5D33399A-94A5-4352-83A4-873C8303AECB");
                                 exit;
                              end if;

                              if P > Contents'Last then
                                 Initialize (Call_Result, "7FB4B266-4657-4185-8928-2DBBE3E5D16F");
                                 exit;
                              end if;

                              Text (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    "",
                                    Call_Result);

                              if Has_Failed (Call_Result) then
                                 return;
                              end if;

                              State_Id := Extracting_End_Tag_Name;
                              End_Tag_Name_First_Index := P;
                           elsif not Is_Special_Symbol (CP) then
                              State_Id := Extracting_Start_Tag_Name;
                              Start_Tag_Name_First_Index := Prev_P;

                              pragma Assert (Start_Tag_Name_First_Index < P);
                           else
                              Initialize (Call_Result, "Unexpected UTF8 symbol (code point ), state ");
                              exit;
                           end if;
                        when Init_Found_Less_Followed_By_Exclamation_Sign =>
                           if CP = Character'Pos ('-') then
                              State_Id := Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign;
                           else
                              Initialize (Call_Result, "Unexpected UTF8 symbol");
                              return;
                           end if;
                        when Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
                           if CP = Character'Pos ('-') then
                              State_Id := Init_Extracting_Comment;
                              Comment_First_Index := P;
                           else
                              Initialize (Call_Result, "Unexpected UTF8 symbol");
                              return;
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

                              if Has_Failed (Call_Result) then
                                 exit;
                              end if;

                              Depth := Depth + 1;

                              State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
                           elsif CP = Character'Pos ('>') then
                              Start_Tag_Name_Last_Index := Prev_Prev_P;

                              Start_Tag (Arg1,
                                         Arg2,
                                         Arg3,
                                         Arg4,
                                         Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                         Call_Result);

                              if Has_Failed (Call_Result) then
                                 exit;
                              end if;

                              Depth := Depth + 1;

                              Tag_Value_First_Index := P;

                              State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                           elsif Is_Special_Symbol (CP) then
                              Initialize (Call_Result, "Unexpected UTF8 symbol (code point");
                              exit;
                           end if;
                        when Expecting_G_Sign_Or_Extracting_Attributes =>
                           if CP = Character'Pos (' ') or CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                              null; -- Normal
                           elsif CP = Character'Pos ('>') then
                              State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;

                              if P > Contents'Last then
                                 Initialize (Call_Result, "Unexpected end of XML.");
                                 exit;
                              end if;

                              Tag_Value_First_Index := P;
                           elsif CP = Character'Pos ('/') then
                              State_Id := Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash;
                           elsif not Is_Special_Symbol (CP) then
                              Attribute_First_Index := Prev_P;
                              State_Id := Extracting_Attribute_Name;
                           else
                              Initialize (Call_Result, "05E6E7A0-4ADC-48AE-9230-8A69F003521D");
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

                              if Has_Failed (Call_Result) then
                                 return;
                              end if;

                              End_Tag (Arg1,
                                       Arg2,
                                       Arg3,
                                       Arg4,
                                       Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                       Call_Result);

                              if Has_Failed (Call_Result) then
                                 return;
                              end if;

                              Tag_Value_First_Index := P;
                           else
                              Initialize (Call_Result, "Expected '>', state ");
                              return;
                           end if;
                        when Extracting_Attribute_Name =>
                           if CP = Character'Pos ('=') then
                              Attribute_Last_Index := Prev_Prev_P;
                              State_Id := Expecting_Attribute_Value_Quotation_Mark;
                              --                  Ada.Text_IO.Put_Line ("Extracted attribute name: '" & Contents (Attribute_First_Index..Attribute_Last_Index) & "'");
                           elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                              Initialize (Call_Result, "New line is forbidden inside attribute name, state ");
                              return;
                           elsif not Is_Special_Symbol (CP) then
                              null; -- Normal
                           end if;
                        when Expecting_Attribute_Value_Quotation_Mark =>
                           if CP = Character'Pos ('"') then
                              Expected_Quotation_Symbol := Double_Quotes;
                              Attribute_Value_First_Index := P;
                              State_Id := Extracting_Attribute_Value;
                           elsif CP = Character'Pos (''') then
                              Expected_Quotation_Symbol := Single_Quotes;
                              Attribute_Value_First_Index := P;
                              State_Id := Extracting_Attribute_Value;
                           else
                              Initialize (Call_Result, "Unexpected UTF8 symbol");
                              return;
                           end if;
                        when Extracting_Attribute_Value =>
                           if
                             (CP = Character'Pos ('"') and Expected_Quotation_Symbol = Double_Quotes) or
                             (CP = Character'Pos (''') and Expected_Quotation_Symbol = Single_Quotes)
                           then
                              Attribute_Value_Last_Index := Prev_Prev_P;
                              State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
                              --                  Ada.Text_IO.Put_Line ("Extracted attribute value: '" & Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index) & "'");
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

                              if Has_Failed (Call_Result) then
                                 return;
                              end if;
                           elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                              Initialize (Call_Result, "New line is forbidden inside attribute value, state ");
                              return;
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

                              if Has_Failed (Call_Result) then
                                 exit;
                              end if;

                              --                     end if;
                           end if;
                        when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L =>
                           if CP = Character'Pos ('/') then
                              if P > Contents'Last then
                                 Initialize (Call_Result, "Unexpected end of XML.");
                                 exit;
                              end if;

                              State_Id := Extracting_End_Tag_Name;

                              End_Tag_Name_First_Index := P;
                           elsif CP = Character'Pos ('!') then
                              State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation;
                           elsif Is_Special_Symbol (CP) then
                              --                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                              Initialize (Call_Result, "Unexpected UTF8 symbol ");
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
                              Tag_Value_First_Index := P;
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
                              State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign;
                           else
                              State_Id := Extracting_CDATA;
                           end if;
                        when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign =>
                           if CP = Character'Pos ('<') then
                              State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign;
                           else
                              Initialize (Call_Result, "Expecting '<' followed by end tag but was something else, state ");
                              return;
                           end if;
                        when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign =>
                           if CP = Character'Pos ('/') then
                              State_Id := Extracting_End_Tag_Name;
                              End_Tag_Name_First_Index := P;
                           else
                              Initialize (Call_Result, "Expecting '/' followed by end tag but was something else, state ");
                              return;
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

                              if Has_Failed (Call_Result) then
                                 exit;
                              end if;

                              Depth := Depth - 1;

                              if Depth = 0 then
                                 State_Id := Expecting_Only_Trailing_Spaces;
                              else
                                 State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                              end if;

                              Tag_Value_First_Index := (if P <= Contents'Last then
                                                           P
                                                        else
                                                           Contents'Last);

                           elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                              --                     Initialize (Call_Result, "New line is forbidden inside attribute value, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                              Initialize (Call_Result, "New line is forbidden inside attribute value, state ");
                              exit;
                           elsif Is_Special_Symbol (CP) then
                              --                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                              Initialize (Call_Result, "Unexpected UTF8 symbol (code point");
                              exit;
                           end if;
                        when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash =>
                           if CP = Character'Pos ('-') then
                              --                                declare
                              --                                   Value : String := Contents (Tag_Value_First_Index..(P - 5));
                              --                                begin
                              --                                   Text (Value       => Value,
                              --                                         Parent_Tags => Tag_Names,
                              --                                         Call_Result => Call_Result);
                              --                                end;
                              Comment_First_Index := P;
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

                              if Has_Failed (Call_Result) then
                                 exit;
                              end if;

                              Tag_Value_First_Index := P;
                              --                              State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
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

                              if Has_Failed (Call_Result) then
                                 exit;
                              end if;

                              Tag_Value_First_Index := P;
                              State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                           else
                              State_Id := Init_Extracting_Comment;
                           end if;
                        when Expecting_Only_Trailing_Spaces =>
                           if CP = Character'Pos (' ') then
                              null; -- Trailing spaces are OK
                           else
                              Initialize (Call_Result, "Unexpected symbol!");
                              exit;
                           end if;
                     end case;
                  end loop;
               end;
            else
               Initialize (Call_Result, XML_IDENTIFIER_ERROR);
               exit;
            end if;
      end case;
   end loop;
end Aida.XML.Generic_Parse_XML_File;
