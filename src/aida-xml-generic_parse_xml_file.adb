with Aida.UTF8;
with Aida.UTF8_Code_Point;
with Ada.Characters.Latin_1;
with Aida.Text_IO;
with Aida.Containers.Bounded_Vector;

pragma Elaborate_All (Aida.Containers.Bounded_Vector);

procedure Aida.XML.Generic_Parse_XML_File (Arg           : in out Arg_T;
                                           Contents      : Aida.String_T;
                                           Call_Result   : in out Procedure_Call_Result.T)
is
   use all type Aida.String_T;
   use all type Aida.Int32_T;
   use all type Aida.UTF8_Code_Point.T;
   use all type Procedure_Call_Result.T;

   Tag_Ids : Tag_Id_Vector.T;

   State_Id : State_Id_Type;

   XML_File_Start_String  : Aida.String_T := "<?xml version=""1.0"" encoding=""utf-8""?>";
   XML_File_Start_String2 : Aida.String_T := "<?xml version=""1.0"" encoding=""UTF-8""?>";

   F : Natural := Contents'First;
--   L : Natural;-- := Contents'Last;

   Shall_Ignore_Tag_Value_List : Boolean_Vector.T;

   use all type Tag_Id_Vector.T;
   use all type Tag_Id_Vector.Length_T;
   use all type Boolean_Vector.T;
   use all type Boolean_Vector.Length_T;

--     procedure Append (This : in out Tag_Name_Vectors.Vector;
--                       Item : String)
--     is
--        N : Mutable_Tag_Name.Mutable_T;
--     begin
--        Initialize (N, Item);
--        Tag_Name_Vectors.Append (This, Tag_Name.T (N));
--     end Append;

   Next_Tag_Id : Tag_Id_T := Tag_Id_T'First;
begin
   if
     Contents (F..F+XML_File_Start_String'Length-1) /= XML_File_Start_String and
     Contents (F..F+XML_File_Start_String'Length-1) /= XML_File_Start_String2
   then
      Initialize (This    => Call_Result,
                  Message => "File does not start with <?xml version...>");
   else
      State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

      pragma Assert (Contents'Last < Integer'Last - 4);

      declare
         subtype P_T           is Integer range Contents'First + 2..Contents'Last + 4;
         subtype Prev_P_T      is Integer range Contents'First + 1..Contents'Last;
         subtype Prev_Prev_P_T is Integer range Contents'First + 0..Contents'Last;

         subtype Contents_Index_T is Integer range Contents'First..Contents'Last;

         P           : P_T := Contents'First + XML_File_Start_String'Length;
         Prev_P      : Prev_P_T := P - 1;
         Prev_Prev_P : Prev_Prev_P_T;-- := Prev_P;

         CP      : Aida.UTF8_Code_Point.T;

         Start_Tag_Name_First_Index : Contents_Index_T := Prev_P;
         Start_Tag_Name_Last_Index  : Contents_Index_T;

         Tag_Value_First_Index : Contents_Index_T := Contents'First;
         Tag_Value_Last_Index  : Contents_Index_T := Contents'First;

         End_Tag_Name_First_Index : Contents_Index_T := Contents'First;
         End_Tag_Name_Last_Index  : Contents_Index_T;

--           Attribute_First_Index : Integer;
--           Attribute_Last_Index  : Integer;
--
--           Attribute_Value_First_Index : Integer;
--           Attribute_Value_Last_Index  : Integer;
--
--           Comment_First_Index : Integer;

         Shall_Ignore_Tag_Value : Boolean := False;

--         Shall_Ignore_Until_Next_Quotation_Mark : Boolean := False;

--         Expected_Quotation_Symbol : Expected_Quotation_Symbol_T := Double_Quotes;
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
            pragma Loop_Invariant (Aida.Nat32_T (Length (Tag_Ids)) = Aida.Nat32_T(Length (Shall_Ignore_Tag_Value_List)));
            pragma Loop_Invariant (State_Id /= Expecting_NL_Sign_Or_Space_Or_Less_Sign or
                                     (State_Id = Expecting_NL_Sign_Or_Space_Or_Less_Sign and then (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));
            pragma Loop_Invariant (State_Id /= Found_Less_Sign or
                                     (State_Id = Found_Less_Sign and then (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));
            pragma Loop_Invariant (State_Id /= Extracting_Root_Start_Tag_Name or
                                     (State_Id = Extracting_Root_Start_Tag_Name and then (Start_Tag_Name_First_Index <= Prev_Prev_P and Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));
            pragma Loop_Invariant (State_Id /= Extracting_Start_Tag_Name or
                                     (State_Id = Extracting_Start_Tag_Name and then (Start_Tag_Name_First_Index <= Prev_Prev_P and Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0)));
            pragma Loop_Invariant (State_Id /= Extracting_End_Tag_Name or
                                     (State_Id = Extracting_End_Tag_Name and then (Length (Tag_Ids) > 1 and Length (Shall_Ignore_Tag_Value_List) > 1)));
            pragma Loop_Invariant (State_Id /= Extracting_Root_End_Tag_Name or
                                     (State_Id = Extracting_Root_End_Tag_Name and then (Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1)));
            pragma Loop_Invariant (State_Id /= Expecting_New_Tag_Or_Extracting_Tag_Value or
                                     (State_Id = Expecting_New_Tag_Or_Extracting_Tag_Value and then (Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0)));
            pragma Loop_Invariant (State_Id /= Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L or
                                     (State_Id = Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L and then (Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0)));
            pragma Loop_Invariant (State_Id /= Expecting_G_Sign_Or_Extracting_Attributes_For_Root or
                                     (State_Id = Expecting_G_Sign_Or_Extracting_Attributes_For_Root and then (Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1)));
            pragma Loop_Invariant (State_Id /= Expecting_G_Sign_Or_Extracting_Attributes or
                                     (State_Id = Expecting_G_Sign_Or_Extracting_Attributes and then (Length (Tag_Ids) > 1 and Length (Shall_Ignore_Tag_Value_List) > 1)));
            pragma Loop_Invariant (State_Id /= Expecting_Only_Trailing_Spaces or
                                     (State_Id = Expecting_Only_Trailing_Spaces and then (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0)));




--              Aida.Text_IO.Put_Line ("Extracted:" & Image (CP) & ", state " & State_Id_Type'Image (State_Id));
--              Aida.Text_IO.Put (Image (CP));

            case State_Id is
               when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
                  --                 if CP = Character'Pos (Ada.Characters.Latin_1.LF) or CP = Character'Pos (' ') then
                  --                    null; -- Normal
                  --                 elsif CP = Character'Pos ('<') then
                  --                    State_Id := Found_Less_Sign;
                  --                 else
                  --                    Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
                  if CP = Character'Pos ('<') then
                     State_Id := Found_Less_Sign;
                  end if;
               when Found_Less_Sign =>
                  --                 if CP = Character'Pos ('!') then
                  --                    State_Id := Found_Less_Followed_By_Exclamation_Sign;
                  if CP = Character'Pos ('/') then
--                       if Length (Tag_Ids) = 0 or Length (Shall_Ignore_Tag_Value_List) = 0 then
                        Initialize (Call_Result, "Found unexpected symbol '/'! Cannot continue parsing XML.");
                        exit;
--                       end if;

--                       State_Id := Extracting_End_Tag_Name;
--                       End_Tag_Name_First_Index := P;
--                       Tag_Value_Last_Index := Prev_Prev_P - 1;
                  elsif not Is_Special_Symbol (CP) then
                     State_Id := Extracting_Root_Start_Tag_Name;
                     Start_Tag_Name_First_Index := Prev_P;

                     pragma Assert (Start_Tag_Name_First_Index < P);
                  else
                     --                    Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point ), state ");
                     exit;
                  end if;
                  --              when Found_Less_Followed_By_Exclamation_Sign =>
                  --                 if CP = Character'Pos ('-') then
                  --                    State_Id := Found_Less_Followed_By_Exclamation_And_Dash_Sign;
                  --                 else
                  --                    Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
                  --              when Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
                  --                 if CP = Character'Pos ('-') then
                  --                    State_Id := Extracting_Comment;
                  --                 else
                  --                    Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
               when Extracting_Root_Start_Tag_Name =>
                  if CP = Character'Pos (' ') then
                     if Next_Tag_Id = Tag_Id_T'Last then
                        Initialize (Call_Result, "Too many tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     if Length (Tag_Ids) = MAX_DEPTH or Length (Shall_Ignore_Tag_Value_List) = MAX_DEPTH then
                        Initialize (Call_Result, "Too many nested tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     Start_Tag_Name_Last_Index := Prev_Prev_P;

                     Root_Start_Tag (Arg,
                                     Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                     Next_Tag_Id,
                                     Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     declare
                        Tag : Tag_T := (Id => Next_Tag_Id,
                                        Name_First_Index => Start_Tag_Name_First_Index,
                                        Name_Last_Index  => Start_Tag_Name_Last_Index);
                     begin
                        Append (Tag_Ids, Tag);
                     end;

                     Next_Tag_Id := Next_Tag_Id + 1;

                     Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);

                     Shall_Ignore_Tag_Value := False;

                     State_Id := Expecting_G_Sign_Or_Extracting_Attributes_For_Root;
                  elsif CP = Character'Pos ('>') then
                     if Next_Tag_Id = Tag_Id_T'Last then
                        Initialize (Call_Result, "Too many tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     if Length (Tag_Ids) = MAX_DEPTH or Length (Shall_Ignore_Tag_Value_List) = MAX_DEPTH then
                        Initialize (Call_Result, "Too many nested tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     if P > Contents'Last then
                        Initialize (Call_Result, "Unexpected end of XML.");
                        exit;
                     end if;

                     Start_Tag_Name_Last_Index := Prev_Prev_P;

                     Root_Start_Tag (Arg,
                                     Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                     Next_Tag_Id,
                                     Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     declare
                        Tag : Tag_T := (Id => Next_Tag_Id,
                                        Name_First_Index => Start_Tag_Name_First_Index,
                                        Name_Last_Index  => Start_Tag_Name_Last_Index);
                     begin
                        Append (Tag_Ids, Tag);
                     end;

                     Next_Tag_Id := Next_Tag_Id + 1;

                     Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);

                     Shall_Ignore_Tag_Value := False;
                     Tag_Value_First_Index := P;

                     pragma Assert (Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0);

                     State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  elsif Is_Special_Symbol (CP) then
--                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     exit;
                  end if;
               when Extracting_Start_Tag_Name =>
                  if CP = Character'Pos (' ') then
                     if Next_Tag_Id = Tag_Id_T'Last then
                        Initialize (Call_Result, "Too many tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     if Length (Tag_Ids) = MAX_DEPTH or Length (Shall_Ignore_Tag_Value_List) = MAX_DEPTH then
                        Initialize (Call_Result, "Too many nested tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     Start_Tag_Name_Last_Index := Prev_Prev_P;

                     if Length (Tag_Ids) = 0 then
                        Root_Start_Tag (Arg,
                                        Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                        Next_Tag_Id,
                                        Call_Result);

                     else
                        exit;
--                          Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
--                                     Tag_Names,
--                                     Call_Result);
                     end if;

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     declare
                        Tag : Tag_T := (Id => Next_Tag_Id,
                                        Name_First_Index => Start_Tag_Name_First_Index,
                                        Name_Last_Index  => Start_Tag_Name_Last_Index);
                     begin
                        Append (Tag_Ids, Tag);
                     end;

                     Next_Tag_Id := Next_Tag_Id + 1;

                     Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);

                     Shall_Ignore_Tag_Value := False;

                     State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
                  elsif CP = Character'Pos ('>') then
                     if Next_Tag_Id = Tag_Id_T'Last then
                        Initialize (Call_Result, "Too many tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     if Length (Tag_Ids) = MAX_DEPTH or Length (Shall_Ignore_Tag_Value_List) = MAX_DEPTH then
                        Initialize (Call_Result, "Too many nested tags! Cannot continue parsing XML.");
                        exit;
                     end if;

                     Start_Tag_Name_Last_Index := Prev_Prev_P;

                     if Length (Tag_Ids) = 0 then
                        Root_Start_Tag (Arg,
                                        Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                                        Next_Tag_Id,
                                        Call_Result);

                     else
                        exit;
--                       Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
--                                  Tag_Names,
--                                  Call_Result);
                     end if;

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     declare
                        Tag : Tag_T := (Id => Next_Tag_Id,
                                        Name_First_Index => Start_Tag_Name_First_Index,
                                        Name_Last_Index  => Start_Tag_Name_Last_Index);
                     begin
                        Append (Tag_Ids, Tag);
                     end;

                     Next_Tag_Id := Next_Tag_Id + 1;

                     Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);

                     Shall_Ignore_Tag_Value := False;
                     Tag_Value_First_Index := P;

                     pragma Assert (Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0);

                     State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  elsif Is_Special_Symbol (CP) then
--                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     exit;
                  end if;
               when Expecting_G_Sign_Or_Extracting_Attributes_For_Root =>
                  if CP = Character'Pos (' ') or CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                     null; -- Normal
                  elsif CP = Character'Pos ('>') then
                     State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;

                     if P > Contents'Last then
                        Initialize (Call_Result, "Unexpected end of XML.");
                        exit;
                     end if;

                     Tag_Value_First_Index := P;
--                    elsif CP = Character'Pos ('/') then
--                       State_Id := Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash;
--                    elsif not Is_Special_Symbol (CP) then
--                       Attribute_First_Index := Prev_P;
--                       State_Id := Extracting_Attribute_Name;
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
--                    elsif CP = Character'Pos ('/') then
--                       State_Id := Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash;
--                    elsif not Is_Special_Symbol (CP) then
--                       Attribute_First_Index := Prev_P;
--                       State_Id := Extracting_Attribute_Name;
                  end if;
                  --              when Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash =>
                  --                 if CP = Character'Pos ('>') then
                  --                    State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
                  --
                  --                    declare
                  --                       Name : String := Tag_Name.To_String (Tag_Name_Vectors.Last_Element (Tag_Names));
                  --                    begin
                  --                       Tag_Name_Vectors.Delete_Last (Tag_Names);
                  --
                  --                       End_Tag (Name,
                  --                                Tag_Names,
                  --                                Call_Result);
                  --                       if Has_Failed (Call_Result) then
                  --                          return;
                  --                       end if;
                  --                    end;
                  --                    Tag_Value_First_Index := P;
                  --                    Shall_Ignore_Tag_Value := True;
                  --                 else
                  --                    Initialize (Call_Result, "Expected '>', state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
--                 when Extracting_Attribute_Name =>
--                    if CP = Character'Pos ('=') then
--                       Attribute_Last_Index := Prev_Prev_P;
--                       State_Id := Expecting_Attribute_Value_Quotation_Mark;
--                       --                  Ada.Text_IO.Put_Line ("Extracted attribute name: '" & Contents (Attribute_First_Index..Attribute_Last_Index) & "'");
--                    elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
--                       Initialize (Call_Result, "New line is forbidden inside attribute name, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
--                       return;
--                    elsif not Is_Special_Symbol (CP) then
--                       null; -- Normal
--                    end if;
                  --              when Expecting_Attribute_Value_Quotation_Mark =>
                  --                 if CP = Character'Pos ('"') then
                  --                    Expected_Quotation_Symbol := Double_Quotes;
                  --                    Attribute_Value_First_Index := P;
                  --                    State_Id := Extracting_Attribute_Value;
                  --                 elsif CP = Character'Pos (''') then
                  --                    Expected_Quotation_Symbol := Single_Quotes;
                  --                    Attribute_Value_First_Index := P;
                  --                    State_Id := Extracting_Attribute_Value;
                  --                 else
                  --                    Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
                  --              when Extracting_Attribute_Value =>
                  --                 if
                  --                   (CP = Character'Pos ('"') and Expected_Quotation_Symbol = Double_Quotes) or
                  --                   (CP = Character'Pos (''') and Expected_Quotation_Symbol = Single_Quotes)
                  --                 then
                  --                    Attribute_Value_Last_Index := Prev_Prev_P;
                  --                    State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
                  --                    --                  Ada.Text_IO.Put_Line ("Extracted attribute value: '" & Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index) & "'");
                  --                    declare
                  --                       Name : String := Contents (Attribute_First_Index..Attribute_Last_Index);
                  --                       Value : String := Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index);
                  --                    begin
                  --                       Attribute (Name,
                  --                                  Value,
                  --                                  Tag_Names,
                  --                                  Call_Result);
                  --                    end;
                  --
                  --                    if Has_Failed (Call_Result) then
                  --                       return;
                  --                    end if;
                  --                 elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  --                    Initialize (Call_Result, "New line is forbidden inside attribute value, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
               when Expecting_New_Tag_Or_Extracting_Tag_Value =>
--                    if CP = Character'Pos ('"') then
--                       Shall_Ignore_Until_Next_Quotation_Mark := not Shall_Ignore_Until_Next_Quotation_Mark;
                  if CP = Character'Pos ('<') then
--                     if not Shall_Ignore_Until_Next_Quotation_Mark then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L;
                        Tag_Value_Last_Index := Prev_Prev_P;
--                     end if;
                  end if;
               when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L =>
--                    if CP = Character'Pos ('!') then
--                       State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation;
                  if CP = Character'Pos ('/') then
                     if P > Contents'Last then
                        Initialize (Call_Result, "Unexpected end of XML.");
                        exit;
                     end if;

                     if Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1 then
                        State_Id := Extracting_Root_End_Tag_Name;
                     else
                        pragma Assert (Length (Tag_Ids) > 1 and Length (Shall_Ignore_Tag_Value_List) > 1);

                        State_Id := Extracting_End_Tag_Name;
                     end if;

                     End_Tag_Name_First_Index := P;
                  elsif Is_Special_Symbol (CP) then
--                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     Initialize (Call_Result, "Unexpected UTF8 symbol ");
                     exit;
--                    else
--                       -- Will ignore tag value, and start parsing child tag!
--                       declare
--                          Value : String := Contents (Tag_Value_First_Index..(P - 3));
--                       begin
--                          Text (Value       => Value,
--                                Parent_Tags => Tag_Names,
--                                Call_Result => Call_Result);
--                       end;
--                       Shall_Ignore_Tag_Value := True;
--                       State_Id := Extracting_Start_Tag_Name;
--                       Start_Tag_Name_First_Index := Prev_P;
                  end if;
                  --              when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation =>
                  --                 if CP = Character'Pos ('[') then
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C;
                  --                 elsif CP = Character'Pos ('-') then
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash;
                  --                 else
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
                  --              when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C =>
                  --                 if CP = Character'Pos ('C') then
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD;
                  --                 else
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
                  --              when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD =>
                  --                 if CP = Character'Pos ('D') then
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA;
                  --                 else
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
                  --              when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA =>
                  --                 if CP = Character'Pos ('A') then
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT;
                  --                 else
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
                  --              when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT =>
                  --                 if CP = Character'Pos ('T') then
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA;
                  --                 else
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
                  --              when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA =>
                  --                 if CP = Character'Pos ('A') then
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket;
                  --                 else
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
                  --              when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket =>
                  --                 if CP = Character'Pos ('[') then
                  --                    State_Id := Extracting_CDATA;
                  --                    Tag_Value_First_Index := P;
                  --                 else
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
                  --              when Extracting_CDATA =>
                  --                 if CP = Character'Pos (']') then
                  --                    Tag_Value_Last_Index := Prev_Prev_P;
                  --                    State_Id := Extracting_CDATA_Found_Square_Bracket;
                  --                 end if;
                  --              when Extracting_CDATA_Found_Square_Bracket =>
                  --                 if CP = Character'Pos (']') then
                  --                    State_Id := Extracting_CDATA_Found_Two_Square_Brackets;
                  --                 else
                  --                    State_Id := Extracting_CDATA;
                  --                 end if;
                  --              when Extracting_CDATA_Found_Two_Square_Brackets =>
                  --                 if CP = Character'Pos ('>') then
                  --                    State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign;
                  --                 else
                  --                    State_Id := Extracting_CDATA;
                  --                 end if;
                  --              when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign =>
                  --                 if CP = Character'Pos ('<') then
                  --                    State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign;
                  --                 else
                  --                    Initialize (Call_Result, "Expecting '<' followed by end tag but was something else, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
                  --              when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign =>
                  --                 if CP = Character'Pos ('/') then
                  --                    State_Id := Extracting_End_Tag_Name;
                  --                    End_Tag_Name_First_Index := P;
                  --                 else
                  --                    Initialize (Call_Result, "Expecting '/' followed by end tag but was something else, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  --                    return;
                  --                 end if;
               when Extracting_Root_End_Tag_Name =>
                  pragma Assert (Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1);
                  if CP = Character'Pos ('>') then
--                       if P > Contents'Last then
--                          Initialize (Call_Result, "Unexpected end of XML.");
--                          exit;
--                       end if;

                     End_Tag_Name_Last_Index := Prev_Prev_P;

                     declare
                        Tag : constant Tag_T := Last_Element (Tag_Ids);

                        pragma Assume (Tag.Name_First_Index in Contents_Index_T);
                        pragma Assume (Tag.Name_Last_Index in Contents_Index_T);

                        Tag_Name : Aida.String_T := Contents (Tag.Name_First_Index..Tag.Name_Last_Index);
                        Value : Aida.String_T := (if Tag_Value_First_Index <= Tag_Value_Last_Index then
                                                          Contents (Tag_Value_First_Index..Tag_Value_Last_Index)
                                                        else
                                                           "");
                     begin
                        if Tag_Name /= Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) then
--                             Initialize (Call_Result, "Tag names does not match! Start tag is '" & Tag_Name &
--                                           "', and end tag is '" & Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) & "' state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                           Initialize (Call_Result, "Tag names does not match! Start tag is ");
                           exit;
                        end if;

                        if not Shall_Ignore_Tag_Value then
                           Delete_Last (Tag_Ids);

                           Root_End_Tag (Arg,
                                         Tag_Name,
                                         Value,
                                         Tag_Id_T'First,
                                         Call_Result);

                           if Has_Failed (Call_Result) then
                              exit;
                           end if;
                        else
--                             declare
--                                Text_Value : String := Contents (Tag_Value_First_Index..Tag_Value_Last_Index);
--                             begin
--                                Text (Value       => Text_Value,
--                                      Parent_Tags => Tag_Names,
--                                      Call_Result => Call_Result);
--
--                                if Has_Failed (Call_Result) then
--                                   return;
--                                end if;
--                             end;

                           Delete_Last (Tag_Ids);
--                             End_Tag (Tag_Name,
--                                      Tag_Names,
--                                      Call_Result);

                           if Has_Failed (Call_Result) then
                              exit;
                           end if;
                        end if;

--                        Tag_Value_First_Index := P; -- should not be set because end of XML
                        Shall_Ignore_Tag_Value := Last_Element (Shall_Ignore_Tag_Value_List);
                        Delete_Last (Shall_Ignore_Tag_Value_List);
                     end;

                     pragma Assert (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0);

                     State_Id := Expecting_Only_Trailing_Spaces;
                  elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
--                     Initialize (Call_Result, "New line is forbidden inside attribute value, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     Initialize (Call_Result, "New line is forbidden inside attribute value, state ");
                     exit;
                  elsif Is_Special_Symbol (CP) then
--                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point");
                     exit;
                  end if;
               when Extracting_End_Tag_Name =>
                  if CP = Character'Pos ('>') then
                     if P > Contents'Last then
                        Initialize (Call_Result, "Unexpected end of XML.");
                        exit;
                     end if;


                     End_Tag_Name_Last_Index := Prev_Prev_P;

                     declare
                        Tag : constant Tag_T := Last_Element (Tag_Ids);

                        pragma Assume (Tag.Name_First_Index in Contents_Index_T);
                        pragma Assume (Tag.Name_Last_Index in Contents_Index_T);

                        Tag_Name : Aida.String_T := Contents (Tag.Name_First_Index..Tag.Name_Last_Index);
                        Value : Aida.String_T := (if Tag_Value_First_Index <= Tag_Value_Last_Index then
                                                          Contents (Tag_Value_First_Index..Tag_Value_Last_Index)
                                                        else
                                                           "");
                     begin
                        if Tag_Name /= Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) then
--                             Initialize (Call_Result, "Tag names does not match! Start tag is '" & Tag_Name &
--                                           "', and end tag is '" & Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) & "' state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                           Initialize (Call_Result, "Tag names does not match! Start tag is ");
                           exit;
                        end if;

                        if not Shall_Ignore_Tag_Value then
                           Delete_Last (Tag_Ids);

                           if Length (Tag_Ids) = 0 then
                              Root_End_Tag (Arg,
                                            Tag_Name,
                                            Value,
                                            Tag_Id_T'First,
                                            Call_Result);
                           else
                              exit;
                              --                       Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                              --                                  Tag_Names,
                              --                                  Call_Result);
                           end if;

                           if Has_Failed (Call_Result) then
                              exit;
                           end if;
                        else
--                             declare
--                                Text_Value : String := Contents (Tag_Value_First_Index..Tag_Value_Last_Index);
--                             begin
--                                Text (Value       => Text_Value,
--                                      Parent_Tags => Tag_Names,
--                                      Call_Result => Call_Result);
--
--                                if Has_Failed (Call_Result) then
--                                   return;
--                                end if;
--                             end;

                           Delete_Last (Tag_Ids);
--                             End_Tag (Tag_Name,
--                                      Tag_Names,
--                                      Call_Result);

                           if Has_Failed (Call_Result) then
                              exit;
                           end if;
                        end if;

                        Tag_Value_First_Index := P;
                        Shall_Ignore_Tag_Value := Last_Element (Shall_Ignore_Tag_Value_List);
                        Delete_Last (Shall_Ignore_Tag_Value_List);
                     end;

                     State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
--                     Initialize (Call_Result, "New line is forbidden inside attribute value, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     Initialize (Call_Result, "New line is forbidden inside attribute value, state ");
                     exit;
                  elsif Is_Special_Symbol (CP) then
--                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point");
                     exit;
                  end if;
--                 when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash =>
--                    if CP = Character'Pos ('-') then
--                       declare
--                          Value : String := Contents (Tag_Value_First_Index..(P - 5));
--                       begin
--                          Text (Value       => Value,
--                                Parent_Tags => Tag_Names,
--                                Call_Result => Call_Result);
--                       end;
--                       Comment_First_Index := P;
--                       State_Id := Extracting_Comment;
--                    else
--                       State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
--                    end if;
                  --              when Extracting_Comment =>
                  --                 if CP = Character'Pos ('-') then
                  --                    State_Id := Extracting_Comment_And_Found_Dash;
                  --                 end if;
                  --              when Extracting_Comment_And_Found_Dash =>
                  --                 if CP = Character'Pos ('-') then
                  --                    State_Id := Extracting_Comment_And_Found_Dash_Dash;
                  --                 end if;
                  --              when Extracting_Comment_And_Found_Dash_Dash =>
                  --                 if CP = Character'Pos ('>') then
                  --                    declare
                  --                       Value : String := Contents (Comment_First_Index..(P - 4));
                  --                    begin
                  --                       Comment (Value       => Value,
                  --                                Parent_Tags => Tag_Names,
                  --                                Call_Result => Call_Result);
                  --                    end;
                  --
                  --                    Tag_Value_First_Index := P;
                  --                    State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  --                 end if;
               when Expecting_Only_Trailing_Spaces =>
                  if CP = Character'Pos (' ') then
                     null; -- Trailing spaces are OK
                  else
--                     Initialize (Call_Result, "New line is forbidden inside attribute value, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                     Initialize (Call_Result, "Unexpected symbol!");
                     exit;
                  end if;
            end case;
         end loop;
      end;

   end if;

end Aida.XML.Generic_Parse_XML_File;
