with Aida.UTF8;
with Aida.UTF8_Code_Point;
with Ada.Characters.Latin_1;
with Aida.Text_IO;
with Aida.Containers.Bounded_Vector;

procedure Aida.JSON.Generic_Parse_JSON (Arg           : in out Arg_T;
                                        Contents      : Aida.Types.String_T;
                                        Call_Result   : in out Procedure_Call_Result.T)
is
   use all type Aida.Types.String_T;
   use all type Aida.Types.Int32_T;
   use all type Aida.UTF8_Code_Point.T;
   use all type Procedure_Call_Result.T;

   Tag_Ids : Tag_Id_Vector.T;

   State_Id : State_Id_Type := Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket;

   F : Natural := Contents'First;

   use all type Tag_Id_Vector.T;
   use all type Tag_Id_Vector.Length_T;

   Next_Tag_Id : Tag_Id_T := Tag_Id_T'First;

   subtype P_T           is Integer range Contents'First..Contents'Last + 4;
   subtype Prev_P_T      is Integer range Contents'First..Contents'Last;
   subtype Prev_Prev_P_T is Integer range Contents'First..Contents'Last;

   subtype Contents_Index_T is Integer range Contents'First..Contents'Last;

   P           : P_T := Contents'First;
   Prev_P      : Prev_P_T := P;
   Prev_Prev_P : Prev_Prev_P_T;-- := Prev_P;

   CP      : Aida.UTF8_Code_Point.T;

   Key_Name_First_Index : Contents_Index_T := Contents_Index_T'First;
   Key_Name_Last_Index  : Contents_Index_T := Contents_Index_T'First;

   Value_First_Index : Contents_Index_T := Contents'First;
   Value_Last_Index  : Contents_Index_T := Contents'First;

   End_Tag_Name_First_Index : Contents_Index_T := Contents'First;
--   End_Tag_Name_Last_Index  : Contents_Index_T;

   Shall_Ignore_Tag_Value : Boolean := False;

begin
   while P <= Contents'Last loop
      Prev_Prev_P := Prev_P;

      Prev_P := P;

      if not Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => String (Contents),
                                                 Pointer => P)
      then
         Initialize (Call_Result, "ad460552-b38f-4004-b95e-a00dbf59d9fa, Found invalid UTF-8 character.");
         exit;
      end if;

      Aida.UTF8.Get (Source  => String (Contents),
                     Pointer => P,
                     Value   => CP);

      pragma Loop_Variant (Increases => P);
      pragma Loop_Invariant (not Has_Failed (Call_Result));
      pragma Loop_Invariant (P <= Contents'Last + 4);
--        pragma Loop_Invariant (Prev_Prev_P < Prev_P and Prev_P < P);

--                    Aida.Text_IO.Put_Line ("Extracted:" & Image (CP) & ", state " & State_Id_Type'Image (State_Id));
--                    Aida.Text_IO.Put (Image (CP));

      case State_Id is
         when Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket=>
            if CP = Character'Pos ('{') then
               State_Id := Found_Left_Curly_Bracket;


               Root_Start_Tag (Arg,
                               Next_Tag_Id,
                               Call_Result);

               Append (Tag_Ids, Next_Tag_Id);

               Next_Tag_Id := Next_Tag_Id + 1;

            end if;
         when Found_Left_Curly_Bracket =>
            if CP = Character'Pos ('"') then
               State_Id := Extracting_Key_Name;

               Key_Name_First_Index := P;

               --                 Initialize (Call_Result, "Found unexpected symbol '/'! Cannot continue parsing XML.");
               --                 exit;
               --              elsif not Is_Special_Symbol (CP) then
               --                 State_Id := Extracting_Root_Start_Tag_Name;
               --                 Start_Tag_Name_First_Index := Prev_P;
            elsif CP = Character'Pos (' ') then
               null;
            else
               Initialize (Call_Result, "572be577-962a-466b-9f16-5a83576fe513, Unexpected UTF8 symbol (code point ), state ");
               exit;
            end if;
         when Extracting_Key_Name =>
            if CP = Character'Pos ('"') then
               State_Id := Expecting_Comma_Sign_After_Key_Name;

               Key_Name_Last_Index := Prev_Prev_P;

               Key_Name (Arg,
                         Contents (Key_Name_First_Index..Key_Name_Last_Index),
                         Last_Element (Tag_Ids),
                         Call_Result);

            end if;
         when Expecting_Comma_Sign_After_Key_Name =>
            if CP = Character'Pos (',') then
               State_Id := Expecting_Value;
            elsif CP = Character'Pos (' ') then
               null;
            else
               Initialize (Call_Result, "a87ca7ac-4fdc-470e-b2f7-2acee4cc72fb, Unexpected UTF8 symbol (code point ), state ");
               exit;
            end if;
         when Expecting_Value =>
            if CP = Character'Pos ('"') then
               State_Id := Extracting_Value_String;

               Value_First_Index := P;

               --                 Initialize (Call_Result, "Found unexpected symbol '/'! Cannot continue parsing XML.");
               --                 exit;
               --              elsif not Is_Special_Symbol (CP) then
               --                 State_Id := Extracting_Root_Start_Tag_Name;
               --                 Start_Tag_Name_First_Index := Prev_P;
            elsif CP = Character'Pos (' ') then
               null;
            else
               Initialize (Call_Result, "e23b3bfb-6e24-47e7-a461-a6412cadb395, Unexpected UTF8 symbol (code point ), state ");
               exit;
            end if;
         when Extracting_Value_String =>
            if CP = Character'Pos ('"') then
               State_Id := Expecting_Comma_Sign_Or_Right_Bracket;

               Value_Last_Index := Prev_Prev_P;

               Value_String (Arg,
                             Contents (Value_First_Index..Value_Last_Index),
                             Last_Element (Tag_Ids),
                             Call_Result);

            end if;
         when Expecting_Comma_Sign_Or_Right_Bracket =>
            if CP = Character'Pos ('}') then
               State_Id := Found_End_Of_The_Very_Last_Object;

               if Length (Tag_Ids) = 1 then
                  Root_End_Tag (Arg,
                                Last_Element (Tag_Ids),
                                Call_Result);
               else
                  Initialize (Call_Result, "84cd2584-c5ee-4ede-9b95-1ad023efe356, Unexpected UTF8 symbol (code point ), state ");
                  exit;
               end if;
            end if;
         when Found_End_Of_The_Very_Last_Object =>
            if CP = Character'Pos (' ') then
               null;
            else
               Initialize (Call_Result, "51007c88-330d-4905-a82d-8fdd8354d5f7, Unexpected UTF8 symbol (code point ), state ");
               exit;
            end if;
--           when Extracting_Start_Tag_Name =>
--              if CP = Character'Pos (' ') then
--                 if Next_Tag_Id = Tag_Id_T'Last then
--                    Initialize (Call_Result, "Too many tags! Cannot continue parsing XML.");
--                    exit;
--                 end if;
--
--                 if Length (Tag_Ids) = MAX_DEPTH or Length (Shall_Ignore_Tag_Value_List) = MAX_DEPTH then
--                    Initialize (Call_Result, "Too many nested tags! Cannot continue parsing XML.");
--                    exit;
--                 end if;
--
--                 Start_Tag_Name_Last_Index := Prev_Prev_P;
--
--                 if Length (Tag_Ids) = 0 then
--                    Root_Start_Tag (Arg,
--                                    Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
--                                    Next_Tag_Id,
--                                    Call_Result);
--
--                 else
--                    exit;
--                    --                          Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
--                    --                                     Tag_Names,
--                    --                                     Call_Result);
--                 end if;
--
--                 if Has_Failed (Call_Result) then
--                    exit;
--                 end if;
--
--                 declare
--                    Tag : Tag_T := (Id => Next_Tag_Id,
--                                    Name_First_Index => Start_Tag_Name_First_Index,
--                                    Name_Last_Index  => Start_Tag_Name_Last_Index);
--                 begin
--                    Append (Tag_Ids, Tag);
--                 end;
--
--                 Next_Tag_Id := Next_Tag_Id + 1;
--
--                 Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);
--
--                 Shall_Ignore_Tag_Value := False;
--
--                 State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
--              elsif CP = Character'Pos ('>') then
--                 if Next_Tag_Id = Tag_Id_T'Last then
--                    Initialize (Call_Result, "Too many tags! Cannot continue parsing XML.");
--                    exit;
--                 end if;
--
--                 if Length (Tag_Ids) = MAX_DEPTH or Length (Shall_Ignore_Tag_Value_List) = MAX_DEPTH then
--                    Initialize (Call_Result, "Too many nested tags! Cannot continue parsing XML.");
--                    exit;
--                 end if;
--
--                 Start_Tag_Name_Last_Index := Prev_Prev_P;
--
--                 if Length (Tag_Ids) = 0 then
--                    Root_Start_Tag (Arg,
--                                    Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
--                                    Next_Tag_Id,
--                                    Call_Result);
--
--                 else
--                    exit;
--                    --                       Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
--                    --                                  Tag_Names,
--                    --                                  Call_Result);
--                 end if;
--
--                 if Has_Failed (Call_Result) then
--                    exit;
--                 end if;
--
--                 declare
--                    Tag : Tag_T := (Id => Next_Tag_Id,
--                                    Name_First_Index => Start_Tag_Name_First_Index,
--                                    Name_Last_Index  => Start_Tag_Name_Last_Index);
--                 begin
--                    Append (Tag_Ids, Tag);
--                 end;
--
--                 Next_Tag_Id := Next_Tag_Id + 1;
--
--                 Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);
--
--                 Shall_Ignore_Tag_Value := False;
--                 Tag_Value_First_Index := P;
--
--                 pragma Assert (Length (Tag_Ids) > 0 and Length (Shall_Ignore_Tag_Value_List) > 0);
--
--                 State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
--              elsif Is_Special_Symbol (CP) then
--                 --                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
--                 exit;
--              end if;
--           when Expecting_G_Sign_Or_Extracting_Attributes_For_Root =>
--              if CP = Character'Pos (' ') or CP = Character'Pos (Ada.Characters.Latin_1.LF) then
--                 null; -- Normal
--              elsif CP = Character'Pos ('>') then
--                 State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
--
--                 if P > Contents'Last then
--                    Initialize (Call_Result, "Unexpected end of XML.");
--                    exit;
--                 end if;
--
--                 Tag_Value_First_Index := P;
--              end if;
--           when Expecting_G_Sign_Or_Extracting_Attributes =>
--              if CP = Character'Pos (' ') or CP = Character'Pos (Ada.Characters.Latin_1.LF) then
--                 null; -- Normal
--              elsif CP = Character'Pos ('>') then
--                 State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
--
--                 if P > Contents'Last then
--                    Initialize (Call_Result, "Unexpected end of XML.");
--                    exit;
--                 end if;
--
--                 Tag_Value_First_Index := P;
--              end if;
--           when Expecting_New_Tag_Or_Extracting_Tag_Value =>
--              if CP = Character'Pos ('<') then
--                 State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L;
--                 Tag_Value_Last_Index := Prev_Prev_P;
--              end if;
--           when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L =>
--              if CP = Character'Pos ('/') then
--                 if P > Contents'Last then
--                    Initialize (Call_Result, "Unexpected end of XML.");
--                    exit;
--                 end if;
--
--                 if Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1 then
--                    State_Id := Extracting_Root_End_Tag_Name;
--                 else
--                    pragma Assert (Length (Tag_Ids) > 1 and Length (Shall_Ignore_Tag_Value_List) > 1);
--
--                    State_Id := Extracting_End_Tag_Name;
--                 end if;
--
--                 End_Tag_Name_First_Index := P;
--              elsif Is_Special_Symbol (CP) then
--                 --                     Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
--                 Initialize (Call_Result, "Unexpected UTF8 symbol ");
--                 exit;
--              end if;
--           when Extracting_Root_End_Tag_Name =>
--              pragma Assert (Length (Tag_Ids) = 1 and Length (Shall_Ignore_Tag_Value_List) = 1);
--              if CP = Character'Pos ('>') then
--
--                 End_Tag_Name_Last_Index := Prev_Prev_P;
--
--                 declare
--                    Tag : constant Tag_T := Last_Element (Tag_Ids);
--
--                    pragma Assume (Tag.Name_First_Index in Contents_Index_T);
--                    pragma Assume (Tag.Name_Last_Index in Contents_Index_T);
--
--                    Tag_Name : Aida.Types.String_T := Contents (Tag.Name_First_Index..Tag.Name_Last_Index);
--                    Value : Aida.Types.String_T := (if Tag_Value_First_Index <= Tag_Value_Last_Index then
--                                                       Contents (Tag_Value_First_Index..Tag_Value_Last_Index)
--                                                    else
--                                                       "");
--                 begin
--                    if Tag_Name /= Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) then
--                       --                             Initialize (Call_Result, "Tag names does not match! Start tag is '" & Tag_Name &
--                       --                                           "', and end tag is '" & Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) & "' state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
--                       Initialize (Call_Result, "Tag names does not match! Start tag is ");
--                       exit;
--                    end if;
--
--                    if not Shall_Ignore_Tag_Value then
--                       Delete_Last (Tag_Ids);
--
--                       Root_End_Tag (Arg,
--                                     Tag_Name,
--                                     Value,
--                                     Tag_Id_T'First,
--                                     Call_Result);
--
--                       if Has_Failed (Call_Result) then
--                          exit;
--                       end if;
--                    else
--
--                       Delete_Last (Tag_Ids);
--
--                       if Has_Failed (Call_Result) then
--                          exit;
--                       end if;
--                    end if;
--
--                    Shall_Ignore_Tag_Value := Last_Element (Shall_Ignore_Tag_Value_List);
--                    Delete_Last (Shall_Ignore_Tag_Value_List);
--                 end;
--
--                 pragma Assert (Length (Tag_Ids) = 0 and Length (Shall_Ignore_Tag_Value_List) = 0);
--
--                 State_Id := Expecting_Only_Trailing_Spaces;
--              elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
--                 Initialize (Call_Result, "New line is forbidden inside attribute value, state ");
--                 exit;
--              elsif Is_Special_Symbol (CP) then
--                 Initialize (Call_Result, "Unexpected UTF8 symbol (code point");
--                 exit;
--              end if;
--           when Extracting_End_Tag_Name =>
--              if CP = Character'Pos ('>') then
--                 if P > Contents'Last then
--                    Initialize (Call_Result, "Unexpected end of XML.");
--                    exit;
--                 end if;
--
--
--                 End_Tag_Name_Last_Index := Prev_Prev_P;
--
--                 declare
--                    Tag : constant Tag_T := Last_Element (Tag_Ids);
--
--                    pragma Assume (Tag.Name_First_Index in Contents_Index_T);
--                    pragma Assume (Tag.Name_Last_Index in Contents_Index_T);
--
--                    Tag_Name : Aida.Types.String_T := Contents (Tag.Name_First_Index..Tag.Name_Last_Index);
--                    Value : Aida.Types.String_T := (if Tag_Value_First_Index <= Tag_Value_Last_Index then
--                                                       Contents (Tag_Value_First_Index..Tag_Value_Last_Index)
--                                                    else
--                                                       "");
--                 begin
--                    if Tag_Name /= Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) then
--                       Initialize (Call_Result, "Tag names does not match! Start tag is ");
--                       exit;
--                    end if;
--
--                    if not Shall_Ignore_Tag_Value then
--                       Delete_Last (Tag_Ids);
--
--                       if Length (Tag_Ids) = 0 then
--                          Root_End_Tag (Arg,
--                                        Tag_Name,
--                                        Value,
--                                        Tag_Id_T'First,
--                                        Call_Result);
--                       else
--                          exit;
--                       end if;
--
--                       if Has_Failed (Call_Result) then
--                          exit;
--                       end if;
--                    else
--
--                       Delete_Last (Tag_Ids);
--
--                       if Has_Failed (Call_Result) then
--                          exit;
--                       end if;
--                    end if;
--
--                    Tag_Value_First_Index := P;
--                    Shall_Ignore_Tag_Value := Last_Element (Shall_Ignore_Tag_Value_List);
--                    Delete_Last (Shall_Ignore_Tag_Value_List);
--                 end;
--
--                 State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
--              elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
--                 Initialize (Call_Result, "New line is forbidden inside attribute value, state ");
--                 exit;
--              elsif Is_Special_Symbol (CP) then
--                 Initialize (Call_Result, "Unexpected UTF8 symbol (code point");
--                 exit;
--              end if;
--           when Expecting_Only_Trailing_Spaces =>
--              if CP = Character'Pos (' ') then
--                 null; -- Trailing spaces are OK
--              else
--                 Initialize (Call_Result, "Unexpected symbol!");
--                 exit;
--              end if;
      end case;
   end loop;
end Aida.JSON.Generic_Parse_JSON;

