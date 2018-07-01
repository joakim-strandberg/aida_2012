with Aida.UTF8;
with Aida.UTF8_Code_Point;
with Ada.Characters.Latin_1;
with Aida.Text_IO;
with Aida.Tagged_Bounded_Vector;
with Aida.Text_IO;

pragma Elaborate_All (Aida.Tagged_Bounded_Vector);

procedure Aida.JSON_SAX_Parse (Arg1        : in out Arg1_T;
                               Arg2        : in out Arg2_T;
                               Arg3        : in out Arg3_T;
                               Arg4        : in out Arg4_T;
                               Contents    : Standard.String;
                               Call_Result : in out Subprogram_Call_Result.T)
is
   use all type Standard.String;
   use all type Aida.Int32;
   use all type Aida.UTF8_Code_Point.T;

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
                          Extracting_Value_Integer_And_Found_Digit,
                          Found_End_Of_Object,
                          Found_Array_Start, -- Extracting elements
                          Found_End_Of_Element_In_Array,
                          Found_T,
                          Found_Tr,
                          Found_Tru,
                          Found_F,
                          Found_Fa,
                          Found_Fal,
                          Found_Fals,
                          Found_N,
                          Found_Nu,
                          Found_Nul
                         );

   type Tag_Id_T is new Aida.Int32;

   MAX_DEPTH : constant := 50;

   function Default_Tag_Id return Tag_Id_T with
     Global => null;

   function Default_Tag_Id return Tag_Id_T is (0);

   type Tag_Id_Index_T is new Aida.Pos32 range 1..MAX_DEPTH;

   package Tag_Id_Vector is new Aida.Tagged_Bounded_Vector
     (Max_Last_Index  => Int32'First + MAX_DEPTH,
      Element_T       => Tag_Id_T,
      Default_Element => Default_Tag_Id);


   Tag_Ids : Tag_Id_Vector.T;

   Array_Tag_Ids : Tag_Id_Vector.T;

   State_Id : State_Id_Type := Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket;

   Next_Tag_Id : Tag_Id_T := Tag_Id_T'First;
begin
   if Contents'Length = 0 then
      Call_Result.Initialize (-1522500631, -1361354891);
   else
      declare
         subtype P_T           is Int32 range Contents'First..Contents'Last + 4;
         subtype Prev_P_T      is Int32 range Contents'First..Contents'Last;
         subtype Prev_Prev_P_T is Int32 range Contents'First..Contents'Last;

         subtype Contents_Index_T is Int32 range Contents'First..Contents'Last;

         P           : P_T := Contents'First;
         Prev_P      : Prev_P_T := P;
         Prev_Prev_P : Prev_Prev_P_T;-- := Prev_P;

         CP      : Aida.UTF8_Code_Point.T;

         Key_Name_First_Index : P_T := Contents_Index_T'First;
         Key_Name_Last_Index  : Contents_Index_T;-- := Contents_Index_T'First;

         Value_First_Index : P_T := Contents'First;
         Value_Last_Index  : Contents_Index_T;-- := Contents'First;

      begin
         while P <= Contents'Last loop
            Prev_Prev_P := Prev_P;

            Prev_P := P;

            if not Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => Contents,
                                                       Pointer => P)
            then
               Call_Result.Initialize (-0694311125, 0512905127);
               exit;
            end if;

            Aida.UTF8.Get (Source  => Contents,
                           Pointer => P,
                           Value   => CP);

            pragma Loop_Variant (Increases => P);
            pragma Loop_Invariant (not Call_Result.Has_Failed);
            pragma Loop_Invariant (
                                   ((Prev_Prev_P = Contents'First and Prev_P = Contents'First) and then P > Contents'First) or
                                     ((Prev_Prev_P = Contents'First and Prev_P > Contents'First) and then Prev_P < P) or
                                       (Prev_Prev_P > Contents'First and then (Prev_Prev_P < Prev_P and Prev_P < P))
                                  );

            pragma Loop_Invariant (State_Id /= Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket or
                                     (State_Id = Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket and then (Tag_Ids.Is_Empty)));
            pragma Loop_Invariant (State_Id /= Extracting_Key_Name or
                                     (State_Id = Extracting_Key_Name and then (Key_Name_First_Index <= Contents'Last and Tag_Ids.Last_Index >= Tag_Ids.First_Index)));
            pragma Loop_Invariant (State_Id /= Found_End_Of_The_Very_Last_Object or
                                     (State_Id = Found_End_Of_The_Very_Last_Object and then (Tag_Ids.Is_Empty)));
            pragma Loop_Invariant ((State_Id = Extracting_Key_Name or State_Id = Found_End_Of_The_Very_Last_Object
                                  or State_Id = Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket) or
                                   ((State_Id /= Extracting_Key_Name and State_Id /= Found_End_Of_The_Very_Last_Object
                                  and State_Id /= Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket) and then (Tag_Ids.Last_Index >= Tag_Ids.First_Index)));

--                                Aida.Text_IO.Put ("Extracted:");
--                                Aida.Text_IO.Put (Image (CP));
--                                Aida.Text_IO.Put (", state ");
--                                Aida.Text_IO.Put_Line (String_T (State_Id_Type'Image (State_Id)));
--                                Aida.Text_IO.Put (Image (CP));

            case State_Id is
               when Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket =>
                  if CP = Standard.Character'Pos ('{') then
                     State_Id := Found_Left_Curly_Bracket;

                     Start_Object (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     if not Tag_Ids.Is_Full and Next_Tag_Id < Tag_Id_T'Last then
                        Tag_Ids.Append (Next_Tag_Id);

                        Next_Tag_Id := Next_Tag_Id + 1;
                     else
                        Call_Result.Initialize (-0828857595, 0729658471);
                        exit;
                     end if;
                  end if;
               when Found_Left_Curly_Bracket =>
                  if CP = Standard.Character'Pos ('"') then
                     State_Id := Extracting_Key_Name;

                     Key_Name_First_Index := P;
                  elsif CP = Standard.Character'Pos (' ') then
                     null;
                  else
                     Call_Result.Initialize (0666875904, -0130597293);
                     exit;
                  end if;
               when Extracting_Key_Name =>
                  if CP = Standard.Character'Pos ('"') then
                     State_Id := Expecting_Colon_Sign_After_Key_Name;

                     Key_Name_Last_Index := Prev_Prev_P;

                     Key (Arg1,
                               Arg2,
                               Arg3,
                               Arg4,
                               Contents (Key_Name_First_Index..Key_Name_Last_Index),
                               Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;
                  end if;
               when Expecting_Colon_Sign_After_Key_Name =>
                  if CP = Standard.Character'Pos (':') then
                     State_Id := Expecting_Value;
                  elsif CP = Standard.Character'Pos (' ') then
                     null;
                  else
                     Call_Result.Initialize (2012396005, -0563874321);
                     exit;
                  end if;
               when Expecting_Value =>
                  if CP = Standard.Character'Pos ('"') then
                     State_Id := Extracting_Value_String;

                     Value_First_Index := P;
                  elsif Is_Digit (CP) then
                     State_Id := Extracting_Value_Integer;
                     Value_First_Index := Prev_P;
                  elsif CP = Standard.Character'Pos ('-') then
                     State_Id := Extracting_Value_Integer;
                     Value_First_Index := Prev_P;
                  elsif CP = Standard.Character'Pos ('{') then
                     State_Id := Found_Left_Curly_Bracket;

                     Start_Object (Arg1,
                                     Arg2,
                                     Arg3,
                                     Arg4,
                                     Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     if not Tag_Ids.Is_Full and Next_Tag_Id < Tag_Id_T'Last then
                        Tag_Ids.Append (Next_Tag_Id);

                        Next_Tag_Id := Next_Tag_Id + 1;
                     else
                        Call_Result.Initialize (0511736023, -1155056733);
                        exit;
                     end if;
                  elsif CP = Standard.Character'Pos ('[') then
                     State_Id := Found_Array_Start;

                     if
                       Array_Tag_Ids.Is_Non_Empty and then
                       Array_Tag_Ids.Last_Element = Tag_Ids.Last_Element
                     then
                        Call_Result.Initialize (0690744029, 1711091773);
                        exit;
                     end if;

                     if not Array_Tag_Ids.Is_Full then
                        Array_Tag_Ids.Append (Tag_Ids.Last_Element);
                     else
                        Call_Result.Initialize (-0860721970, -0792673405);
                        exit;
                     end if;

                     Array_Start (Arg1,
                                  Arg2,
                                  Arg3,
                                  Arg4,
                                  Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                  elsif CP = Standard.Character'Pos (' ') then
                     null;
                  elsif CP = Standard.Character'Pos ('t') then
                     State_Id := Found_T;
                  elsif CP = Standard.Character'Pos ('f') then
                     State_Id := Found_F;
                  elsif CP = Standard.Character'Pos ('n') then
                     State_Id := Found_N;
                  else
                     Call_Result.Initialize (0573649478, -1295009043);
                     exit;
                  end if;
               when Extracting_Value_String =>
                  if CP = Standard.Character'Pos ('"') then
                     State_Id := Expecting_Comma_Sign_Or_Right_Bracket;

                     Value_Last_Index := Prev_Prev_P;

                     String_Value (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Contents (Value_First_Index..Value_Last_Index),
                                   Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                  end if;
               when Expecting_Comma_Sign_Or_Right_Bracket =>
                  if CP = Standard.Character'Pos ('}') then

                     End_Object (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     if Tag_Ids.Last_Index = Tag_Ids.First_Index then
                        State_Id := Found_End_Of_The_Very_Last_Object;

                        Tag_Ids.Delete_Last;
                     else
                        Tag_Ids.Delete_Last;

                        if
                          Array_Tag_Ids.Is_Non_Empty and then
                          Array_Tag_Ids.Last_Element = Tag_Ids.Last_Element
                        then
                           State_Id := Found_End_Of_Element_In_Array;
                        else
                           State_Id := Found_End_Of_Object;
                        end if;
                     end if;
                  elsif CP = Standard.Character'Pos (',') then
                     State_Id := Found_Left_Curly_Bracket;
                  elsif CP = Standard.Character'Pos (' ') then
                     null;
                  else
                     Call_Result.Initialize (2101566339, -1066214396);
                     exit;
                  end if;
               when Found_End_Of_The_Very_Last_Object =>
                  if CP = Standard.Character'Pos (' ') then
                     null;
                  else
                     Call_Result.Initialize (1630688703, 1885787424);
                     exit;
                  end if;
               when Extracting_Value_Integer =>
                  if Is_Digit (CP) then
                     null;
                  elsif CP = Standard.Character'Pos ('}') then

                     Value_Last_Index := Prev_Prev_P;

                     Integer_Value (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    Contents (Value_First_Index..Value_Last_Index),
                                    Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     if Tag_Ids.Last_Index = Tag_Ids.First_Index then
                        State_Id := Found_End_Of_The_Very_Last_Object;

                        End_Object (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Ids.Delete_Last;
                     else
                        End_Object (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Ids.Delete_Last;

                        if
                          Array_Tag_Ids.Is_Non_Empty and then
                          Array_Tag_Ids.Last_Element = Tag_Ids.Last_Element
                        then
                           State_Id := Found_End_Of_Element_In_Array;
                        else
                           State_Id := Found_End_Of_Object;
                        end if;
                     end if;
                  elsif CP = Standard.Character'Pos (',') then
                     Value_Last_Index := Prev_Prev_P;

                     Integer_Value (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    Contents (Value_First_Index..Value_Last_Index),
                                    Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     State_Id := Found_Left_Curly_Bracket;
                  elsif CP = Standard.Character'Pos ('.') then
                     State_Id := Extracting_Value_Integer_And_Found_Digit;
                  elsif CP = Standard.Character'Pos (' ') then
                     Value_Last_Index := Prev_Prev_P;

                     Integer_Value (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    Contents (Value_First_Index..Value_Last_Index),
                                    Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     State_Id := Found_End_Of_Object;
                  else
                     Call_Result.Initialize (0198286677, 0715152759);
                     exit;
                  end if;
               when Extracting_Value_Integer_And_Found_Digit =>
                  if Is_Digit (CP) then
                     null;
                  elsif CP = Standard.Character'Pos ('}') then

                     Value_Last_Index := Prev_Prev_P;

                     Real_Value (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Contents (Value_First_Index..Value_Last_Index),
                                 Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     if Tag_Ids.Last_Index = Tag_Ids.First_Index then
                        State_Id := Found_End_Of_The_Very_Last_Object;

                        End_Object (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Ids.Delete_Last;
                     else
                        End_Object (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Ids.Delete_Last;

                        if
                          Array_Tag_Ids.Is_Non_Empty and then
                          Array_Tag_Ids.Last_Element = Tag_Ids.Last_Element
                        then
                           State_Id := Found_End_Of_Element_In_Array;
                        else
                           State_Id := Found_End_Of_Object;
                        end if;
                     end if;
                  elsif CP = Standard.Character'Pos (',') then
                     Value_Last_Index := Prev_Prev_P;

                     Real_Value (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Contents (Value_First_Index..Value_Last_Index),
                                 Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     State_Id := Found_Left_Curly_Bracket;
                  elsif CP = Standard.Character'Pos (' ') then
                     Value_Last_Index := Prev_Prev_P;

                     Real_Value (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Contents (Value_First_Index..Value_Last_Index),
                                 Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     State_Id := Found_End_Of_Object;
                  else
                     Call_Result.Initialize (-0974748348, 0830710943);
                     exit;
                  end if;
               when Found_End_Of_Object =>
                  if CP = Standard.Character'Pos (',') then
                     State_Id := Found_Left_Curly_Bracket;
                  elsif CP = Standard.Character'Pos ('}') then
                     if Tag_Ids.Last_Index = Tag_Ids.First_Index then
                        State_Id := Found_End_Of_The_Very_Last_Object;

                        End_Object (Arg1,
                                      Arg2,
                                      Arg3,
                                      Arg4,
                                      Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Ids.Delete_Last;
                     else
                        End_Object (Arg1,
                                      Arg2,
                                      Arg3,
                                      Arg4,
                                      Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Ids.Delete_Last;

                        if
                          Array_Tag_Ids.Is_Non_Empty and then
                          Array_Tag_Ids.Last_Element = Tag_Ids.Last_Element
                        then
                           State_Id := Found_End_Of_Element_In_Array;
                        else
                           State_Id := Found_End_Of_Object;
                        end if;
                     end if;
                  elsif CP = Standard.Character'Pos (' ') then
                     null;
                  else
                     Call_Result.Initialize (0743073911, 0562296894);
                     exit;
                  end if;
               when Found_Array_Start =>
                  if CP = Standard.Character'Pos ('{') then
                     State_Id := Found_Left_Curly_Bracket;

                     Start_Object (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     if not Tag_Ids.Is_Full and Next_Tag_Id < Tag_Id_T'Last then
                        Tag_Ids.Append (Next_Tag_Id);

                        Next_Tag_Id := Next_Tag_Id + 1;
                     else
                        Call_Result.Initialize (1158454393, 2020449118);
                        exit;
                     end if;
                  elsif CP = Standard.Character'Pos (' ') then
                     null;
                  else
                     Call_Result.Initialize (-2083949210, -0974236942);
                     exit;
                  end if;
               when Found_End_Of_Element_In_Array =>
                  if CP = Standard.Character'Pos (',') then
                     State_Id := Found_Array_Start;
                  elsif CP = Standard.Character'Pos (']') then
                     State_Id := Expecting_Comma_Sign_Or_Right_Bracket;

                     if Array_Tag_Ids.Is_Non_Empty then
                        Array_End (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Array_Tag_Ids.Delete_Last;
                     else
                        Call_Result.Initialize (-1612352731, -1655012836);
                        exit;
                     end if;

                  elsif CP = Standard.Character'Pos (' ') then
                     null;
                  else
                     Call_Result.Initialize (-0942307720, -1564578584);
                     exit;
                  end if;
               when Found_T =>
                  if CP = Standard.Character'Pos ('r') then
                     State_Id := Found_Tr;
                  else
                     Call_Result.Initialize (-1617816769, -1590689811);
                     exit;
                  end if;
               when Found_Tr =>
                  if CP = Standard.Character'Pos ('u') then
                     State_Id := Found_Tru;
                  else
                     Call_Result.Initialize (-0048100488, 0487444183);
                     exit;
                  end if;
               when Found_Tru =>
                  if CP = Standard.Character'Pos ('e') then
                     Boolean_Value (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    True,
                                    Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     State_Id := Expecting_Comma_Sign_Or_Right_Bracket;
                  else
                     Call_Result.Initialize (-1046526135, 1665221499);
                     exit;
                  end if;
               when Found_F =>
                  if CP = Standard.Character'Pos ('a') then
                     State_Id := Found_Fa;
                  else
                     Call_Result.Initialize (-0405689656, 1766237800);
                     exit;
                  end if;
               when Found_Fa =>
                  if CP = Standard.Character'Pos ('l') then
                     State_Id := Found_Fal;
                  else
                     Call_Result.Initialize (-0832673158, 0650292100);
                     exit;
                  end if;
               when Found_Fal =>
                  if CP = Standard.Character'Pos ('s') then
                     State_Id := Found_Fals;
                  else
                     Call_Result.Initialize (2123299680, 0898372600);
                     exit;
                  end if;
               when Found_Fals =>
                  if CP = Standard.Character'Pos ('e') then
                     Boolean_Value (Arg1,
                                    Arg2,
                                    Arg3,
                                    Arg4,
                                    False,
                                    Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     State_Id := Expecting_Comma_Sign_Or_Right_Bracket;
                  else
                     Call_Result.Initialize (1003390043, 0541089769);
                     exit;
                  end if;
               when Found_N =>
                  if CP = Standard.Character'Pos ('u') then
                     State_Id := Found_Nu;
                  else
                     Call_Result.Initialize (-0396203491, -1065957165);
                     exit;
                  end if;
               when Found_Nu =>
                  if CP = Standard.Character'Pos ('l') then
                     State_Id := Found_Nul;
                  else
                     Call_Result.Initialize (-1690061121, -0156509527);
                     exit;
                  end if;
               when Found_Nul =>
                  if CP = Standard.Character'Pos ('l') then
                     Null_Value (Arg1,
                                 Arg2,
                                 Arg3,
                                 Arg4,
                                 Call_Result);

                     if Call_Result.Has_Failed then
                        exit;
                     end if;

                     State_Id := Expecting_Comma_Sign_Or_Right_Bracket;
                  else
                     Call_Result.Initialize (-1016092230, -0426084221);
                     exit;
                  end if;
            end case;
         end loop;
      end;
   end if;

end Aida.JSON_SAX_Parse;
