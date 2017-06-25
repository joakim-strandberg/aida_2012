with Aida.UTF8;
with Aida.UTF8_Code_Point;
with Ada.Characters.Latin_1;
with Aida.Text_IO;
with Aida.Containers.Bounded_Vector;

procedure Aida.JSON.Generic_Parse_JSON (Arg1        : in out Arg1_T;
                                        Arg2        : in out Arg2_T;
                                        Arg3        : in out Arg3_T;
                                        Arg4        : in out Arg4_T;
                                        Contents    : Aida.Types.String_T;
                                        Call_Result : in out Procedure_Call_Result.T)
is
   use all type Aida.Types.String_T;
   use all type Aida.Types.Int32_T;
   use all type Aida.UTF8_Code_Point.T;
   use all type Procedure_Call_Result.T;

   Tag_Ids : Tag_Id_Vector.T;

   Array_Tag_Ids : Tag_Id_Vector.T;

   State_Id : State_Id_Type := Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket;

   use all type Tag_Id_Vector.T;
   use all type Tag_Id_Vector.Length_T;

   Next_Tag_Id : Tag_Id_T := Tag_Id_T'First;
begin
   if Contents'Length = 0 then
      Initialize (Call_Result, "1a7aca16-16dd-41cc-99a2-491f27fbb432, Contents is empty!");
   else
      declare
         subtype P_T           is Integer range Contents'First..Contents'Last + 4;
         subtype Prev_P_T      is Integer range Contents'First..Contents'Last;
         subtype Prev_Prev_P_T is Integer range Contents'First..Contents'Last;

         subtype Contents_Index_T is Integer range Contents'First..Contents'Last;

         P           : P_T := Contents'First;
         Prev_P      : Prev_P_T := P;
         Prev_Prev_P : Prev_Prev_P_T;-- := Prev_P;

         CP      : Aida.UTF8_Code_Point.T;

         Key_Name_First_Index : P_T := Contents_Index_T'First;
         Key_Name_Last_Index  : Contents_Index_T;-- := Contents_Index_T'First;

         Value_First_Index : P_T := Contents'First;
         Value_Last_Index  : Contents_Index_T;-- := Contents'First;

         --         End_Tag_Name_First_Index : Contents_Index_T := Contents'First;
         --   End_Tag_Name_Last_Index  : Contents_Index_T;

         --         Shall_Ignore_Tag_Value : Boolean := False;
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
            pragma Loop_Invariant (
                                   ((Prev_Prev_P = Contents'First and Prev_P = Contents'First) and then P > Contents'First) or
                                     ((Prev_Prev_P = Contents'First and Prev_P > Contents'First) and then Prev_P < P) or
                                       (Prev_Prev_P > Contents'First and then (Prev_Prev_P < Prev_P and Prev_P < P))
                                  );
            pragma Loop_Invariant (not Has_Failed (Call_Result));
            pragma Loop_Invariant (State_Id /= Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket or
                                     (State_Id = Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket and then (Length (Tag_Ids) = 0)));
            pragma Loop_Invariant (State_Id /= Found_Left_Curly_Bracket or
                                     (State_Id = Found_Left_Curly_Bracket and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Extracting_Key_Name or
                                     (State_Id = Extracting_Key_Name and then (Key_Name_First_Index <= Contents'Last and Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Expecting_Colon_Sign_After_Key_Name or
                                     (State_Id = Expecting_Colon_Sign_After_Key_Name and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Expecting_Value or
                                     (State_Id = Expecting_Value and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Extracting_Value_String or
                                     (State_Id = Extracting_Value_String and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Extracting_Value_String or
                                     (State_Id = Extracting_Value_String and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Expecting_Comma_Sign_Or_Right_Bracket or
                                     (State_Id = Expecting_Comma_Sign_Or_Right_Bracket and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Found_End_Of_The_Very_Last_Object or
                                     (State_Id = Found_End_Of_The_Very_Last_Object and then (Length (Tag_Ids) = 0)));
            pragma Loop_Invariant (State_Id /= Extracting_Value_Integer or
                                     (State_Id = Extracting_Value_Integer and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Found_End_Of_Object or
                                     (State_Id = Found_End_Of_Object and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Found_Array_Start or
                                     (State_Id = Found_Array_Start and then (Length (Tag_Ids) > 0)));
            pragma Loop_Invariant (State_Id /= Found_End_Of_Element_In_Array or
                                     (State_Id = Found_End_Of_Element_In_Array and then (Length (Tag_Ids) > 0)));


            --                    Aida.Text_IO.Put_Line ("Extracted:" & Image (CP) & ", state " & State_Id_Type'Image (State_Id));
            --                    Aida.Text_IO.Put (Image (CP));

            case State_Id is
               when Expecting_NL_Sign_Or_Space_Or_Left_Curly_Bracket =>
                  if CP = Character'Pos ('{') then
                     State_Id := Found_Left_Curly_Bracket;

                     Root_Start_Tag (Arg1,
                                     Arg2,
                                     Arg3,
                                     Arg4,
                                     Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     if Length (Tag_Ids) < MAX_DEPTH and Next_Tag_Id < Tag_Id_T'Last then
                        Append (Tag_Ids, Next_Tag_Id);

                        Next_Tag_Id := Next_Tag_Id + 1;
                     else
                        Initialize (Call_Result, "6c505745-532b-4597-9553-6c93d89bc4f9");
                        exit;
                     end if;
                  end if;
               when Found_Left_Curly_Bracket =>
                  if CP = Character'Pos ('"') then
                     State_Id := Extracting_Key_Name;

                     Key_Name_First_Index := P;
                  elsif CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "572be577-962a-466b-9f16-5a83576fe513");
                     exit;
                  end if;
               when Extracting_Key_Name =>
                  if CP = Character'Pos ('"') then
                     State_Id := Expecting_Colon_Sign_After_Key_Name;

                     Key_Name_Last_Index := Prev_Prev_P;

                     Key_Name (Arg1,
                               Arg2,
                               Arg3,
                               Arg4,
                               Contents (Key_Name_First_Index..Key_Name_Last_Index),
                               Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;
                  end if;
               when Expecting_Colon_Sign_After_Key_Name =>
                  if CP = Character'Pos (':') then
                     State_Id := Expecting_Value;
                  elsif CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "a87ca7ac-4fdc-470e-b2f7-2acee4cc72fb");
                     exit;
                  end if;
               when Expecting_Value =>
                  if CP = Character'Pos ('"') then
                     State_Id := Extracting_Value_String;

                     Value_First_Index := P;
                  elsif Is_Digit (CP) then
                     State_Id := Extracting_Value_Integer;
                     Value_First_Index := Prev_P;
                  elsif CP = Character'Pos ('-') then
                     State_Id := Extracting_Value_Integer;
                     Value_First_Index := Prev_P;
                  elsif CP = Character'Pos ('{') then
                     State_Id := Found_Left_Curly_Bracket;

                     Root_Start_Tag (Arg1,
                                     Arg2,
                                     Arg3,
                                     Arg4,
                                     Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     if Length (Tag_Ids) < MAX_DEPTH and Next_Tag_Id < Tag_Id_T'Last then
                        Append (Tag_Ids, Next_Tag_Id);

                        Next_Tag_Id := Next_Tag_Id + 1;
                     else
                        Initialize (Call_Result, "81471a75-a68e-49fc-9341-85644da117af");
                        exit;
                     end if;
                  elsif CP = Character'Pos ('[') then
                     State_Id := Found_Array_Start;

                     if
                       (Length (Array_Tag_Ids) > 0) and then
                       Last_Element (Array_Tag_Ids) = Last_Element (Tag_Ids)
                     then
                        Initialize (Call_Result, "07d6a3ab-179e-482b-b61d-b7915c34bd62");
                        exit;
                     end if;

                     if Length (Array_Tag_Ids) < MAX_DEPTH then
                        Append (Array_Tag_Ids, Last_Element (Tag_Ids));
                     else
                        Initialize (Call_Result, "3006c915-77e7-4fba-8c96-69567b96c373");
                        exit;
                     end if;

                     Array_Start (Arg1,
                                  Arg2,
                                  Arg3,
                                  Arg4,
                                  Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                  elsif CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "e23b3bfb-6e24-47e7-a461-a6412cadb395");
                     exit;
                  end if;
               when Extracting_Value_String =>
                  if CP = Character'Pos ('"') then
                     State_Id := Expecting_Comma_Sign_Or_Right_Bracket;

                     Value_Last_Index := Prev_Prev_P;

                     Value_String (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Contents (Value_First_Index..Value_Last_Index),
                                   Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                  end if;
               when Expecting_Comma_Sign_Or_Right_Bracket =>
                  if CP = Character'Pos ('}') then

                     Root_End_Tag (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     if Length (Tag_Ids) = 1 then
                        State_Id := Found_End_Of_The_Very_Last_Object;

                        Delete_Last (Tag_Ids);
                     else
                        Delete_Last (Tag_Ids);

                        if
                          Length (Array_Tag_Ids) > 0 and then
                          Last_Element (Array_Tag_Ids) = Last_Element (Tag_Ids)
                        then
                           State_Id := Found_End_Of_Element_In_Array;
                        else
                           State_Id := Found_End_Of_Object;
                        end if;
                     end if;
                  elsif CP = Character'Pos (',') then
                     State_Id := Found_Left_Curly_Bracket;
                  elsif CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "427b46d0-8f27-475b-8856-9bb345517f55");
                     exit;
                  end if;
               when Found_End_Of_The_Very_Last_Object =>
                  if CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "51007c88-330d-4905-a82d-8fdd8354d5f7");
                     exit;
                  end if;
               when Extracting_Value_Integer =>
                  if Is_Digit (CP) then
                     null;
                  elsif CP = Character'Pos ('}') then

                     Value_Last_Index := Prev_Prev_P;

                     declare
                        I : Aida.Types.Int32_T;
                        HF : Boolean;
                     begin
                        To_Int32 (Contents (Value_First_Index..Value_Last_Index),
                                  I,
                                  HF);
                        if HF then
                           Initialize (Call_Result, "e1d58647-b39e-402b-9ae8-de22d6fbf5be, failed to convert string to integer");
                           exit;
                        end if;

                        Value_Integer (Arg1,
                                       Arg2,
                                       Arg3,
                                       Arg4,
                                       I,
                                       Call_Result);

                        if Has_Failed (Call_Result) then
                           exit;
                        end if;

                        if Length (Tag_Ids) = 1 then
                           State_Id := Found_End_Of_The_Very_Last_Object;

                           Root_End_Tag (Arg1,
                                         Arg2,
                                         Arg3,
                                         Arg4,
                                         Call_Result);

                           if Has_Failed (Call_Result) then
                              exit;
                           end if;

                           Delete_Last (Tag_Ids);
                        else
                           Root_End_Tag (Arg1,
                                         Arg2,
                                         Arg3,
                                         Arg4,
                                         Call_Result);

                           if Has_Failed (Call_Result) then
                              exit;
                           end if;

                           Delete_Last (Tag_Ids);

                           if
                             Length (Array_Tag_Ids) > 0 and then
                             Last_Element (Array_Tag_Ids) = Last_Element (Tag_Ids)
                           then
                              State_Id := Found_End_Of_Element_In_Array;
                           else
                              State_Id := Found_End_Of_Object;
                           end if;
                        end if;
                     end;
                  elsif CP = Character'Pos (',') then
                     Value_Last_Index := Prev_Prev_P;

                     declare
                        I : Aida.Types.Int32_T;
                        HF : Boolean;
                     begin
                        To_Int32 (Contents (Value_First_Index..Value_Last_Index),
                                  I,
                                  HF);
                        if HF then
                           Initialize (Call_Result, "b03c7fbd-f8da-4f6d-b8ba-6302f72680eb, failed to convert string to integer");
                           exit;
                        end if;

                        Value_Integer (Arg1,
                                       Arg2,
                                       Arg3,
                                       Arg4,
                                       I,
                                       Call_Result);

                        if Has_Failed (Call_Result) then
                           exit;
                        end if;

                        State_Id := Found_Left_Curly_Bracket;
                     end;
                  elsif CP = Character'Pos (' ') then
                     Value_Last_Index := Prev_Prev_P;

                     declare
                        I : Aida.Types.Int32_T;
                        HF : Boolean;
                     begin
                        To_Int32 (Contents (Value_First_Index..Value_Last_Index),
                                  I,
                                  HF);
                        if HF then
                           Initialize (Call_Result, "e1d58647-b39e-402b-9ae8-de22d6fbf5be, failed to convert string to integer");
                           exit;
                        end if;

                        Value_Integer (Arg1,
                                       Arg2,
                                       Arg3,
                                       Arg4,
                                       I,
                                       Call_Result);

                        if Has_Failed (Call_Result) then
                           exit;
                        end if;

                        State_Id := Found_End_Of_Object;
                     end;
                  else
                     Initialize (Call_Result, "db92b536-4543-4fe5-9254-e477d0cdc01b");
                     exit;
                  end if;
               when Found_End_Of_Object =>
                  if CP = Character'Pos (',') then
                     State_Id := Found_Left_Curly_Bracket;
                  elsif CP = Character'Pos ('}') then
                     if Length (Tag_Ids) = 1 then
                        State_Id := Found_End_Of_The_Very_Last_Object;

                        Root_End_Tag (Arg1,
                                      Arg2,
                                      Arg3,
                                      Arg4,
                                      Call_Result);

                        if Has_Failed (Call_Result) then
                           exit;
                        end if;

                        Delete_Last (Tag_Ids);
                     else
                        Root_End_Tag (Arg1,
                                      Arg2,
                                      Arg3,
                                      Arg4,
                                      Call_Result);

                        if Has_Failed (Call_Result) then
                           exit;
                        end if;

                        Delete_Last (Tag_Ids);

                        if
                          Length (Array_Tag_Ids) > 0 and then
                          Last_Element (Array_Tag_Ids) = Last_Element (Tag_Ids)
                        then
                           State_Id := Found_End_Of_Element_In_Array;
                        else
                           State_Id := Found_End_Of_Object;
                        end if;
                     end if;
                  elsif CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "51007c88-330d-4905-a82d-8fdd8354d5f7");
                     exit;
                  end if;
               when Found_Array_Start =>
                  if CP = Character'Pos ('{') then
                     State_Id := Found_Left_Curly_Bracket;

                     Root_Start_Tag (Arg1,
                                     Arg2,
                                     Arg3,
                                     Arg4,
                                     Call_Result);

                     if Has_Failed (Call_Result) then
                        exit;
                     end if;

                     if Length (Tag_Ids) < MAX_DEPTH and Next_Tag_Id < Tag_Id_T'Last then
                        Append (Tag_Ids, Next_Tag_Id);

                        Next_Tag_Id := Next_Tag_Id + 1;
                     else
                        Initialize (Call_Result, "62d0dd9d-de2b-4fd4-a879-f8691a591f73");
                        exit;
                     end if;
                  elsif CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "924eac23-376d-4648-821b-8f4fb68e2bf0");
                     exit;
                  end if;
               when Found_End_Of_Element_In_Array =>
                  if CP = Character'Pos (',') then
                     State_Id := Found_Array_Start;
                  elsif CP = Character'Pos (']') then
                     State_Id := Expecting_Comma_Sign_Or_Right_Bracket;

                     if Length (Array_Tag_Ids) > 0 then
                        Array_End (Arg1,
                                   Arg2,
                                   Arg3,
                                   Arg4,
                                   Call_Result);

                        if Has_Failed (Call_Result) then
                           exit;
                        end if;

                        Delete_Last (Array_Tag_Ids);
                     else
                        Initialize (Call_Result, "c0c9ce80-36f7-4ad8-81e9-1129db4b3c8b");
                        exit;
                     end if;

                  elsif CP = Character'Pos (' ') then
                     null;
                  else
                     Initialize (Call_Result, "52ae4ef7-d735-4255-b00d-18396d0215ad");
                     exit;
                  end if;
            end case;
         end loop;
      end;
   end if;

end Aida.JSON.Generic_Parse_JSON;

