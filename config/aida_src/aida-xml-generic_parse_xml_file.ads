with Aida.Types;

generic
   type Arg_T is limited private; -- The result should be stored in this datastructure
   with procedure Root_Start_Tag (Arg         : Arg_T;
                                  Tag_Name    : Aida.Types.String_T;
                                  Tag_Id      : Tag_Id_T;
                                  Call_Result : in out Procedure_Call_Result.T);

   with procedure Root_End_Tag (Arg         : Arg_T;
                                Tag_Name    : Aida.Types.String_T;
                                Tag_Value   : Aida.Types.String_T;
                                Tag_Id      : Tag_Id_T;
                                Call_Result : in out Procedure_Call_Result.T);

--     with procedure Start_Tag (Tag_Name    : String;
--                               Parent_Tags : Tag_Name_Vector_T;
--                               Call_Result : in out Subprogram_Call_Result.T);

--     with procedure Attribute (Attribute_Name              : String;
--                               Attribute_Value             : String;
--                               Parent_Tags_And_Current_Tag : Tag_Name_Vector_T;
--                               Call_Result                 : in out Subprogram_Call_Result.T);
--
--     with procedure Text (Value       : String;
--                          Parent_Tags : Tag_Name_Vector_T;
--                          Call_Result : in out Subprogram_Call_Result.T);
--
--     with procedure Comment (Value       : String;
--                             Parent_Tags : Tag_Name_Vector_T;
--                             Call_Result : in out Subprogram_Call_Result.T);
--
--     with procedure End_Tag (Tag_Name    : String;
--                             Parent_Tags : Tag_Name_Vector_T;
--                             Call_Result : in out Subprogram_Call_Result.T);

--     with procedure End_Tag (Tag_Name    : String;
--                             Tag_Value   : String;
--                             Parent_Tags : Tag_Name_Vector_T;
--                             Call_Result : in out Subprogram_Call_Result.T);
procedure Aida.XML.Generic_Parse_XML_File (Arg           : Arg_T;
                                           Contents      : Aida.Types.String_T;
                                           Call_Result   : in out Procedure_Call_Result.T) with
  Global => null,
  Pre    => not Procedure_Call_Result.Has_Failed (Call_Result) and Procedure_Call_Result.Length (Call_Result) = 0 and Contents'Length > 38 and Contents'Last < Integer'Last - 4;
