with Aida.Types;
with Ada.Containers;

use all type Ada.Containers.Count_Type;

generic
   type Arg_T is limited private; -- The result should be stored in this datastructure
   with procedure Root_Start_Tag (Arg         : Arg_T;
                                  Tag_Id      : Tag_Id_T;
                                  Call_Result : in out Procedure_Call_Result.T);

   with procedure Root_End_Tag (Arg         : Arg_T;
                                Tag_Id      : Tag_Id_T;
                                Call_Result : in out Procedure_Call_Result.T);
   with procedure Key_Name (Arg         : Arg_T;
                            Name        : Aida.Types.String_T;
                            Tag_Id      : Tag_Id_T;
                            Call_Result : in out Procedure_Call_Result.T);
   with procedure Value_String (Arg         : Arg_T;
                                Value       : Aida.Types.String_T;
                                Tag_Id      : Tag_Id_T;
                                Call_Result : in out Procedure_Call_Result.T);
   with procedure Value_Integer (Arg         : Arg_T;
                                 Value       : Aida.Types.Int32_T;
                                 Tag_Id      : Tag_Id_T;
                                 Call_Result : in out Procedure_Call_Result.T);
   with procedure Array_Start (Arg         : Arg_T;
                               Tag_Id      : Tag_Id_T;
                               Call_Result : in out Procedure_Call_Result.T);
   with procedure Array_End (Arg         : Arg_T;
                             Tag_Id      : Tag_Id_T;
                             Call_Result : in out Procedure_Call_Result.T);
procedure Aida.JSON.Generic_Parse_JSON (Arg           : in out Arg_T;
                                        Contents      : Aida.Types.String_T;
                                        Call_Result   : in out Procedure_Call_Result.T) with
  Global => null,
  Pre    => not Procedure_Call_Result.Has_Failed (Call_Result) and Procedure_Call_Result.Length (Call_Result) = 0 and Contents'Last < Integer'Last - 4;
