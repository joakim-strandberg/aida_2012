with Aida.Types;
with Ada.Containers;

use all type Ada.Containers.Count_Type;

generic
   type Arg1_T is limited private; -- The result should be stored in this datastructure
   type Arg2_T is limited private;
   type Arg3_T is limited private;
   type Arg4_T is limited private;
   with procedure Root_Start_Tag (Arg1        : in out Arg1_T;
                                  Arg2        : in out Arg2_T;
                                  Arg3        : in out Arg3_T;
                                  Arg4        : in out Arg4_T;
                                  Tag_Id      : Tag_Id_T;
                                  Call_Result : in out Procedure_Call_Result.T);

   with procedure Root_End_Tag (Arg1        : in out Arg1_T;
                                Arg2        : in out Arg2_T;
                                Arg3        : in out Arg3_T;
                                Arg4        : in out Arg4_T;
                                Tag_Id      : Tag_Id_T;
                                Call_Result : in out Procedure_Call_Result.T);
   with procedure Key_Name (Arg1        : in out Arg1_T;
                            Arg2        : in out Arg2_T;
                            Arg3        : in out Arg3_T;
                            Arg4        : in out Arg4_T;
                            Name        : Aida.Types.String_T;
                            Tag_Id      : Tag_Id_T;
                            Call_Result : in out Procedure_Call_Result.T);
   with procedure Value_String (Arg1        : in out Arg1_T;
                                Arg2        : in out Arg2_T;
                                Arg3        : in out Arg3_T;
                                Arg4        : in out Arg4_T;
                                Value       : Aida.Types.String_T;
                                Tag_Id      : Tag_Id_T;
                                Call_Result : in out Procedure_Call_Result.T);
   with procedure Value_Integer (Arg1        : in out Arg1_T;
                                 Arg2        : in out Arg2_T;
                                 Arg3        : in out Arg3_T;
                                 Arg4        : in out Arg4_T;
                                 Value       : Aida.Types.Int32_T;
                                 Tag_Id      : Tag_Id_T;
                                 Call_Result : in out Procedure_Call_Result.T);
   with procedure Array_Start (Arg1        : in out Arg1_T;
                               Arg2        : in out Arg2_T;
                               Arg3        : in out Arg3_T;
                               Arg4        : in out Arg4_T;
                               Tag_Id      : Tag_Id_T;
                               Call_Result : in out Procedure_Call_Result.T);
   with procedure Array_End (Arg1        : in out Arg1_T;
                             Arg2        : in out Arg2_T;
                             Arg3        : in out Arg3_T;
                             Arg4        : in out Arg4_T;
                             Tag_Id      : Tag_Id_T;
                             Call_Result : in out Procedure_Call_Result.T);
procedure Aida.JSON.Generic_Parse_JSON (Arg1        : in out Arg1_T;
                                        Arg2        : in out Arg2_T;
                                        Arg3        : in out Arg3_T;
                                        Arg4        : in out Arg4_T;
                                        Contents    : Aida.Types.String_T;
                                        Call_Result : in out Procedure_Call_Result.T) with
  Global => null,
  Pre    => not Procedure_Call_Result.Has_Failed (Call_Result) and Procedure_Call_Result.Length (Call_Result) = 0 and Contents'Last < Integer'Last - 4;
