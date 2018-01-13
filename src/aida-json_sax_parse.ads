with Aida;
with Ada.Containers;
with Aida.Subprogram_Call_Result;

use all type Ada.Containers.Count_Type;

-- The names of the procedures Start_Object, End_Object, Key, Array_Start and
-- Array_End are inspired by the RapidJSON implemented in C++.
--
-- An alternative JSON parsing implementation would be to create a Parent type:
--
-- type Parent_Type is abstract tagged private;
--
-- procedure Start_Object (This : Parent_Type;
--                         ...) is null;
--
-- procedure End_Object (This : Parent_Type;
--                       ...) is null;
-- ...
--
-- The idea would then be for the user to override
-- the Start_Object, End_Object,... procedures to do the JSON parsing.
-- However, this would introduce usage of Dynamic dispatch to determine which
-- implementation of the procedures should be called. Therefore, for performance
-- reasons Dynamic dispatch has been avoided by using a generic procedure for
-- the JSON parsing.
--
-- Note that whenever an Int32_T or Real number is encountered
-- it is passed as an instance of Standard.String to the procedures
-- Integer_Value and Real_Value. This leaves the actual parsing of these values
-- to the implementer. This is because unbounded Integers and Reals are part
-- of the JSON standard. If rounding is accepted or not is up to the implementer
-- and so on.
generic
   type Arg1_T is limited private;
   type Arg2_T is limited private;
   type Arg3_T is limited private;
   type Arg4_T is limited private;
   with procedure Start_Object (Arg1        : in out Arg1_T;
                                Arg2        : in out Arg2_T;
                                Arg3        : in out Arg3_T;
                                Arg4        : in out Arg4_T;
                                Call_Result : in out Subprogram_Call_Result.T);
   with procedure End_Object (Arg1        : in out Arg1_T;
                              Arg2        : in out Arg2_T;
                              Arg3        : in out Arg3_T;
                              Arg4        : in out Arg4_T;
                              Call_Result : in out Subprogram_Call_Result.T);
   with procedure Key (Arg1        : in out Arg1_T;
                       Arg2        : in out Arg2_T;
                       Arg3        : in out Arg3_T;
                       Arg4        : in out Arg4_T;
                       Name        : in     Standard.String;
                       Call_Result : in out Subprogram_Call_Result.T);
   with procedure String_Value (Arg1        : in out Arg1_T;
                                Arg2        : in out Arg2_T;
                                Arg3        : in out Arg3_T;
                                Arg4        : in out Arg4_T;
                                Value       : in     Standard.String;
                                Call_Result : in out Subprogram_Call_Result.T);
   with procedure Integer_Value (Arg1        : in out Arg1_T;
                                 Arg2        : in out Arg2_T;
                                 Arg3        : in out Arg3_T;
                                 Arg4        : in out Arg4_T;
                                 Value       : in     Standard.String;
                                 Call_Result : in out Subprogram_Call_Result.T);
   with procedure Real_Value (Arg1        : in out Arg1_T;
                              Arg2        : in out Arg2_T;
                              Arg3        : in out Arg3_T;
                              Arg4        : in out Arg4_T;
                              Value       : in     Standard.String;
                              Call_Result : in out Subprogram_Call_Result.T);
   with procedure Boolean_Value (Arg1        : in out Arg1_T;
                                 Arg2        : in out Arg2_T;
                                 Arg3        : in out Arg3_T;
                                 Arg4        : in out Arg4_T;
                                 Value       : in     Boolean;
                                 Call_Result : in out Subprogram_Call_Result.T);
   with procedure Null_Value (Arg1        : in out Arg1_T;
                              Arg2        : in out Arg2_T;
                              Arg3        : in out Arg3_T;
                              Arg4        : in out Arg4_T;
                              Call_Result : in out Subprogram_Call_Result.T);
   with procedure Array_Start (Arg1        : in out Arg1_T;
                               Arg2        : in out Arg2_T;
                               Arg3        : in out Arg3_T;
                               Arg4        : in out Arg4_T;
                               Call_Result : in out Subprogram_Call_Result.T);
   with procedure Array_End (Arg1        : in out Arg1_T;
                             Arg2        : in out Arg2_T;
                             Arg3        : in out Arg3_T;
                             Arg4        : in out Arg4_T;
                             Call_Result : in out Subprogram_Call_Result.T);
procedure Aida.JSON_SAX_Parse (Arg1        : in out Arg1_T;
                               Arg2        : in out Arg2_T;
                               Arg3        : in out Arg3_T;
                               Arg4        : in out Arg4_T;
                               Contents    : Standard.String;
                               Call_Result : in out Subprogram_Call_Result.T) with
  Global => null,
  Pre    => not Call_Result.Has_Failed and Contents'Last < Int32_T'Last - 4;
