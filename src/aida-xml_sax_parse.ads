with Aida;
with Ada.Containers;
with Aida.Subprogram_Call_Result;

use all type Ada.Containers.Count_Type;

generic
   type Arg1_T is limited private;
   type Arg2_T is limited private;
   type Arg3_T is limited private;
   type Arg4_T is limited private;
   with procedure Start_Tag (Arg1        : in out Arg1_T;
                             Arg2        : in out Arg2_T;
                             Arg3        : in out Arg3_T;
                             Arg4        : in out Arg4_T;
                             Tag_Name    : in     Standard.String;
                             Call_Result : in out Subprogram_Call_Result.T);

   with procedure End_Tag (Arg1        : in out Arg1_T;
                           Arg2        : in out Arg2_T;
                           Arg3        : in out Arg3_T;
                           Arg4        : in out Arg4_T;
                           Tag_Name    : in     Standard.String;
                           Call_Result : in out Subprogram_Call_Result.T);
   -- It is the responsibility of the implementor of End_Tag to verify
   -- that the tag name corresponds to the expected tag name.

   with procedure Text (Arg1        : in out Arg1_T;
                        Arg2        : in out Arg2_T;
                        Arg3        : in out Arg3_T;
                        Arg4        : in out Arg4_T;
                        Value       : in     Standard.String;
                        Call_Result : in out Subprogram_Call_Result.T);

   with procedure Attribute (Arg1            : in out Arg1_T;
                             Arg2            : in out Arg2_T;
                             Arg3            : in out Arg3_T;
                             Arg4            : in out Arg4_T;
                             Attribute_Name  : in     Standard.String;
                             Attribute_Value : in     Standard.String;
                             Call_Result     : in out Subprogram_Call_Result.T);

   with procedure Comment (Arg1        : in out Arg1_T;
                           Arg2        : in out Arg2_T;
                           Arg3        : in out Arg3_T;
                           Arg4        : in out Arg4_T;
                           Value       : in     Standard.String;
                           Call_Result : in out Subprogram_Call_Result.T);

   with procedure CDATA (Arg1        : in out Arg1_T;
                         Arg2        : in out Arg2_T;
                         Arg3        : in out Arg3_T;
                         Arg4        : in out Arg4_T;
                         Value       : in     Standard.String;
                         Call_Result : in out Subprogram_Call_Result.T);

procedure Aida.XML_SAX_Parse (Arg1        : in out Arg1_T;
                              Arg2        : in out Arg2_T;
                              Arg3        : in out Arg3_T;
                              Arg4        : in out Arg4_T;
                              Contents    : in     Standard.String;
                              Call_Result : in out Subprogram_Call_Result.T) with
  Global => null,
  Pre    => not Call_Result.Has_Failed and Contents'Length > 0 and Contents'Last < Int32_T'Last - 4;
pragma Annotate (GNATProve, Terminating, Aida.XML_SAX_Parse);
