with Aida;
with Ada.Containers;

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
                             Tag_Name    : in     String;
                             Call_Result : in out Aida.Call_Result);

   with procedure End_Tag (Arg1        : in out Arg1_T;
                           Arg2        : in out Arg2_T;
                           Arg3        : in out Arg3_T;
                           Arg4        : in out Arg4_T;
                           Tag_Name    : in     String;
                           Call_Result : in out Aida.Call_Result);
   --  It is the responsibility of the implementor of End_Tag to verify
   --  that the tag name corresponds to the expected tag name.

   with procedure Text (Arg1        : in out Arg1_T;
                        Arg2        : in out Arg2_T;
                        Arg3        : in out Arg3_T;
                        Arg4        : in out Arg4_T;
                        Value       : in     String;
                        Call_Result : in out Aida.Call_Result);

   with procedure Attribute (Arg1            : in out Arg1_T;
                             Arg2            : in out Arg2_T;
                             Arg3            : in out Arg3_T;
                             Arg4            : in out Arg4_T;
                             Attribute_Name  : in     String;
                             Attribute_Value : in     String;
                             Call_Result     : in out Aida.Call_Result);

   with procedure Comment (Arg1        : in out Arg1_T;
                           Arg2        : in out Arg2_T;
                           Arg3        : in out Arg3_T;
                           Arg4        : in out Arg4_T;
                           Value       : in     String;
                           Call_Result : in out Aida.Call_Result);

   with procedure CDATA (Arg1        : in out Arg1_T;
                         Arg2        : in out Arg2_T;
                         Arg3        : in out Arg3_T;
                         Arg4        : in out Arg4_T;
                         Value       : in     String;
                         Call_Result : in out Aida.Call_Result);

procedure Aida.XML_SAX_Parse (Arg1        : in out Arg1_T;
                              Arg2        : in out Arg2_T;
                              Arg3        : in out Arg3_T;
                              Arg4        : in out Arg4_T;
                              Contents    : in     String;
                              Call_Result : in out Aida.Call_Result) with
  Global => null,
  Pre    =>
    (not Call_Result.Has_Failed and Contents'Length > 0 and
       Contents'Last < Integer'Last - 4);
pragma Annotate (GNATProve, Terminating, Aida.XML_SAX_Parse);
