with Aida;
with Ada.Containers;
with Aida.Subprogram_Call_Result;
with System.Storage_Pools.Subpools;

use all type Ada.Containers.Count_Type;

package Aida.Deepend_XML_SAX_Parser is

   type SAX_Parser_T is tagged limited null record;

   procedure Start_Tag (This        : in out SAX_Parser_T;
                        Tag_Name    : in     Standard.String;
                        Call_Result : in out Subprogram_Call_Result.T) is null;

   procedure End_Tag (This        : in out SAX_Parser_T;
                      Tag_Name    : in     Standard.String;
                      Call_Result : in out Subprogram_Call_Result.T) is null;
   -- It is the responsibility of the implementor of End_Tag to verify
   -- that the tag name corresponds to the expected tag name.

   procedure Text (This        : in out SAX_Parser_T;
                   Value       : in     Standard.String;
                   Call_Result : in out Subprogram_Call_Result.T) is null;

   procedure Attribute (This            : in out SAX_Parser_T;
                        Attribute_Name  : in     Standard.String;
                        Attribute_Value : in     Standard.String;
                        Call_Result     : in out Subprogram_Call_Result.T) is null;

   procedure Comment (This        : in out SAX_Parser_T;
                      Value       : in     Standard.String;
                      Call_Result : in out Subprogram_Call_Result.T) is null;

   procedure CDATA (This        : in out SAX_Parser_T;
                    Value       : in     Standard.String;
                    Call_Result : in out Subprogram_Call_Result.T) is null;

   procedure Parse (This        : in out SAX_Parser_T;
                    Contents    : in     Standard.String;
                    Call_Result : in out Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed and Contents'Length > 0 and Contents'Last < Integer'Last - 4;

end Aida.Deepend_XML_SAX_Parser;
