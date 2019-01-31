with Ada.Containers;

package Aida.Deepend_XML_SAX_Parser is

   use all type Ada.Containers.Count_Type;

   type SAX_Parser is tagged limited null record;

   procedure Handle_Start_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : in     Standard.String;
      Call_Result : in out Aida.Call_Result) is null;

   procedure Handle_End_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : in     Standard.String;
      Call_Result : in out Aida.Call_Result) is null;
   --  It is the responsibility of the implementor of End_Tag to verify
   --  that the tag name corresponds to the expected tag name.

   procedure Handle_Text
     (This        : in out SAX_Parser;
      Value       : in     Standard.String;
      Call_Result : in out Aida.Call_Result) is null;

   procedure Handle_Attribute
     (This            : in out SAX_Parser;
      Attribute_Name  : in     Standard.String;
      Attribute_Value : in     Standard.String;
      Call_Result     : in out Aida.Call_Result) is null;

   procedure Handle_Comment
     (This        : in out SAX_Parser;
      Value       : in     Standard.String;
      Call_Result : in out Aida.Call_Result) is null;

   procedure Handle_CDATA
     (This        : in out SAX_Parser;
      Value       : in     Standard.String;
      Call_Result : in out Aida.Call_Result) is null;

   procedure Parse
     (This        : in out SAX_Parser;
      Contents    : in     Standard.String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          Contents'Length > 0 and Contents'Last < Integer'Last - 4);

end Aida.Deepend_XML_SAX_Parser;
