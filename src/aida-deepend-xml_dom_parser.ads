with Ada.Containers.Vectors;

with Aida.XML_SAX_Parse;

pragma Elaborate_All (Aida.XML_SAX_Parse);

package Aida.Deepend.XML_DOM_Parser is

   type Attribute is limited private;

   function Name (This : Attribute) return String with
     Global => null;

   function Value (This : Attribute) return String with
     Global => null;

   type Attribute_Ptr is access all Attribute with
     Storage_Pool => Default_Subpool;

   type Attribute_Index is new Positive;

   package Attribute_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Attribute_Index,
      Element_Type => Attribute_Ptr,
      "="          => "=");

   type Node_Kind_Id is
     (
      Node_Kind_Tag,
      Node_Kind_Comment,
      Node_Kind_CDATA,
      Node_Kind_Text
     );

   type Node;
   type Node_Ptr is access all Node with Storage_Pool => Default_Subpool;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Ptr);

   type Attributes_Ref
     (
      Element : not null access constant Attribute_Vectors.Vector
     )
   is limited null record with Implicit_Dereference => Element;

   type Child_Nodes_Ref
     (
      Element : not null access constant Node_Vectors.Vector
     )
   is limited null record with Implicit_Dereference => Element;

   type XML_Tag is private;

   function Attributes (This : aliased XML_Tag) return Attributes_Ref with
     Global => null;

   function Child_Nodes (This : aliased XML_Tag) return Child_Nodes_Ref with
     Global => null;

   function Name (This : XML_Tag) return String with
     Global => null;

   type Node (Id : Node_Kind_Id := Node_Kind_Tag) is record
      case Id is
         when Node_Kind_Tag  =>
            Tag : aliased XML_Tag;
         when Node_Kind_Comment |
              Node_Kind_CDATA |
              Node_Kind_Text =>
            Text : not null String_Ptr := Empty_String'Access;
            --  It would be cool to specify that this can only be set once.
            --  To be improved in the future!
      end case;
   end record;

   procedure Parse
     (Subpool     : in out Dynamic_Pools.Subpool_Handle;
      XML_Message : String;
      Call_Result : in out Aida.Call_Result;
      Root_Node   :    out Node_Ptr) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          XML_Message'Length > 0 and XML_Message'Last < Integer'Last - 4);

private

   type Attribute is limited record
      My_Name  : String_Ptr;
      My_Value : String_Ptr;
   end record;

   type XML_Tag is record
      My_Name        : String_Ptr;
      My_Child_Nodes : aliased Node_Vectors.Vector;
      My_Attributes  : aliased Attribute_Vectors.Vector;
   end record;

   type State_T is
     (
      Expecting_Object_Start, -- seems to only apply to the root start tag
      --  Expecting_Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End,
      Expecting_Default, -- Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End
      End_State
     );

--     overriding
--     procedure Handle_Start_Tag
--       (This        : in out SAX_Parser;
--        Tag_Name    : String;
--        Call_Result : in out Aida.Call_Result) with
--       Global => null,
--       Pre    => not Call_Result.Has_Failed;
--
--     overriding
--     procedure Handle_End_Tag
--       (This        : in out SAX_Parser;
--        Tag_Name    : String;
--        Call_Result : in out Aida.Call_Result) with
--       Global => null,
--       Pre    => not Call_Result.Has_Failed;
--
--     overriding
--     procedure Handle_Text
--       (This        : in out SAX_Parser;
--        Value       : String;
--        Call_Result : in out Aida.Call_Result) with
--       Global => null,
--       Pre    => not Call_Result.Has_Failed;
--
--     overriding
--     procedure Handle_Attribute
--       (This            : in out SAX_Parser;
--        Attribute_Name  : String;
--        Attribute_Value : String;
--        Call_Result     : in out Aida.Call_Result) with
--       Global => null,
--       Pre    => not Call_Result.Has_Failed;
--
--     overriding
--     procedure Handle_Comment
--       (This        : in out SAX_Parser;
--        Value       : String;
--        Call_Result : in out Aida.Call_Result) with
--       Global => null,
--       Pre    => not Call_Result.Has_Failed;
--
--     overriding
--     procedure Handle_CDATA
--       (This        : in out SAX_Parser;
--        Value       : String;
--        Call_Result : in out Aida.Call_Result) with
--       Global => null,
--       Pre    => not Call_Result.Has_Failed;

end Aida.Deepend.XML_DOM_Parser;
