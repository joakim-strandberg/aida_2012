with Dynamic_Pools;
with Ada.Containers.Vectors;
with Aida.Deepend_XML_SAX_Parser;

pragma Elaborate_All (Aida.Deepend_XML_SAX_Parser);

package Aida.Deepend_XML_DOM_Parser is

   Default_Subpool : Dynamic_Pools.Dynamic_Pool (0);
   -- Allocations are done in subpools, not the default subpool

   type String_Ptr is access all String with
     Storage_Pool => Default_Subpool;

   Empty_String : aliased String := "";

   type Attribute is tagged limited private;

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
      XML_Tag,
      XML_Comment,
      XML_CDATA,
      XML_Text
     );

   type Attributes_Ref
     (
      E : not null access constant Attribute_Vectors.Vector
     )
   is limited null record with Implicit_Dereference => E;

   type Node_T;
   type Node_Ptr is access all Node_T with Storage_Pool => Default_Subpool;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Ptr,
      "="          => "=");

   type Child_Nodes_Ref
     (
      E : not null access constant Node_Vectors.Vector
     )
   is limited null record with
     Implicit_Dereference => E;

   type XML_Tag_T is tagged private;

   function Attributes (This : aliased XML_Tag_T) return Attributes_Ref with
     Global    => null;

   function Child_Nodes (This : aliased XML_Tag_T) return Child_Nodes_Ref with
     Global    => null;

   function Name (This : XML_Tag_T) return String with
     Global    => null;

   type Tag_Ref
     (
      E : not null access constant XML_Tag_T
     )
   is limited null record with
     Implicit_Dereference => E;

   type Node_T is tagged limited private;

   function Id (This : Node_T) return Node_Kind_Id with
     Global => null;

   function Tag (This : aliased Node_T) return Tag_Ref with
     Global    => null,
     Pre'Class => This.Id = XML_Tag;

   function Comment (This : Node_T) return String with
     Global    => null,
     Pre'Class => This.Id = XML_Comment;

   function CDATA (This : Node_T) return String with
     Global => null,
     Pre    => This.Id = XML_CDATA;

   function Text (This : Node_T) return String with
     Global => null,
     Pre    => This.Id = XML_Text;

   type DOM_Parser_T is tagged limited null record;

   procedure Parse
     (This        : in out DOM_Parser_T;
      Subpool     : in out Dynamic_Pools.Subpool_Handle;
      XML_Message : String;
      Call_Result : in out Aida.Call_Result;
      Root_Node   :    out Node_Ptr) with
     Global    => null,
       Pre'Class =>
         (not Call_Result.Has_Failed and
            XML_Message'Length > 0 and XML_Message'Last < Integer'Last - 4);

private

   type Attribute is tagged limited record
      My_Name  : String_Ptr;
      My_Value : String_Ptr;
   end record;

   function Name (This : Attribute) return String is
     (This.My_Name.all);

   function Value (This : Attribute) return String is
     (This.My_Value.all);

   type XML_Tag_T is tagged record
      My_Name        : String_Ptr;
      My_Child_Nodes : aliased Node_Vectors.Vector;
      My_Attributes  : aliased Attribute_Vectors.Vector;
   end record;

   function Name (This : XML_Tag_T) return String is
     (This.My_Name.all);

   function Child_Nodes (This : aliased XML_Tag_T) return Child_Nodes_Ref is
     ((E => This.My_Child_Nodes'Access));

   function Attributes (This : aliased XML_Tag_T) return Attributes_Ref is
     ((E => This.My_Attributes'Access));

   type Inner_Node_T (My_Id : Node_Kind_Id := XML_Tag) is record
      case My_Id is
         when XML_Tag  => My_Tag  : aliased XML_Tag_T;
         when XML_Comment |
              XML_CDATA |
              XML_Text => My_Text : not null String_Ptr := Empty_String'Access;
      end case;
   end record;

   type Node_T is tagged limited record
      Inner : Inner_Node_T;
   end record;

   function Id (This : Node_T) return Node_Kind_Id is (This.Inner.My_Id);

   function Tag (This : aliased Node_T) return Tag_Ref is
     ((E => This.Inner.My_Tag'Access));

   function Comment (This : Node_T) return String is
     (This.Inner.My_Text.all);

   function CDATA (This : Node_T) return String is
     (This.Inner.My_Text.all);

   function Text (This : Node_T) return String is
     (This.Inner.My_Text.all);

   type State_T is
     (
      Expecting_Object_Start, -- seems to only apply to the root start tag
      --  Expecting_Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End,
      Expecting_Default, -- Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End
      End_State
     );

   type SAX_Parser is new Aida.Deepend_XML_SAX_Parser.SAX_Parser with record
      Current_Nodes : Node_Vectors.Vector;
      -- The current node is the last Node pointed to in the container

      Root_Node     : Node_Ptr := null;
      State         : State_T := Expecting_Object_Start;
      Subpool       : Dynamic_Pools.Subpool_Handle;
   end record;

   overriding
   procedure Handle_Start_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_End_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_Text
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_Attribute
     (This            : in out SAX_Parser;
      Attribute_Name  : String;
      Attribute_Value : String;
      Call_Result     : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_Comment
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_CDATA
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

end Aida.Deepend_XML_DOM_Parser;
