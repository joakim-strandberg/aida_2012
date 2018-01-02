with Aida.Subprogram_Call_Result;
with Dynamic_Pools;
with Ada.Containers.Vectors;
with Aida.Deepend_XML_SAX_Parser;

pragma Elaborate_All (Aida.Deepend_XML_SAX_Parser);

package Aida.Deepend_XML_DOM_Parser is

   Default_Subpool : Dynamic_Pools.Dynamic_Pool (0);
   -- Allocations are done in subpools, not the default subpool

   type String_Ptr is access all Aida.String_T with Storage_Pool => Default_Subpool;

   Empty_String : aliased Aida.String_T := "";

   type Attribute_T is tagged limited private;

   function Name (This : Attribute_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Attribute_T) return Boolean with
     Global => null;

   function Value (This : Attribute_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Value;

   function Exists_Value (This : Attribute_T) return Boolean with
     Global => null;

   type Attribute_Ptr is access all Attribute_T with Storage_Pool => Default_Subpool;

   package Attribute_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                            Element_Type => Attribute_Ptr,
                                                            "="          => "=");

   type Node_Kind_Id_T is (
                           XML_Tag,
                           XML_Comment,
                           XML_CDATA,
                           XML_Text
                          );

   type Attributes_Ref (E : not null access constant Attribute_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Node_T;
   type Node_Ptr is access all Node_T with Storage_Pool => Default_Subpool;

   package Child_Node_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => Node_Ptr,
                                                             "="          => "=");

   type Child_Nodes_Ref (E : not null access constant Child_Node_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Node_T is tagged limited private;

   function Id (This : Node_T) return Node_Kind_Id_T with
     Global => null;

   function Child_Nodes (This : aliased Node_T) return Child_Nodes_Ref with
     Global    => null,
     Pre'Class => This.Id = XML_Tag;

   function Attributes (This : aliased Node_T) return Attributes_Ref with
     Global    => null,
     Pre'Class => This.Id = XML_Tag;

   function Name (This : Node_T) return Aida.String_T with
     Global    => null,
     Pre'Class => This.Id = XML_Tag and then This.Exists_Name;

   function Exists_Name (This : Node_T) return Boolean with
     Global      => null,
       Pre'Class => This.Id = XML_Tag;

   function Comment (This : Node_T) return Aida.String_T with
     Global => null,
     Pre'Class    => This.Id = XML_Comment and then This.Exists_Comment;

   function Exists_Comment (This : Node_T) return Boolean with
     Global => null,
     Pre'Class    => This.Id = XML_Comment;

   function CDATA (This : Node_T) return Aida.String_T with
     Global => null,
     Pre    => This.Id = XML_CDATA and then This.Exists_CDATA;

   function Exists_CDATA (This : Node_T) return Boolean with
     Global => null,
     Pre    => This.Id = XML_CDATA;

   function Text (This : Node_T) return Aida.String_T with
     Global => null,
     Pre    => This.Id = XML_Text and then This.Exists_Text;

   function Exists_Text (This : Node_T) return Boolean with
     Global => null,
       Pre'Class => This.Id = XML_Text;

   type DOM_Parser_T is tagged limited null record;

   procedure Parse (This        : in out DOM_Parser_T;
                    Subpool     : in out Dynamic_Pools.Subpool_Handle;
                    XML_Message : String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T;
                    Root_Node   :    out Node_Ptr) with
     Global    => null,
     Pre'Class => not Call_Result.Has_Failed and XML_Message'Length > 0 and XML_Message'Last < Integer'Last - 4;

private

   type Nullable_String_Ptr (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : not null String_Ptr := Empty_String'Access;
         when False => null;
      end case;
   end record;

   type Nullable_Boolean_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   type Attribute_T is tagged limited record
      My_Name  : Nullable_String_Ptr;
      My_Value : Nullable_String_Ptr;
   end record;

   procedure Set_Name (This    : in out Attribute_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   procedure Set_Value (This    : in out Attribute_T;
                        Value   : Aida.String_T;
                        Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Value,
     Post   => This.Exists_Value and This.Value = Value;

   function Name (This : Attribute_T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : Attribute_T) return Boolean is (This.My_Name.Exists);

   function Value (This : Attribute_T) return Aida.String_T is (This.My_Value.Value.all);

   function Exists_Value (This : Attribute_T) return Boolean is (This.My_Value.Exists);

   type Inner_Node_T (My_Id : Node_Kind_Id_T := XML_Tag) is record
      case My_Id is
         when XML_Tag =>
            My_Name : Nullable_String_Ptr;
            My_Child_Nodes : aliased Child_Node_Vectors.Vector;
            My_Attributes  : aliased Attribute_Vectors.Vector;
         when XML_Comment | XML_CDATA | XML_Text =>
            My_Text : Nullable_String_Ptr;
      end case;
   end record;

   type Node_T is tagged limited record
      Inner : Inner_Node_T;
   end record;

   procedure Set_Name (This  : in out Node_T;
                       Value : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => This.Id = XML_Tag and then (not This.Exists_Name),
     Post   => This.Exists_Name and This.Name = Value;

--     procedure Set_Comment (This  : in out Node_T;
--                            Value : Aida.String_T;
--                            Subpool : Dynamic_Pools.Subpool_Handle) with
--       Global => null,
--       Pre    => This.Id = XML_Comment and then not This.Exists_Comment,
--       Post   => This.Exists_Comment and This.Comment = Value;
--
--     procedure Set_CDATA (This  : in out Node_T;
--                          Value : Aida.String_T;
--                          Subpool : Dynamic_Pools.Subpool_Handle) with
--       Global => null,
--       Pre    => This.Id = XML_CDATA and then not This.Exists_CDATA,
--       Post   => This.Exists_CDATA and This.CDATA = Value;
--
--     procedure Set_Text (This  : in out Node_T;
--                         Value : Aida.String_T;
--                         Subpool : Dynamic_Pools.Subpool_Handle) with
--       Global => null,
--       Pre    => not This.Exists_Text,
--       Post   => This.Exists_Text and This.Text = Value;

   function Child_Nodes (This : aliased Node_T) return Child_Nodes_Ref is ((E => This.Inner.My_Child_Nodes'Access));

   function Attributes (This : aliased Node_T) return Attributes_Ref is ((E => This.Inner.My_Attributes'Access));

   function Id (This : Node_T) return Node_Kind_Id_T is (This.Inner.My_Id);

   function Name (This : Node_T) return Aida.String_T is (This.Inner.My_Name.Value.all);

   function Exists_Name (This : Node_T) return Boolean is (This.Inner.My_Name.Exists);

   function Comment (This : Node_T) return Aida.String_T is (This.Inner.My_Text.Value.all);

   function Exists_Comment (This : Node_T) return Boolean is (This.Inner.My_Text.Exists);

   function CDATA (This : Node_T) return Aida.String_T is (This.Inner.My_Text.Value.all);

   function Exists_CDATA (This : Node_T) return Boolean is (This.Inner.My_Text.Exists);

   function Text (This : Node_T) return Aida.String_T is (This.Inner.My_Text.Value.all);

   function Exists_Text (This : Node_T) return Boolean is (This.Inner.My_Text.Exists);

   type State_T is (
                    Expecting_Object_Start, -- seems to only apply to the root start tag
--                    Expecting_Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End,
                    Expecting_Default, -- Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End
                    End_State
                   );

--     type This_T is limited record
--        Root_Node     : Node_Ptr;
--        Current_Nodes : Child_Node_Vectors.Vector; -- The current node is the last Node pointed to in the container
--        State         : State_T;
--     end record;

   type SAX_Parser_T is new Aida.Deepend_XML_SAX_Parser.SAX_Parser_T with record
      Root_Node     : Node_Ptr := null;
      Current_Nodes : Child_Node_Vectors.Vector; -- The current node is the last Node pointed to in the container
      State         : State_T := Expecting_Object_Start;
      Subpool       : Dynamic_Pools.Subpool_Handle;
   end record;

   overriding
   procedure Start_Tag (This        : in out SAX_Parser_T;
                        Tag_Name    : Aida.String_T;
                        Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure End_Tag (This        : in out SAX_Parser_T;
                      Tag_Name    : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Text (This        : in out SAX_Parser_T;
                   Value       : Aida.String_T;
                   Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Attribute (This            : in out SAX_Parser_T;
                        Attribute_Name  : Aida.String_T;
                        Attribute_Value : Aida.String_T;
                        Call_Result     : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Comment (This        : in out SAX_Parser_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure CDATA (This        : in out SAX_Parser_T;
                    Value       : Aida.String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

end Aida.Deepend_XML_DOM_Parser;
