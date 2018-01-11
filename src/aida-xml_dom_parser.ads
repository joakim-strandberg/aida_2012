with Aida.Integer_To_String_Map;
with Aida.Bounded_Vector;
with Aida.Tagged_Bounded_Vector;
with Aida.Subprogram_Call_Result;

pragma Elaborate_All (Aida.Integer_To_String_Map);
pragma Elaborate_All (Aida.Bounded_Vector);
pragma Elaborate_All (Aida.Tagged_Bounded_Vector);

generic
   Max_Chars        : Pos32_T;
   Max_Strings      : Pos32_T;
   Max_Nodes        : Pos32_T;
   Max_Attributes   : Pos32_T;
package Aida.XML_DOM_Parser is

   package Int_To_String_Map is new Aida.Integer_To_String_Map (Max_Chars   => Max_Chars,
                                                                Max_Strings => Max_Strings,
                                                                Value_T     => String_T);

   type Node_Id_T is new Int32_T range 1..Max_Nodes;

   subtype Extended_Node_Id_T is Node_Id_T'Base range 0..Node_Id_T'Last;

   type Attribute_Id_T is new Int32_T range 1..Max_Attributes;

   subtype Extended_Attribute_Id_T is Attribute_Id_T'Base range 0..Attribute_Id_T'Last;

   package Max_Indices_Def is

      type T is tagged limited private;

      function Node_Id_Max (This : T) return Extended_Node_Id_T with
        Global => null;

      function Can_Allocate_Node_Id (This : T) return Boolean with
        Global => null;

      procedure Allocate_Node_Id (This : in out T;
                                  Id   : out Node_Id_T) with
        Global => null,
        Pre'Class    => This.Can_Allocate_Node_Id,
        Post'Class   => This.Node_Id_Max = This.Node_Id_Max'Old + 1;

      function Attribute_Id_Max (This : T) return Extended_Attribute_Id_T with
        Global => null;

      function Can_Allocate_Attribute_Id (This : T) return Boolean with
        Global => null;

      procedure Allocate_Attribute_Id (This : in out T;
                                       Id   : out Attribute_Id_T) with
        Global => null,
        Pre'Class    => This.Can_Allocate_Attribute_Id,
        Post'Class   => This.Attribute_Id_Max = This.Attribute_Id_Max'Old + 1;

      procedure Clear (This : in out T) with
        Global       => null,
          Post'Class => This.Node_Id_Max = 0 and This.Attribute_Id_Max = 0;

   private

      type T is tagged limited record
         My_Node_Id_Max       : Extended_Node_Id_T       := 0;
         My_Attribute_Id_Max  : Extended_Attribute_Id_T  := 0;
      end record;

      function Can_Allocate_Node_Id (This : T) return Boolean is (This.Node_Id_Max < Extended_Node_Id_T'Last);

      function Node_Id_Max (This : T) return Extended_Node_Id_T is (This.My_Node_Id_Max);

      function Can_Allocate_Attribute_Id (This : T) return Boolean is (This.Attribute_Id_Max < Extended_Attribute_Id_T'Last);

      function Attribute_Id_Max (This : T) return Extended_Attribute_Id_T is (This.My_Attribute_Id_Max);

   end Max_Indices_Def;

   subtype Max_Indices_T is Max_Indices_Def.T;

   type Attribute_T is tagged limited private;

   function Name (This : Attribute_T;
                  Map  : Int_To_String_Map.T) return String_T with
     Global    => null;

   function Value (This : Attribute_T;
                   Map  : Int_To_String_Map.T) return String_T with
     Global    => null;

   function Next_Attribute (This : Attribute_T) return Attribute_Id_T with
     Global    => null,
     Pre'Class => This.Has_Next_Attribute;

   function Has_Next_Attribute (This : Attribute_T) return Boolean with
     Global => null;

   type Node_Kind_Id_T is (
                           XML_Tag,
                           XML_Comment,
                           XML_CDATA,
                           XML_Text
                          );

   type Node_T is tagged private;

   function Id (This : Node_T) return Node_Kind_Id_T with
     Global => null;

   function Name (This : Node_T;
                  Map  : Int_To_String_Map.T) return String_T with
     Global    => null,
     Pre'Class => This.Id = XML_Tag;

   function Comment (This : Node_T;
                     Map  : Int_To_String_Map.T) return String_T with
     Global    => null,
       Pre'Class => This.Id = XML_Comment;

   function CDATA (This : Node_T;
                   Map  : Int_To_String_Map.T) return String_T with
     Global    => null,
       Pre'Class => This.Id = XML_CDATA;

   function Text (This : Node_T;
                  Map  : Int_To_String_Map.T) return String_T with
     Global    => null,
       Pre'Class => This.Id = XML_Text;

   function First_Attribute (This : Node_T) return Attribute_Id_T with
     Global    => null,
     Pre'Class => This.Id = XML_Tag and then This.Has_Attributes;

   function Has_Attributes (This : Node_T) return Boolean with
     Global    => null,
     Pre'Class => This.Id = XML_Tag;

   function First_Child_Node (This : Node_T) return Node_Id_T with
     Global    => null,
     Pre'Class => This.Id = XML_Tag and then This.Has_Child_Nodes;

   function Has_Child_Nodes (This : Node_T) return Boolean with
     Global => null,
     Pre'Class => This.Id = XML_Tag;

   function Next_Node (This : Node_T) return Node_Id_T with
     Global    => null,
     Pre'Class => This.Has_Next_Node;

   function Has_Next_Node (This : Node_T) return Boolean with
     Global => null;

   function Default_Node return Node_T with
     Global => null;

   function Default_Attribute return Attribute_T with
     Global => null;

   type Node_Array_T is array (Node_Id_T) of Node_T;

   type Attribute_Array_T is array (Attribute_Id_T) of Attribute_T;

   package Public_Part_Def is

      type Public_Part_T is tagged limited record
         Nodes      : Node_Array_T        := (others => Default_Node);
         Attributes : Attribute_Array_T   := (others => Default_Attribute);
         Map        : Int_To_String_Map.T := Int_To_String_Map.Make;
      end record;

   end Public_Part_Def;

   type T is new Public_Part_Def.Public_Part_T with private;

   procedure Parse (This        : in out T;
                    XML_Message : String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global    => null,
     Pre'Class => not Call_Result.Has_Failed and XML_Message'Length > 0 and XML_Message'Last < Int32_T'Last - 4;

private

   type Attribute_T is tagged limited record
      My_Name_Key       : Int_To_String_Map.Key_T := Int_To_String_Map.Key_T'First;
      My_Value_Key      : Int_To_String_Map.Key_T := Int_To_String_Map.Key_T'First;
      My_Next_Attribute : Extended_Attribute_Id_T := Extended_Attribute_Id_T'First;
   end record;

   function Name (This : Attribute_T;
                  Map  : Int_To_String_Map.T) return String_T is (Map.Value (This.My_Name_Key));

   function Value (This : Attribute_T;
                   Map  : Int_To_String_Map.T) return String_T is (Map.Value (This.My_Value_Key));

   function Next_Attribute (This : Attribute_T) return Attribute_Id_T is (This.My_Next_Attribute);

   function Has_Next_Attribute (This : Attribute_T) return Boolean is (This.My_Next_Attribute > Extended_Attribute_Id_T'First);

   function Default_Attribute return Attribute_T is ((My_Name_Key       => Int_To_String_Map.Key_T'First,
                                                      My_Value_Key      => Int_To_String_Map.Key_T'First,
                                                      My_Next_Attribute => Extended_Attribute_Id_T'First));

   type Inner_Node_T (My_Id : Node_Kind_Id_T := Node_Kind_Id_T'First) is record
      My_Next_Node : Extended_Node_Id_T := Extended_Node_Id_T'First;
      case My_Id is
         when XML_Tag     =>
            My_JSON_Key           : Int_To_String_Map.Key_T := Int_To_String_Map.Key_T'First;
            My_First_Child_Node   : Extended_Node_Id_T      := Extended_Node_Id_T'First;
            My_First_Attribute_Id : Extended_Attribute_Id_T := Extended_Attribute_Id_T'First;
         when XML_Comment | XML_CDATA | XML_Text =>
            My_Key                : Int_To_String_Map.Key_T;
      end case;
   end record;

   type Node_T is tagged record
      Inner : Inner_Node_T;
   end record;

   function Id (This : Node_T) return Node_Kind_Id_T is (This.Inner.My_Id);

   function Name (This : Node_T;
                  Map  : Int_To_String_Map.T) return String_T is (Map.Value (This.Inner.My_JSON_Key));

   function Comment (This : Node_T;
                     Map  : Int_To_String_Map.T) return String_T is (Map.Value (This.Inner.My_Key));

   function CDATA (This : Node_T;
                   Map  : Int_To_String_Map.T) return String_T is (Map.Value (This.Inner.My_Key));

   function Text (This : Node_T;
                  Map  : Int_To_String_Map.T) return String_T is (Map.Value (This.Inner.My_Key));

   function First_Attribute (This : Node_T) return Attribute_Id_T is (This.Inner.My_First_Attribute_Id);

   function Has_Attributes (This : Node_T) return Boolean is (This.Inner.My_First_Attribute_Id > Extended_Attribute_Id_T'First);

   function First_Child_Node (This : Node_T) return Node_Id_T is (This.Inner.My_First_Child_Node);

   function Has_Child_Nodes (This : Node_T) return Boolean is (This.Inner.My_First_Child_Node > Extended_Node_Id_T'First);

   function Next_Node (This : Node_T) return Node_Id_T is (This.Inner.My_Next_Node);

   function Has_Next_Node (This : Node_T) return Boolean is (This.Inner.My_Next_Node > Extended_Node_Id_T'First);

   function Default_Node return Node_T is ((Inner => (My_Id                 => XML_Tag,
                                                      My_JSON_Key           => Int_To_String_Map.Key_T'First,
                                                      My_First_Child_Node   => Extended_Node_Id_T'First,
                                                      My_First_Attribute_Id => Extended_Attribute_Id_T'First,
                                                      My_Next_Node          => Extended_Node_Id_T'First)));

   type T is new Public_Part_Def.Public_Part_T with record
      Max_Indices : Max_Indices_Def.T;
   end record;

   MAX_IDS : constant := 10;

   type Current_Node_T is record
      Node_Id           : Node_Id_T;
      Last_Child_Id     : Extended_Node_Id_T;
      Last_Attribute_Id : Extended_Attribute_Id_T;
      -- An xml tag  can have several child tags and this index points out the last one
   end record;

   function Default_Current_Node return Current_Node_T is ((Node_Id           => Node_Id_T'First,
                                                            Last_Child_Id     => Extended_Node_Id_T'First,
                                                            Last_Attribute_Id => Extended_Attribute_Id_T'First));

   package Node_Vector is new Aida.Tagged_Bounded_Vector (Max_Last_Index  => Int32_T'First + MAX_IDS,
                                                          Element_T       => Current_Node_T,
                                                          Default_Element => Default_Current_Node);

   type State_T is (
                    Expecting_Object_Start, -- seems to only apply to the root start tag
--                    Expecting_Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End,
                    Expecting_Default, -- Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End
                    End_State
                   );

   package Current_Ids_Def is

      type Current_Ids_T is tagged limited record
         Node_Ids : Node_Vector.T;
      end record;

      function Max_Node_Id (This : Current_Ids_T) return Node_Id_T with
        Global => null;

      procedure Append_Node (This : in out Current_Ids_T;
                             Node : in     Current_Node_T) with
        Global => null,
          Pre'Class  => This.Node_Ids.Last_Index < This.Node_Ids.Max_Index,
        Post'Class => This.Node_Ids.Last_Index = This.Node_Ids.Last_Index'Old + 1;

   end Current_Ids_Def;

   subtype Current_Ids_T is Current_Ids_Def.Current_Ids_T;

end Aida.XML_DOM_Parser;
