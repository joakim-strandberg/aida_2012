with Aida.Integer_To_String_Map;
with Aida.Bounded_Vector;
with Aida.Subprogram_Call_Result;

pragma Elaborate_All (Aida.Integer_To_String_Map);
pragma Elaborate_All (Aida.Bounded_Vector);

generic
   Max_Chars        : Pos32_T;
   Max_Strings      : Pos32_T;
   Max_Nodes        : Pos32_T;
   Max_Attributes   : Pos32_T;
   Max_Child_Nodes  : Pos32_T;
package Aida.XML.Generic_DOM_Parser is

   package Int_To_String_Map is new Aida.Integer_To_String_Map (Max_Chars   => Max_Chars,
                                                                Max_Strings => Max_Strings,
                                                                Value_T     => String);

   type Node_Id_T is new Int32_T range 1..Max_Nodes;

   subtype Extended_Node_Id_T is Node_Id_T'Base range 0..Node_Id_T'Last;

   type Attribute_Id_T is new Int32_T range 1..Max_Attributes;

   subtype Extended_Attribute_Id_T is Attribute_Id_T'Base range 0..Attribute_Id_T'Last;

   type Child_Node_Id_T is new Int32_T range 1..Max_Child_Nodes;

   subtype Extended_Child_Node_Id_T is Child_Node_Id_T'Base range 0..Child_Node_Id_T'Last;

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

      function Child_Node_Id_Max (This : T) return Extended_Child_Node_Id_T with
        Global => null;

      function Can_Allocate_Child_Node_Id (This : T) return Boolean with
        Global => null;

      procedure Allocate_Child_Node_Id (This : in out T;
                                        Id   : out Child_Node_Id_T) with
        Global => null,
        Pre'Class    => This.Can_Allocate_Child_Node_Id,
        Post'Class   => This.Child_Node_Id_Max = This.Child_Node_Id_Max'Old + 1;

      procedure Clear (This : in out T) with
        Global       => null,
          Post'Class => This.Node_Id_Max = 0;

   private

      type T is tagged limited record
         My_Node_Id_Max       : Extended_Node_Id_T       := 0;
         My_Attribute_Id_Max  : Extended_Attribute_Id_T  := 0;
         My_Child_Node_Id_Max : Extended_Child_Node_Id_T := 0;
      end record;

      function Can_Allocate_Node_Id (This : T) return Boolean is (This.Node_Id_Max < Extended_Node_Id_T'Last);

      function Node_Id_Max (This : T) return Extended_Node_Id_T is (This.My_Node_Id_Max);

      function Can_Allocate_Attribute_Id (This : T) return Boolean is (This.Attribute_Id_Max < Extended_Attribute_Id_T'Last);

      function Attribute_Id_Max (This : T) return Extended_Attribute_Id_T is (This.My_Attribute_Id_Max);

      function Can_Allocate_Child_Node_Id (This : T) return Boolean is (This.Child_Node_Id_Max < Extended_Child_Node_Id_T'Last);

      function Child_Node_Id_Max (This : T) return Extended_Child_Node_Id_T is (This.My_Child_Node_Id_Max);

   end Max_Indices_Def;

   subtype Max_Indices_T is Max_Indices_Def.T;

   type Node_T is tagged limited private;

   function Name (This : Node_T;
                  Map  : Int_To_String_Map.T) return String_T with
     Global => null;

   function Default_Node return Node_T with
     Global => null;

   type Node_Array_T is array (Node_Id_T) of Node_T;

   package Public_Part_Def is

      type Public_Part_T is tagged limited record
         Nodes  : Node_Array_T        := (others => Default_Node);
         Map    : Int_To_String_Map.T := Int_To_String_Map.Make;
      end record;

   end Public_Part_Def;

   type T is new Public_Part_Def.Public_Part_T with private;

   procedure Parse (This         : in out T;
                    JSON_Message : String_T;
                    Call_Result  : in out Aida.Subprogram_Call_Result.T) with
     Global    => null,
     Pre'Class => not Call_Result.Has_Failed and JSON_Message'Last < Integer'Last - 4;

private

   type Node_T is tagged limited record
      My_JSON_Key   : Int_To_String_Map.Key_T := Int_To_String_Map.Key_T'First;
--      My_Next_Node  : Extended_Node_Id_T := Extended_Node_Id_T'First;
   end record;

   type T is new Public_Part_Def.Public_Part_T with record
      Max_Indices : Max_Indices_Def.T;
   end record;

   MAX_IDS : constant := 10;

   package Node_Id_Vector is new Aida.Bounded_Vector (Max_Last_Index  => Int32_T'First + MAX_IDS,
                                                      Element_T       => Node_Id_T,
                                                      Default_Element => Default_Node);

   package Current_Ids_Def is

      type Current_Ids_T is tagged limited record
         Node_Ids : Node_Id_Vector.T;
         State    : State_T := Expecting_Object_Start;
      end record;

      function Max_Node_Id (This : Current_Ids_T) return Node_Index_T with
        Global => null;

      procedure Append_Node_Id (This    : in out Current_Ids_T;
                                Node_Id : in     Node_Index_T) with
        Global => null,
          Pre'Class  => Node_Id_Vector.Last_Index (This.Node_Ids) < Node_Id_Vector.Max_Index (This.Node_Ids),
        Post'Class => Node_Id_Vector.Last_Index (This.Node_Ids) = Node_Id_Vector.Last_Index (This.Node_Ids)'Old + 1;

   end Current_Ids_Def;

end Aida.XML.Generic_DOM_Parser;
