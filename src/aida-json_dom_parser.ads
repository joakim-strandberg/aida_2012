with Aida.Integer_To_String_Map;
with Aida.Bounded_Vector;

pragma Elaborate_All (Aida.Integer_To_String_Map);
pragma Elaborate_All (Aida.Bounded_Vector);

generic
   Max_Chars        : Positive;
   Max_Strings      : Positive;
   Max_Nodes        : Positive;
   Max_Array_Values : Positive;
package Aida.JSON_DOM_Parser is

   package Int_To_String_Map is new Aida.Integer_To_String_Map
     (Max_Chars   => Max_Chars,
      Max_Strings => Max_Strings,
      Value_T     => Standard.String);

   type Node_Index_T is new Integer range 1 .. Max_Nodes;

   subtype Extended_Node_Id is Node_Index_T'Base range 0 .. Node_Index_T'Last;

   type Array_Index is new Integer range 1 .. Max_Array_Values;

   subtype Extended_Array_Id is Array_Index'Base range 0 .. Array_Index'Last;

   package Max_Indices_Def is

      type T is tagged limited private with
        Default_Initial_Condition =>
          Node_Id_Max (T) = 0 and Array_Id_Max (T) = 0;

      function Node_Id_Max (This : T) return Extended_Node_Id with
        Global => null;

      procedure Allocate_Node_Id (This : in out T;
                                  Id   : out Node_Index_T) with
        Global => null,
        Pre'Class    => This.Node_Id_Max < Extended_Node_Id'Last,
        Post'Class   => This.Node_Id_Max = This.Node_Id_Max'Old + 1;

      function Array_Id_Max (This : T) return Extended_Array_Id with
        Global => null;

      procedure Allocate_Array_Id (This : in out T;
                                   Id   : out Array_Index) with
        Global      => null,
        Pre'Class   => Array_Id_Max (This) < Extended_Array_Id'Last,
        Post'Class  => Array_Id_Max (This) = Array_Id_Max (This)'Old + 1;

      procedure Clear (This : in out T) with
        Global       => null,
          Post'Class => This.Node_Id_Max = 0 and This.Array_Id_Max = 0;

   private

      type T is tagged limited record
         My_Node_Id_Max  : Extended_Node_Id  := 0;
         My_Array_Id_Max : Extended_Array_Id := 0;
      end record;

      function Node_Id_Max (This : T) return Extended_Node_Id is
        (This.My_Node_Id_Max);

      function Array_Id_Max (This : T) return Extended_Array_Id is
        (This.My_Array_Id_Max);

   end Max_Indices_Def;

   subtype Max_Indices_T is Max_Indices_Def.T;

   type JSON_Value_Id_T is
     (
      JSON_No_Value,
      --  If the first node in the array has this value,
      --  it means the parsed JSON is an empty object "{}"

      JSON_Integer,
      JSON_Real,
      JSON_Boolean,
      JSON_Text,
      JSON_Null,
      JSON_Object,
      JSON_Array
     );

   type JSON_Value_T (Id : JSON_Value_Id_T := JSON_No_Value) is record
      case Id is
         when JSON_No_Value | JSON_Null            => null;
         when JSON_Integer | JSON_Real | JSON_Text =>
            Key : Int_To_String_Map.Key_T
            := Int_To_String_Map.Key_T'First;

         when JSON_Object  => Node_Id  : Node_Index_T := Node_Index_T'First;
         when JSON_Array   => Array_Id : Array_Index := Array_Index'First;
         when JSON_Boolean => Is_True  : Boolean := False;
      end case;
   end record;

   type Array_Component_T is tagged limited private;

   function Next (This : Array_Component_T) return Array_Index with
     Global    => null,
     Pre'Class => This.Has_Next;

   function Has_Next (This : Array_Component_T) return Boolean with
     Global => null;

   function JSON_Value (This : Array_Component_T) return JSON_Value_T with
     Global => null;

   function Default_Array_Component return Array_Component_T with
     Global => null;

   type Node_T is tagged limited private;

   function JSON_Key (This : Node_T) return Int_To_String_Map.Key_T
     with Global => null;

   function JSON_Value (This : Node_T) return JSON_Value_T
     with Global => null;

   function Next_Node (This : Node_T) return Extended_Node_Id
     with Global => null;

   function Has_Next_Node (This : Node_T) return Boolean
     with Global => null;

   function Default_Node return Node_T with
     Global => null;

   type Node_Array_T is array (Node_Index_T) of Node_T;

   type Array_T is array (Array_Index) of Array_Component_T;

   package Public_Part_Def is

      type Public_Part_T is tagged limited record
         Nodes  : Node_Array_T        := (others => Default_Node);
         Arrays : Array_T             := (others => Default_Array_Component);
         Map    : Int_To_String_Map.T := Int_To_String_Map.Make;
      end record;

   end Public_Part_Def;

   type T is new Public_Part_Def.Public_Part_T with private;

   procedure Parse (This         : in out T;
                    JSON_Message : Standard.String;
                    Call_Result  : in out Aida.Call_Result) with
     Global    => null,
       Pre'Class =>
         not Call_Result.Has_Failed and JSON_Message'Last < Integer'Last - 4;

private

   type Array_Component_T is tagged limited record
      My_JSON_Value : JSON_Value_T := (Id => JSON_Text,
                                       Key => Int_To_String_Map.Key_T'First);
      My_Next       : Extended_Array_Id := Extended_Array_Id'First;
   end record;

   type Node_T is tagged limited record
      My_JSON_Key   : Int_To_String_Map.Key_T := Int_To_String_Map.Key_T'First;
      My_JSON_Value : JSON_Value_T := (Id => JSON_No_Value);
      My_Next_Node  : Extended_Node_Id := Extended_Node_Id'First;
   end record;

   type T is new Public_Part_Def.Public_Part_T with record
      Max_Indices : Max_Indices_Def.T;
   end record;

   type Inside_Construct_Id is (
                                Node_Construct,
                                Array_Construct
                                );

   type Inside_Construct (Id : Inside_Construct_Id := Node_Construct) is record
      case Id is
         when Node_Construct  => Node_Id : Node_Index_T;
         when Array_Construct => Array_Id : Array_Index;
      end case;
   end record;

   function Default_Inside_Construct return Inside_Construct is
     ((Id      => Node_Construct,
       Node_Id => 1));

   MAX_IDS : constant := 10;

   package Node_Id_Vector is new Aida.Bounded_Vector
     (Max_Last_Index  => Integer'First + MAX_IDS,
      Element_T       => Inside_Construct,
      Default_Element => Default_Inside_Construct);

   use all type Node_Id_Vector.T;

   type State_T is (
                    Expecting_Object_Start,
                    Expecting_Key_Or_Object_End_After_Object_Start,
                    Expecting_Value,
                    Expecting_Array_Value_After_Array_Start,
                    Expecting_Key_Or_Object_End,
                    End_State
                   );

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
          Pre'Class  => Last_Index (This.Node_Ids) < Max_Index (This.Node_Ids),
        Post'Class =>
          Last_Index (This.Node_Ids) = Last_Index (This.Node_Ids)'Old + 1;

      procedure Append_Array_Id (This     : in out Current_Ids_T;
                                 Array_Id : in     Array_Index) with
        Global => null,
          Pre'Class  => Last_Index (This.Node_Ids) < Max_Index (This.Node_Ids),
        Post'Class =>
          Last_Index (This.Node_Ids) = Last_Index (This.Node_Ids)'Old + 1;

   end Current_Ids_Def;

   type Arg3_T is (
                   Dummy_Value
                  );

   procedure Start_Object (This        : in out Public_Part_Def.Public_Part_T;
                           Max_Indices : in out Max_Indices_T;
                           Arg3        : in out Arg3_T;
                           Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                           Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained) and
            (for all I in This.Arrays'Range =>
                   not This.Arrays (I).My_JSON_Value'Constrained));

   procedure End_Object (This        : in out Public_Part_Def.Public_Part_T;
                         Max_Indices : in out Max_Indices_T;
                         Arg3        : in out Arg3_T;
                         Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                         Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure Key (This        : in out Public_Part_Def.Public_Part_T;
                  Max_Indices : in out Max_Indices_T;
                  Arg3        : in out Arg3_T;
                  Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                  Name        : Standard.String;
                  Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure String_Value (This        : in out Public_Part_Def.Public_Part_T;
                           Max_Indices : in out Max_Indices_T;
                           Arg3        : in out Arg3_T;
                           Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                           Value       : Standard.String;
                           Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure Integer_Value (This        : in out Public_Part_Def.Public_Part_T;
                            Max_Indices : in out Max_Indices_T;
                            Arg3        : in out Arg3_T;
                            Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                            Value       : in     Standard.String;
                            Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure Real_Value (This        : in out Public_Part_Def.Public_Part_T;
                         Max_Indices : in out Max_Indices_T;
                         Arg3        : in out Arg3_T;
                         Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                         Value       : in     Standard.String;
                         Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure Boolean_Value (This        : in out Public_Part_Def.Public_Part_T;
                            Max_Indices : in out Max_Indices_T;
                            Arg3        : in out Arg3_T;
                            Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                            Value       : in     Boolean;
                            Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure Null_Value (This        : in out Public_Part_Def.Public_Part_T;
                         Max_Indices : in out Max_Indices_T;
                         Arg3        : in out Arg3_T;
                         Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                         Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure Array_Start (This        : in out Public_Part_Def.Public_Part_T;
                          Max_Indices : in out Max_Indices_T;
                          Arg3        : in out Arg3_T;
                          Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                          Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

   procedure Array_End (This        : in out Public_Part_Def.Public_Part_T;
                        Max_Indices : in out Max_Indices_T;
                        Arg3        : in out Arg3_T;
                        Current_Ids : in out Current_Ids_Def.Current_Ids_T;
                        Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          (for all I in This.Nodes'Range =>
                 not This.Nodes (I).My_JSON_Value'Constrained));

end Aida.JSON_DOM_Parser;
