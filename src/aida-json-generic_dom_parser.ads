with Aida.Integer_To_String_Map;
with Aida.Bounded_Vector;
with Aida.Subprogram_Call_Result;

pragma Elaborate_All (Aida.Integer_To_String_Map);
pragma Elaborate_All (Aida.Bounded_Vector);

generic
   Max_Chars        : Pos32_T;
   Max_Strings      : Pos32_T;
   Max_Nodes        : Pos32_T;
   Max_Array_Values : Pos32_T;
package Aida.JSON.Generic_DOM_Parser is

   package Int_To_String_Map is new Aida.Integer_To_String_Map (Max_Chars   => Max_Chars,
                                                                Max_Strings => Max_Strings,
                                                                Value_T     => String);

   type Node_Index_T is new Int32_T range 1..Max_Nodes;

   subtype Extended_Node_Id_T is Node_Index_T'Base range 0..Node_Index_T'Last;

   type Array_Index_T is new Int32_T range 1..Max_Array_Values;

   subtype Extended_Array_Id_T is Array_Index_T'Base range 0..Array_Index_T'Last;

   package Max_Indices_Def is

      type T is tagged limited private;

      function Node_Id_Max (This : T) return Extended_Node_Id_T;

      procedure Allocate_Node_Id (This : in out T;
                                  Id   : out Node_Index_T) with
        Global => null,
        Pre'Class    => Node_Id_Max (This) < Extended_Node_Id_T'Last,
        Post'Class   => Node_Id_Max (This) = Node_Id_Max (This)'Old + 1;

      procedure Clear (This : in out T) with
        Global       => null,
          Post'Class => Node_Id_Max (This) = 0;

   private

      type T is tagged limited record
         My_Node_Id_Max : Extended_Node_Id_T  := 0;
      end record;

      function Node_Id_Max (This : T) return Extended_Node_Id_T is (This.My_Node_Id_Max);

   end Max_Indices_Def;

   subtype Max_Indices_T is Max_Indices_Def.T;

   type JSON_Value_Id_T is (
                            JSON_No_Value, -- If the first node in the array has this value,
                                           -- it means the parsed JSON is an empty object "{}"
                            JSON_Integer,
                            JSON_Text
                           );

   type JSON_Value_T is record
      Id  : JSON_Value_Id_T := JSON_Text;
      Key : Int_To_String_Map.Key_T;
   end record;

   type JSON_Array_Value_Id_T is (
                                  JSON_Integer,
                                  JSON_Text
                                 );

   type JSON_Array_Value_T is record
      Id  : JSON_Array_Value_Id_T := JSON_Text;
      Key : Int_To_String_Map.Key_T;
   end record;

   type Array_Component_T is tagged limited private;

   function JSON_Value (This : Array_Component_T) return JSON_Array_Value_T with
     Global => null;

   function Default_Array_Component return Array_Component_T with
     Global => null;

   type Node_T is tagged limited private;

   function JSON_Key (This : Node_T) return Int_To_String_Map.Key_T;

   function JSON_Value (This : Node_T) return JSON_Value_T;

   function Next_Node (This : Node_T) return Extended_Node_Id_T;

   function Has_Next_Node (This : Node_T) return Boolean;

   function Default_Node return Node_T with
     Global => null;

   type Node_Array_T is array (Node_Index_T) of Node_T;

   type Array_T is array (Array_Index_T) of Array_Component_T;

   type Public_Part_T is abstract tagged limited record
      Nodes  : Node_Array_T        := (others => Default_Node);
      Arrays : Array_T             := (others => Default_Array_Component);
      Map    : Int_To_String_Map.T := Int_To_String_Map.Make;
   end record;

   type T is new Public_Part_T with private;

   procedure Parse (This         : in out T;
                    JSON_Message : String_T;
                    Call_Result  : in out Aida.Subprogram_Call_Result.T) with
     Global    => null,
     Pre'Class => not Call_Result.Has_Failed and JSON_Message'Last < Integer'Last - 4;

private

   type Array_Component_T is tagged limited record
      My_JSON_Value : JSON_Array_Value_T := (Id => JSON_Text, Key => Int_To_String_Map.Key_T'First);
      My_Next       : Extended_Array_Id_T := Extended_Array_Id_T'First;
   end record;

   function JSON_Value (This : Array_Component_T) return JSON_Array_Value_T is (This.My_JSON_Value);

   function Default_Array_Component return Array_Component_T is (
                                                                 My_JSON_Value => (Id => JSON_Text, Key => Int_To_String_Map.Key_T'First),
                                                                 My_Next       => Extended_Array_Id_T'First
                                                                );

   type Node_T is tagged limited record
      My_JSON_Key   : Int_To_String_Map.Key_T := Int_To_String_Map.Key_T'First;
      My_JSON_Value : JSON_Value_T := (Id => JSON_No_Value, Key => Int_To_String_Map.Key_T'First);
      My_Next_Node  : Extended_Node_Id_T := Extended_Node_Id_T'First;
   end record;

   function Default_Node return Node_T is (
                                           My_JSON_Key   => Int_To_String_Map.Key_T'First,
                                           My_JSON_Value => (Id => JSON_No_Value, Key => Int_To_String_Map.Key_T'First),
                                           My_Next_Node  => Extended_Node_Id_T'First
                                          );

   function JSON_Key (This : Node_T) return Int_To_String_Map.Key_T is (This.My_JSON_Key);

   function JSON_Value (This : Node_T) return JSON_Value_T is (This.My_JSON_Value);

   function Next_Node (This : Node_T) return Extended_Node_Id_T is (This.My_Next_Node);

   function Has_Next_Node (This : Node_T) return Boolean is (This.My_Next_Node /= Extended_Node_Id_T'First);

   type T is new Public_Part_T with record
      Max_Indices : Max_Indices_Def.T;
   end record;

   function Default_Node_Id return Node_Index_T is (1);

   MAX_IDS : constant := 10;

   package Node_Id_Vector is new Aida.Bounded_Vector (Max_Last_Index  => Int32_T'First + MAX_IDS,
                                                      Element_T       => Node_Index_T,
                                                      Default_Element => Default_Node_Id);

   type State_T is (
                    Expecting_Object_Start,
                    Expecting_Key_Or_Object_End_After_Object_Start,
                    Expecting_Value,
                    Expecting_Key_Or_Object_End,
                    End_State
                   );

   type Current_Ids_T is limited record
      Node_Ids : Node_Id_Vector.T;
      State    : State_T := Expecting_Object_Start;
   end record;

   procedure Start_Object (Storage     : in out Node_Array_T;
                           Max_Indices : in out Max_Indices_T;
                           Map         : in out Int_To_String_Map.T;
                           Current_Ids : in out Current_Ids_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure End_Object (Result      : in out Node_Array_T;
                         Max_Indices : in out Max_Indices_T;
                         Map         : in out Int_To_String_Map.T;
                         Current_Ids : in out Current_Ids_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Key (Result      : in out Node_Array_T;
                  Max_Indices : in out Max_Indices_T;
                  Map         : in out Int_To_String_Map.T;
                  Current_Ids : in out Current_Ids_T;
                  Name        : Aida.String_T;
                  Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure String_Value (Result      : in out Node_Array_T;
                           Max_Indices : in out Max_Indices_T;
                           Map         : in out Int_To_String_Map.T;
                           Current_Ids : in out Current_Ids_T;
                           Value       : Aida.String_T;
                           Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Integer_Value (Storage     : in out Node_Array_T;
                            Max_Indices : in out Max_Indices_T;
                            Map         : in out Int_To_String_Map.T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : in     Aida.String_T;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Real_Value (Storage     : in out Node_Array_T;
                         Max_Indices : in out Max_Indices_T;
                         Map         : in out Int_To_String_Map.T;
                         Current_Ids : in out Current_Ids_T;
                         Value       : in     Aida.String_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Boolean_Value (Storage     : in out Node_Array_T;
                            Max_Indices : in out Max_Indices_T;
                            Map         : in out Int_To_String_Map.T;
                            Current_Ids : in out Current_Ids_T;
                            Value       : in     Boolean;
                            Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Null_Value (Storage     : in out Node_Array_T;
                         Max_Indices : in out Max_Indices_T;
                         Map         : in out Int_To_String_Map.T;
                         Current_Ids : in out Current_Ids_T;
                         Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Array_Start (Result      : in out Node_Array_T;
                          Max_Indices : in out Max_Indices_T;
                          Map         : in out Int_To_String_Map.T;
                          Current_Ids : in out Current_Ids_T;
                          Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   procedure Array_End (Result      : in out Node_Array_T;
                        Max_Indices : in out Max_Indices_T;
                        Map         : in out Int_To_String_Map.T;
                        Current_Ids : in out Current_Ids_T;
                        Call_Result : in out Aida.Subprogram_Call_Result.T) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

end Aida.JSON.Generic_DOM_Parser;
