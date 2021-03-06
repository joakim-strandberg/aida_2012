with Ada.Containers;
with Aida.Bounded_Vector;

generic
   type Key_T is private;
   type Element_T is private;

   with function Default_Key return Key_T;
   with function Default_Element return Element_T;

   with function Hash (Key : Key_T) return Ada.Containers.Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_T) return Boolean;

   Max_Hash_Map_Size : Max_Hash_Map_Size_T;

   Max_Collision_List_Size : Integer := 0;
package Aida.Bounded_Hash_Map with Pure is

   use all type Integer;
   use all type Ada.Containers.Hash_Type;

   type T is limited private with
     Default_Initial_Condition => Used_Capacity (T) = 0;

   procedure Insert (This        : in out T;
                     Key         : Key_T;
                     New_Element : Element_T) with
     Global => null,
     Pre    => Used_Capacity (This) < Max_Collision_List_Size,
     Post   =>
       (Used_Capacity (This)'Old = Used_Capacity (This) or
              Used_Capacity (This) = Used_Capacity (This)'Old + 1);

   function Element (This : T;
                     Key  : Key_T) return Element_T with
     Global => null,
     Pre    => Exists (This, Key);

   function Exists (This : T;
                    Key  : Key_T) return Boolean with
     Global => null;

   function Used_Capacity (This : T) return Natural with
     Global => null,
     Post   => Used_Capacity'Result <= Max_Collision_List_Size;

   type Find_Element_Result_T (Exists : Boolean) is
      record
         case Exists is
            when True  => Element : Element_T;
            when False => null;
         end case;
      end record;

   function Find_Element (This : T;
                          Key  : Key_T) return Find_Element_Result_T with
     Global => null;

   procedure Delete (This : in out T;
                     Key  : Key_T) with
     Global => null,
     Pre    => Exists (This, Key);

private

   type Node_T is
      record
         Key     : Key_T;
         Element : Element_T;
      end record;

   type Nullable_Node_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Node_T;
         when False => null;
      end case;
   end record;

   subtype Bucket_Index_T is Ada.Containers.Hash_Type range
     Ada.Containers.Hash_Type'(0) ..
     Ada.Containers.Hash_Type (Max_Hash_Map_Size - 1);

   type Bucket_Array_T is array (Bucket_Index_T) of Nullable_Node_T;

--   type Collision_Index_T is new Integer range 1..Max_Collision_List_Size;

   function Default_Node return Node_T;

   package Collision_Vector is new Aida.Bounded_Vector
     (Max_Last_Index  => Integer'First + Max_Collision_List_Size,
      Element_T       => Node_T,
      Default_Element => Default_Node);

   use all type Collision_Vector.T;

   type T is
      record
         Buckets        : Bucket_Array_T := (others => (Exists => False));
         Collision_List : Collision_Vector.T := Collision_Vector.Empty_Vector;
      end record;

   function Used_Capacity (This : T) return Natural is
     (Natural ((Collision_Vector.Last_Index (This.Collision_List) + 1)
                    - Collision_Vector.First_Index (This.Collision_List)));

   function Normalize_Index
     (H : Ada.Containers.Hash_Type) return Bucket_Index_T
   is
     (if H < Ada.Containers.Hash_Type (Max_Hash_Map_Size) then
           Bucket_Index_T (H)
      else
         Bucket_Index_T
        (H - ((H / Ada.Containers.Hash_Type (Max_Hash_Map_Size))) *
             Ada.Containers.Hash_Type (Max_Hash_Map_Size)));

--     function Exists (This : T;
--                      Key  : Key_T) return Boolean is
--       (This.Buckets (Normalize_Index (Hash (Key))).Exists and then
--       (This.Buckets (Normalize_Index (Hash (Key))).Value.Key = Key));

   function Exists (This : T;
                    Key  : Key_T) return Boolean is
     ((This.Buckets (Normalize_Index (Hash (Key))).Exists) and then
      (This.Buckets (Normalize_Index (Hash (Key))).Value.Key = Key or
        (for some I in Collision_Vector.Index_T range
             First_Index (This.Collision_List) ..
             Last_Index (This.Collision_List) =>
              Element (This.Collision_List, I).Key = Key)));

end Aida.Bounded_Hash_Map;
