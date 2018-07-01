with Aida.Bounded_Vector;

-- DO NOT USE. Uncommenting the Exists function below triggers a GNAT compiler bug.
-- Will continue developing this package sometime in the future... during 2017 or later???
generic
   type Key_T is private;
   type Element_T is private;

   with function Default_Key return Key_T;
   with function Default_Element return Element_T;

   with function Hash (Key : Key_T) return Aida.Hash32;
   with function Equivalent_Keys (Left, Right : Key_T) return Boolean;

   Max_Hash_Map_Size : Max_Hash_Map_Size_T;

   Max_Collision_List_Size : Aida.Int32 := 0;
package Aida.Bounded_Hash_Map is

   use all type Aida.Int32;
   use all type Aida.Hash32;

   type T is limited private with
     Default_Initial_Condition => Used_Capacity (T) = 0;

   procedure Insert (This        : in out T;
                     Key         : Key_T;
                     New_Element : Element_T) with
     Global => null,
     Pre    => Used_Capacity (This) < Max_Collision_List_Size,
     Post   => (Used_Capacity (This)'Old = Used_Capacity (This) or Used_Capacity (This) = Used_Capacity (This)'Old + 1);

   function Element (This : T;
                     Key  : Key_T) return Element_T with
     Global => null,
     Pre    => Exists (This, Key);

   function Exists (This : T;
                    Key  : Key_T) return Boolean with
     Global => null;

   function Used_Capacity (This : T) return Aida.Nat32 with
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

   subtype Bucket_Index_T is Aida.Hash32 range Aida.Hash32'(0)..Aida.Hash32 (Max_Hash_Map_Size - 1);

   type Bucket_Array_T is array (Bucket_Index_T) of Nullable_Node_T;

--   type Collision_Index_T is new Aida.Int32 range 1..Max_Collision_List_Size;

   function Default_Node return Node_T;

   package Collision_Vector is new Aida.Bounded_Vector
     (Max_Last_Index  => Int32'First + Max_Collision_List_Size,
      Element_T       => Node_T,
      Default_Element => Default_Node);

   type T is
      record
         Buckets        : Bucket_Array_T := (others => (Exists => False));
         Collision_List : Collision_Vector.T;
      end record;

   function Used_Capacity (This : T) return Aida.Nat32 is
     (Aida.Nat32 ((Collision_Vector.Last_Index (This.Collision_List) + 1)
                    - Collision_Vector.First_Index (This.Collision_List)));

   function Normalize_Index (H : Aida.Hash32) return Bucket_Index_T is (if H < Aida.Hash32 (Max_Hash_Map_Size) then
                                                                  Bucket_Index_T (H)
                                                               else
                                                                  Bucket_Index_T (H - ((H/Aida.Hash32 (Max_Hash_Map_Size)))*Aida.Hash32 (Max_Hash_Map_Size)));

   function Exists (This : T;
                    Key  : Key_T) return Boolean is (This.Buckets (Normalize_Index (Hash (Key))).Exists);

--     function Exists (This : T;
--                      Key  : Key_T) return Boolean is ((This.Buckets (Normalize_Index (Hash (Key))).Exists) and
--                                                         (for some I in Collision_Index_T range Collision_Vector.First_Index (This.Collision_List)..Collision_Vector.Last_Index (This.Collision_List) =>
--                                                               Collision_Vector.Element (This.Collision_List, I).Key = Key));

end Aida.Bounded_Hash_Map;
