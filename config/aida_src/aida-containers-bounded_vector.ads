generic
   type Index_T is range <>;
   type Element_T is private;
   with function "=" (L, R : Element_T) return Boolean is <>;
   with function Default_Element return Element_T;
package Aida.Containers.Bounded_Vector is

   subtype Extended_Index_T is Index_T'Base range Index_T'First-1..Index_T'Last;

   subtype Length_T is Index_T'Base range 0 .. Index_T'Last - Index_T'First + 1;

   type Elements_Array_T is array (Index_T range <>) of aliased Element_T;

   type T is limited private;
   -- The vector type is limited because making copies would mean
   -- introducing reference counting to know when to deallocate the vector.

   function "=" (L, R : T) return Boolean;

   function Length (This : T) return Length_T with
     Global => null;

   procedure Append (This     : in out T;
                     New_Item : Element_T) with
     Global => null,
     Pre  => Length (This) < Length_T'Last,
     Post => Length (This) = Length (This)'Old + 1;

   function Contains (This    : T;
                      Element : Element_T) return Boolean with
     Global => null;

   pragma Warnings (Off, "unused variable",
                    Reason => "pragma Unreferenced(..) is uneffective");
   function First_Index (This : T) return Index_T with
     Global => null;
   pragma Warnings (On, "unused variable");

   function Last_Index (This : T) return Extended_Index_T with
     Global => null;

   function Element (This  : T;
                     Index : Index_T) return Element_T with
     Global => null,
     Pre => Index <= Last_Index (This);

   procedure Replace_Element (This        : in out T;
                              Index       : Index_T;
                              New_Element : Element_T) with
     Global => null,
     Pre    => Index <= Last_Index (This),
     Post   => Length (This)'Old = Length (This) and
     Last_Index (This)'Old = Last_Index (This);

   function Is_Empty (This : T) return Boolean with
     Global => null;

   function Last_Element (This : T) return Element_T with
     Global => null,
     Pre    => Length (This) > 0;

   procedure Delete_Last (This : in out T) with
     Global => null,
     Pre    => Length (This) > 0,
     Post   => Length (This) = Length (This)'Old - 1;

   procedure Clear (This : in out T) with
     Global => null,
     Post => Length (This) = 0;

   generic
      with procedure Do_Something (Elements : Elements_Array_T);
   procedure Act_On_Immutable_Elements (This : in T);

   generic
      with procedure Do_Something (Elements : in out Elements_Array_T);
   procedure Act_On_Mutable_Elements (This : in out T);

--     type Element_Const_Ptr is access constant Element_T;
--
--     function Const_Ref (This  : T;
--                         Index : Index_T) return Element_Const_Ptr;
--
--     type Ptr is access all T;

private

   subtype Items_T is Elements_Array_T (Index_T'Range);

   type T is limited
      record
         Items       : aliased Items_T := (others => Default_Element);
         Last_Index  : Extended_Index_T := Extended_Index_T'First;
      end record;

   function Last_Index (This : T) return Extended_Index_T is (This.Last_Index);

   function Length (This : T) return Length_T is (Length_T (This.Last_Index - Index_T'First + 1));

   function Is_Empty (This : T) return Boolean is (This.Last_Index = Extended_Index_T'First);

   function "=" (L, R : T) return Boolean is (Last_Index (L) = Last_Index (R) and then
                                                (for all I in Index_T range Index_T'First..Last_Index (L) => L.Items (I) = R.Items (I)));

end Aida.Containers.Bounded_Vector;
