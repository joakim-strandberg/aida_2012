-- Bounded vector implementation in pure SPARK and not using any formal containers.
generic
   type Index_T is range <>;
   type Element_T is private;
   with function Default_Element return Element_T;
package Aida.Bounded_Vector is

   pragma Assert (Index_T'First = 1);

   subtype Extended_Index_T is Index_T'Base range Index_T'First-1..Index_T'Last;

   subtype Length_T is Index_T'Base range 0 .. Index_T'Last - Index_T'First + 1;

   subtype Capacity_T is Length_T range 1..Length_T'Last;

   type Elements_Array_T is array (Index_T range <>) of aliased Element_T;

   type T (Capacity : Capacity_T) is limited private with
     Default_Initial_Condition => Is_Empty (T);
   -- The vector type is limited to avoid unnecessary copies

   function Default_Vector (C : Capacity_T) return T with
     Global => null;

   function "=" (L, R : T) return Boolean with
     Global => null,
     Pre    => Length (L) <= L.Capacity and Length (R) <= R.Capacity;
   pragma Annotate (GNATprove, Terminating, "=");

   function Length (This : T) return Length_T with
     Global => null;
   pragma Annotate (GNATprove, Terminating, Length);

   procedure Append (This     : in out T;
                     New_Item : Element_T) with
     Global => null,
     Pre  => Length (This) < This.Capacity,
     Post => Length (This) = Length (This)'Old + 1;

   function Contains (This    : T;
                      Element : Element_T) return Boolean with
     Global => null,
     Pre  => Length (This) < This.Capacity;

   function First_Index (This : T) return Index_T with
     Global => null;

   function Last_Index (This : T) return Extended_Index_T with
     Global => null;
   pragma Annotate (GNATprove, Terminating, Last_Index);

   function Element (This  : T;
                     Index : Index_T) return Element_T with
     Global => null,
     Pre => Index <= Last_Index (This) and Index <= This.Capacity;

   procedure Replace_Element (This        : in out T;
                              Index       : Index_T;
                              New_Element : Element_T) with
     Global => null,
     Pre    => Index <= Last_Index (This) and Index <= This.Capacity,
     Post   => Length (This)'Old = Length (This) and
     Last_Index (This)'Old = Last_Index (This);

   function Is_Empty (This : T) return Boolean with
     Global => null;

   function Is_Full (This : T) return Boolean with
     Global => null;

   function Last_Element (This : T) return Element_T with
     Global => null,
     Pre    => Length (This) > 0 and Length (This) <= This.Capacity;

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

private

   type T (Capacity : Capacity_T) is limited
      record
         Items       : aliased Elements_Array_T (1..Capacity) := (others => Default_Element);
         Last_Index  : Extended_Index_T := Extended_Index_T'First;
      end record;

   function Last_Index (This : T) return Extended_Index_T is (This.Last_Index);

   function Length (This : T) return Length_T is (Length_T (This.Last_Index - Index_T'First + 1));

   function Is_Empty (This : T) return Boolean is (This.Last_Index = Extended_Index_T'First);

   function Is_Full (This : T) return Boolean is (This.Last_Index = Extended_Index_T'Last);

   function "=" (L, R : T) return Boolean is (Last_Index (L) = Last_Index (R) and then
                                                (for all I in Index_T range Index_T'First..Last_Index (L) => L.Items (I) = R.Items (I)));

end Aida.Bounded_Vector;
