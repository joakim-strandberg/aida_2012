-- Bounded vector implementation in pure SPARK and not using any formal containers.
--
-- 32-bit vector implementation designed to be able to hold around 4 billion number of items.
--
-- For the Max_Last_Index parameter write Integer + (max size of the vector).
-- Example of a vector of integers with a maximum of 10 elements:
--
--     function Get return Integer is (0);
--     pragma Annotate (GNATprove, Terminating, Get);
--
--     package BV is new Aida.Bounded_Vector2 (Max_Last_Index  => Integer'First + 10,
--                                             Element_T       => Integer,
--                                             Default_Element => Get);
--
generic
   Max_Last_Index : Integer;
   type Element_T is private;
   with function Default_Element return Element_T;
package Aida.Tagged_Bounded_Vector is
   pragma Pure;

   pragma Assert (Max_Last_Index > Integer'First);

   subtype Extended_Index_T is Integer range Integer'First..Max_Last_Index;

   subtype Index_T is Extended_Index_T range Integer'First + 1..Extended_Index_T'Last;

   type Elements_Array_T is array (Index_T range <>) of aliased Element_T;

   type T is tagged limited private with
     Default_Initial_Condition => Is_Empty (T);
   -- The vector type is limited to avoid unnecessary copies

   function Max_Index (This : T) return Integer with
     Global => null,
     Post'Class   => Max_Index'Result = Max_Last_Index;
   pragma Annotate (GNATprove, Terminating, Max_Index);

   -- Used when the vector is a component of a record (or array)
   function Default_Vector return T with
     Global => null;
   pragma Annotate (GNATprove, Terminating, Default_Vector);

   function "=" (L, R : T) return Boolean with
     Global => null;
   pragma Annotate (GNATprove, Terminating, "=");

   procedure Append (This     : in out T;
                     New_Item : Element_T) with
     Global => null,
     Pre'Class  => Last_Index (This) < Max_Index (This),
     Post'Class => Last_Index (This) = Last_Index (This)'Old + 1;

   function Contains (This    : T;
                      Element : Element_T) return Boolean with
     Global => null;

   function First_Index (This : T) return Index_T with
     Global     => null,
     Post'Class => First_Index'Result = Index_T'First;
   pragma Annotate (GNATprove, Terminating, First_Index);

   function Last_Index (This : T) return Extended_Index_T with
     Global => null;
   pragma Annotate (GNATprove, Terminating, Last_Index);

   function Element (This  : T;
                     Index : Index_T) return Element_T with
     Global => null,
     Pre'Class => Index <= Last_Index (This);

   procedure Replace_Element (This        : in out T;
                              Index       : Index_T;
                              New_Element : Element_T) with
     Global => null,
     Pre'Class    => Index <= Last_Index (This),
     Post'Class   => Last_Index (This)'Old = Last_Index (This);

   procedure Replace_Last_Element (This        : in out T;
                                   New_Element : Element_T) with
     Global => null,
     Pre'Class    => Last_Index (This) >= First_Index (This),
     Post'Class   => Last_Index (This)'Old = Last_Index (This);

   function Is_Empty (This : T) return Boolean with
     Global => null,
     Post'Class   => (if Is_Empty'Result then
                  Last_Index (This) = Extended_Index_T'First
                  else
                    Last_Index (This) >= First_Index (This));

   function Is_Non_Empty (This : T) return Boolean with
     Global => null,
     Post   => Is_Non_Empty'Result = (This.Last_Index >= This.First_Index);
   pragma Annotate (GNATprove, Terminating, Is_Non_Empty);

   function Is_Full (This : T) return Boolean with
     Global => null;

   function Last_Element (This : T) return Element_T with
     Global    => null,
     Pre'Class => This.Is_Non_Empty;

   procedure Delete_Last (This : in out T) with
     Global => null,
     Pre'Class  => This.Last_Index >= This.First_Index,
     Post       => This.Last_Index = This.Last_Index'Old - 1;

   procedure Clear (This : in out T) with
     Global => null,
     Post'Class => Last_Index (This) < First_Index (This);

private

   type T is tagged limited
      record
         My_Items       : aliased Elements_Array_T (Index_T) := (others => Default_Element);
         My_Last_Index  : Extended_Index_T := Extended_Index_T'First;
      end record;

   function Is_Full (This : T) return Boolean is
     (This.My_Last_Index = Extended_Index_T'Last);

end Aida.Tagged_Bounded_Vector;
