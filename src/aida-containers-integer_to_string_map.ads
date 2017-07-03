with Aida.Containers.Bounded_Vector;

generic
   type Index_T is range <>;
   type Element_T is new String;
   with function "=" (L, R : Element_T) return Boolean is <>;
package Aida.Containers.Integer_To_String_Map is

   subtype Extended_Index_T is Index_T'Base range Index_T'First-1..Index_T'Last;

   type Length_T is new Index_T'Base range 0 .. Index_T'Last - Index_T'First + 1;

   type T (Capacity : Positive) is limited private;

   function Available_Capacity (This : T) return Natural with
     Global => null,
     Pre    => Hidden_Index (This) <= This.Capacity;

   function Length (This : T) return Length_T with
     Global => null;

   procedure Append (This     : in out T;
                     New_Item : Element_T) with
     Global => null,
     Pre  => (Hidden_Index (This) <= This.Capacity - New_Item'Length and New_Item'Length >= 1) and then (Available_Capacity (This) >= New_Item'Length) and then Length (This) < Length_T'Last,
     Post => Available_Capacity (This)'Old - New_Item'Length = Available_Capacity (This) and Length (This) = Length (This)'Old + 1;

   function First_Index (This : T) return Index_T with
     Global => null;

   function Last_Index (This : T) return Extended_Index_T with
     Global => null;

   generic
      with procedure Do_Something (Text : Element_T);
   procedure Act_On_Immutable_Text (This  : T;
                                    Index : Index_T) with
     Global => null,
     Pre => Index <= Last_Index (This);

   generic
      type Return_T is private;
      type Arg_T is private;
      with function Check_Something (Text : Element_T;
                                     Arg  : Arg_T) return Return_T;
   function Check_Something_On_Immutable_Text (This  : T;
                                               Index : Index_T;
                                               Arg   : Arg_T) return Return_T with
     Global => null,
     Pre => Index <= Last_Index (This);

   function Hidden_Index (This : T) return Natural with
     Global => null;

private

   type Substring_T is record
      From : Positive;
      To   : Positive;
   end record;

   function Default_Substring return Substring_T is (From => 1, To => 1);

   package Item_Vector is new Aida.Containers.Bounded_Vector (Index_T,
                                                              Substring_T,
                                                              "=",
                                                              Default_Substring);

   use all type Item_Vector.T;

   type T (Capacity : Positive) is limited
      record
         My_Huge_Text  : Element_T (1..Capacity) := (others => ' ');
         My_Next       : Natural := 0;
         My_Substrings : Item_Vector.T;
      end record;

   function Available_Capacity (This : T) return Natural is (This.Capacity - This.My_Next);

   function Length (This : T) return Length_T is (Length_T (Length (This.My_Substrings)));

   function Hidden_Index (This : T) return Natural is (This.My_Next);

   function First_Index (This : T) return Index_T is (First_Index (This.My_Substrings));

   function Last_Index (This : T) return Extended_Index_T is (Last_Index (This.My_Substrings));

end Aida.Containers.Integer_To_String_Map;
