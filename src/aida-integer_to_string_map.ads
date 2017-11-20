generic
   Capacity : Pos32_T;
   Max_Strings : Pos32_T;
   type Element_T is new String;
package Aida.Integer_To_String_Map is

   subtype Char_Index_T is Nat32_T'Base range 1..Capacity;

   subtype Index_T is Pos32_T range 1..Max_Strings;

   subtype Available_Capacity_T is Nat32_T range 0..Char_Index_T'Last;

   subtype Available_Substrings_T is Nat32_T range 0..Index_T'Last;

   type T is tagged limited private;

   function Available_Capacity (This : T) return Available_Capacity_T with
     Global => null;

   function Available_Substrings (This : T) return Available_Substrings_T with
     Global => null;

   procedure Append (This     : in out T;
                     New_Item : Element_T;
                     Index    : out Index_T) with
     Global => null,
     Pre'Class  => New_Item'Length >= 1 and This.Available_Capacity >= New_Item'Length and This.Available_Substrings > 0,
     Post'Class => This.Available_Capacity'Old - New_Item'Length = This.Available_Capacity and This.Available_Substrings + 1 = This.Available_Substrings'Old;

   function Element (This  : T;
                     Index : Index_T) return Element_T with
     Global => null;

private

   subtype From_Index_T is Pos32_T range 1..Char_Index_T'Last;

   subtype To_Index_T   is Nat32_T range 0..Char_Index_T'Last;

   type Substring_T is record
      From : From_Index_T := 1;
      To   : To_Index_T   := 0;
   end record;

   type Substring_Indexes_T is array (Index_T) of Substring_T;

   subtype Next_T is Nat32_T range 0..Char_Index_T'Last;

   subtype Next_Index_T is Nat32_T range 0..Index_T'Last;

   type T is tagged limited
      record
         My_Huge_Text  : Element_T (Positive (Char_Index_T'First)..Positive (Char_Index_T'Last)) := (others => ' ');
         My_Next       : Next_T := 0;
         My_Next_Index : Next_Index_T := 0;
         My_Substrings : Substring_Indexes_T;
      end record;

   function Available_Capacity (This : T) return Available_Capacity_T is (Char_Index_T'Last - This.My_Next);

   function Available_Substrings (This : T) return Available_Substrings_T is (Index_T'Last - This.My_Next_Index);

   function Element (This  : T;
                     Index : Index_T) return Element_T is (This.My_Huge_Text (Integer (This.My_Substrings (Index).From)..Integer (This.My_Substrings (Index).To)));

end Aida.Integer_To_String_Map;
