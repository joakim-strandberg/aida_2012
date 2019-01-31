-- This is a key to value map where the keys are integers and the values are of String type.
generic
   Max_Chars : Positive;
   Max_Strings : Positive;
   type Value_T is new Standard.String;
package Aida.Integer_To_String_Map is
   pragma Pure;

   subtype Key_T is Positive range 1..Max_Strings;

   subtype Available_Chars_T is Natural range 0..Max_Chars;

   subtype Available_Keys_T is Natural range 0..Max_Strings;

   type T is tagged limited private with
     Default_Initial_Condition =>
       Available_Chars (T) = Max_Chars and Available_Keys (T) = Max_Strings;

   function Available_Chars (This : T) return Available_Chars_T with
     Global => null;

   function Available_Keys (This : T) return Available_Keys_T with
     Global => null;

   procedure Append (This  : in out T;
                     Value : Value_T;
                     Key   : out Key_T) with
     Global => null,
     Pre'Class  => Value'Length >= 1 and This.Available_Chars >= Value'Length and This.Available_Keys > 0,
     Post'Class => This.Available_Chars'Old - Value'Length = This.Available_Chars and This.Available_Keys + 1 = This.Available_Keys'Old and This.Value (Key) = Value;

   function Value (This  : T;
                   Index : Key_T) return Value_T with
     Global => null;

   function Make return T with
     Global => null;

private

   subtype Char_Index_T is Natural range 1..Max_Chars;

   subtype From_Index_T is Positive range 1..Char_Index_T'Last;

   subtype To_Index_T   is Natural range 0..Char_Index_T'Last;

   type Substring_T is record
      From : From_Index_T := 1;
      To   : To_Index_T   := 0;
   end record;

   type Substring_Indexes_T is array (Key_T) of Substring_T;

   subtype Next_T is Natural range 0..Char_Index_T'Last;

   subtype Next_Index_T is Natural range 0..Key_T'Last;

   type T is tagged limited
      record
         My_Huge_Text  : Value_T (Char_Index_T'First..Char_Index_T'Last) := (others => ' ');
         My_Next       : Next_T := 0;
         My_Next_Index : Next_Index_T := 0;
         My_Substrings : Substring_Indexes_T;
      end record;

   function Value (This  : T;
                     Index : Key_T) return Value_T is (This.My_Huge_Text (This.My_Substrings (Index).From..This.My_Substrings (Index).To));

   function Make return T is (
                              My_Huge_Text  => (others => ' '),
                              My_Next       => 0,
                              My_Next_Index => 0,
                              My_Substrings => (others => (From => 1, To => 0))
                                               );

end Aida.Integer_To_String_Map;
