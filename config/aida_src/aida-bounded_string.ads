with Aida.Types;

generic
   Maximum_Length_Of_Bounded_String : Positive;
package Aida.Bounded_String is

   subtype Length_T is Natural range 0 .. Maximum_Length_Of_Bounded_String;

   type T is private;

   procedure Initialize (This : in out T;
                         Text : Aida.Types.String_T) with
     Global => null,
     Pre    => Text'Length <= Maximum_Length_Of_Bounded_String,
     Post   => Length (This) = Text'Length;

   procedure Append (Target : in out T;
                     Source : Aida.Types.String_T) with
     Global => null,
     Pre    => Source'Length <= Maximum_Length_Of_Bounded_String - Length (Target);

   function Length (This : T) return Length_T with
     Global => null;

   function Equals (This   : T;
                    Object : Standard.String) return Boolean with
     Global => null,
     Pre    => Object'Last < Positive'Last - Object'Length;

   function "="(Left, Right : T) return Boolean with
     Global => null;

   function Are_Equivalent (Left : T; Right : Aida.Types.String_T) return Boolean with
     Global => null;
   -- Although the arguments are of different types, they may still represent the same String.

   function Hash32 (This : T) return Aida.Types.Hash32_T;

   generic
      with procedure Do_Something (Text : Aida.Types.String_T);
   procedure Act_On_Immutable_Text (This : in T) with
     Global => null;

   generic
      type Return_T is private;
      type Arg_T is private;
      with function Check_Something (Text : Aida.Types.String_T;
                                     Arg  : Arg_T) return Return_T;
   function Check_Something_On_Immutable_Text (This  : T;
                                               Arg   : Arg_T) return Return_T with
     Global => null;

private

   subtype Index_T is Positive range 1..Maximum_Length_Of_Bounded_String;

   type T is
      record
         Text        : Aida.Types.String_T (Index_T'Range) := (others => ' ');
         Text_Length : Length_T := 0;
      end record;

   function Length (This : T) return Length_T is (This.Text_Length);

--     function Equals (This   : T;
--                      Object : Standard.String) return Boolean is (Length (This) = Object'Length and then (for all I in Index_T'Range => Object'First + (I - Index_T'First) <= Object'Last and then This.Text (I) = Object (Object'First + (I - Index_T'First))));

   function "="(Left, Right : T) return Boolean is (Length (Left) = Length (Right) and then (for all I in Index_T'Range => Left.Text (I) = Right.Text (I)));

   function Are_Equivalent (Left : T; Right : Aida.Types.String_T) return Boolean is ((Length (Left) = Right'Length) and then (for all I in Right'Range => Left.Text (Index_T'First + (I - Right'First)) = Right (I)));

end Aida.Bounded_String;
