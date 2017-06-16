-- Motivation
--
-- In the standard library bounded strings are represented by instantiations of
-- the generic package Ada.Strings.Bounded.Generic_Bounded_Length. The main reason
-- this design was chosen over using a discriminant that specifies the maximum
-- length of the string is that bounded strings should be non-limited.
-- To avoid unnecessary copying, this bounded string implementation is limited.
-- Thus, using a discriminant is applicable and preferred over a generic package.
-- The idea, is that the resulting executable may be smaller because it might
-- be hard for the compiler to reuse code in generics. When the Ada programming
-- language was designed (Ada 83 and perhaps Ada 95?),
-- the idea was to restrict functionality of generics that
-- it's easy for the compiler to reuse code i.e. use generics all you want,
-- the resulting executable will be small. This is unlike C++ where overuse of
-- templates/generics may result in very large executables. It turned out that
-- this was too restrictive and generics are now instantiated much like in C++.
-- The benefit of avoiding generics are twofold:
--
-- 1. Smaller executables
-- 2. Shorter compilation times
--
-- When SPARK analyzes code, each instantiation of a generic is analyzed independently.
-- Thus, avoiding generics results in shorter analysis-times.
--
-- Using Ada.Containers.Formal_Vectors will result in more optimized code by avoiding
-- unnecessary initializations.
with Aida.Types;
with Ada.Containers.Formal_Vectors;
package Aida.Bounded_String with SPARK_Mode is

   use type Ada.Containers.Count_Type;

   subtype Count_Type is Ada.Containers.Count_Type;

   package Char_Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                                              Element_Type => Character,
                                                              Bounded      => True);

   type T (Maximum_Length : Ada.Containers.Count_Type) is limited private;

   procedure Initialize (This : in out T;
                         Text : Aida.Types.String_T) with
     Global => null,
     Pre    => Text'Length <= This.Maximum_Length,
     Post   => Length (This) = Text'Length;

   procedure Append (Target : in out T;
                     Source : Aida.Types.String_T) with
     Global => null,
     Pre    => Source'Length <= Target.Maximum_Length - Length (Target);

   function Length (This : T) return Ada.Containers.Count_Type with
     Global => null,
     Post   => Length'Result <= This.Maximum_Length;

   function Equals (This   : T;
                    Object : Standard.String) return Boolean with
     Global => null,
     Pre    => Object'Last < Positive'Last - Object'Length;

   function "="(Left, Right : T) return Boolean with
     Global => null;

   function Are_Equivalent (Left : T; Right : Aida.Types.String_T) return Boolean with
     Global => null,
     Pre    => Right'Last < Positive'Last - Right'Length;
   -- Although the arguments are of different types, they may still represent the same String.

   function Hash32 (This : T) return Aida.Types.Hash32_T;

   function To_String (This : T) return Aida.Types.String_T with
     Global => null;

   generic
      type Bounded_String_T (<>) is new T;
      with procedure Do_Something (Text : Char_Vectors.Vector);
   procedure Act_On_Immutable_Text (This : in Bounded_String_T) with
     Global => null;

   generic
      type Bounded_String_T (<>) is new T;
      type Return_T is private;
      type Arg_T is private;
      with function Check_Something (Text : Char_Vectors.Vector;
                                     Arg  : Arg_T) return Return_T;
   function Check_Something_On_Immutable_Text (This  : Bounded_String_T;
                                               Arg   : Arg_T) return Return_T with
     Global => null;

private

   use all type Char_Vectors.Vector;

   type T (Maximum_Length : Ada.Containers.Count_Type) is limited
      record
         Text : Char_Vectors.Vector (Maximum_Length);
      end record;

   function Length (This : T) return Ada.Containers.Count_Type is (Length (This.Text));

   function "=" (Left, Right : T) return Boolean is (Left.Text = Right.Text);

   function Are_Equivalent (Left : T; Right : Aida.Types.String_T) return Boolean is (Equals (Left, String (Right)));

end Aida.Bounded_String;
