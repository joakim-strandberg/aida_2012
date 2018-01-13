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
-- Usage of Ada.Containers.Formal_Vectors will result in optimized but compiler specific code.
-- For example, the changes made between GNAT GPL 2016 and 2017 were incompatible.
-- The formal containers packages are therefore avoided in the Aida library (for now).
with Aida;

package Aida.Bounded_String is
   pragma SPARK_Mode;

--     type T (Maximum_Length : Pos32_T) is limited private with
--       Default_Initial_Condition => Length (T) = 0;
   type T (Maximum_Length : Pos32_T) is limited private;

   procedure Initialize (This : in out T;
                         Text : Standard.String) with
     Global => null,
     Pre    => Text'Length <= This.Maximum_Length,
     Post   => Length (This) = Text'Length;

   procedure Initialize2 (This : out T;
                          Text : Standard.String) with
     Global => null,
     Pre    => Text'Length <= This.Maximum_Length,
     Post   => Length (This) = Text'Length;

   procedure Append (Target : in out T;
                     Source : Standard.String) with
     Global => null,
     Pre    => Source'Length <= Target.Maximum_Length - Length (Target),
     Post   => Length (Target) <= Target.Maximum_Length;

   function Length (This : T) return Nat32_T with
     Global => null;
   -- Compare the Length function with the Length function
   -- in Ada.Containers.Formal_Vectors. Notice the post-condition!

   function Equals (This   : T;
                    Object : Standard.String) return Boolean with
     Global => null,
     Pre    => Length (This) <= This.Maximum_Length;

   function "=" (Left, Right : T) return Boolean with
     Global => null,
     Pre    => Length (Left) <= Left.Maximum_Length and Length (Right) <= Right.Maximum_Length;

   function "=" (Left : T; Right : Standard.String) return Boolean with
     Global => null,
     Pre    => Length (Left) <= Left.Maximum_Length;
   -- Although the arguments are of different types, they may still represent the same String.

   function Hash32 (This : T) return Aida.Hash32_T with
     Global => null,
     Pre    => Length (This) <= This.Maximum_Length;

   function To_String (This : T) return Standard.String with
     Global => null,
     Pre    => Length (This) <= This.Maximum_Length;

private

   type T (Maximum_Length : Pos32_T) is limited record
      Text        : Standard.String (1..T.Maximum_Length) := (others => ' ');
      Text_Length : Nat32_T := 0;
   end record;

   function Length (This : T) return Nat32_T is (This.Text_Length);

   function "=" (Left, Right : T) return Boolean is (Length (Left) = Length (Right) and then (for all I in Pos32_T range 1..Left.Text_Length => Left.Text (I) = Right.Text (I)));

   function "=" (Left : T; Right : Standard.String) return Boolean is (Equals (Left, Right));

end Aida.Bounded_String;
