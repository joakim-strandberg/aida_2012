with Ada.Containers;

package Aida with Pure, SPARK_Mode is

   subtype Digit_Character is Character with
     Predicate =>
       Digit_Character = '0' or
       Digit_Character = '1' or
       Digit_Character = '2' or
       Digit_Character = '3' or
       Digit_Character = '4' or
       Digit_Character = '5' or
       Digit_Character = '6' or
       Digit_Character = '7' or
       Digit_Character = '8' or
       Digit_Character = '9';
   --  It is more useful with a subtype for digit characters than
   --  an function Is_Digit (C : Character) return Boolean;
   --  Consider for example case-statements:
   --
   --  C : Character;
   --
   --  case Digit_Character (C) is
   --     when '0' => ..
   --     ...
   --     when '9' => ..
   --  end case;

   function To_Integer (Source : in Digit_Character) return Integer with
     Post => To_Integer'Result in 0 .. 9;

   procedure To_Integer (Source : in     Character;
                       Target :    out Integer) with
     Pre  => Source in Digit_Character,
     Post => Target in 0 .. 9 and Target = To_Integer (Source);

   function Is_One_Byte_UTF8 (C : Character) return Boolean is
     (Character'Pos (C) <= 127);
   --  A UTF8 code point can be represented by 1-4 characters.
   --  The first 128 characters (US-ASCII) need only one byte.

   function To_Hash_Type (This : Integer) return Ada.Containers.Hash_Type with
     Global => null;
   --  Calculates the hash of an 32-bit Integer based upon the following post
   --  on Stackoverlow:
   --  http://stackoverflow.com/questions/664014/
   --  what-Integer-hash-function-are-good-that-accepts-an-Integer-hash-key
   --
   --  I found the following algorithm provides
   --  a very good statistical distribution.
   --  Each input bit affects each output bit with about 50% probability.
   --  There are no collisions (each input results in a different output).
   --  The algorithm is fast except if the CPU doesn't have a
   --  built-in Integer multiplication unit. C-Code:
   --
   --  unsigned int hash(unsigned int x) {
   --      x = ((x >> 16) ^ x) * 0x45d9f3b;
   --      x = ((x >> 16) ^ x) * 0x45d9f3b;
   --      x = (x >> 16) ^ x;
   --      return x;
   --  }
   --
   --  The magic number was calculated using a special multi-threaded
   --  test program that ran for many hours,
   --  which calculates the avalanche effect
   --  (the number of output bits that change if a single input bit is changed;
   --  should be nearly 16 on average), independence of output bit changes
   --  (output bits should not depend on each other),
   --  and the probability of a change in each output bit if any input bit
   --  is changed. The calculated values are better than the 32-bit finalizer
   --   used by MurmurHash, and nearly as good (not quite) as when using AES.

   function To_Hash_Type (This : String) return Ada.Containers.Hash_Type with
     Global => null;

   function To_String (This : Integer) return String with
     Global => null,
     Post   =>
       (if This < 0 then
          To_String'Result'Length >= 2 and To_String'Result'Length <= 11
            else
              To_String'Result'Length >= 1 and To_String'Result'Length <= 10);

   function To_Char (This : Integer) return Character with
     Global => null,
     Pre    => This >= 0 and This <= 9;

   function To_String (This : Float) return String with
     Global => null,
     Post   => To_String'Result'Length >= 1 and To_String'Result'Length <= 11;

   procedure To_Integer
     (Source     : in  String;
      Target     : out Integer;
      Has_Failed : out Boolean) with
     Global         => null;

   procedure To_Float (Source     : in  String;
                       Target     : out Float;
                       Has_Failed : out Boolean) with
     Global => null;

   function Is_Latin1_Graphic_Characters (Text : String) return Boolean with
     Global => null;

   function Starts_With (This         : String;
                         Searched_For : String) return Boolean with
     Global => null,
     Pre    => Searched_For'Length > 0;

   function Concat (Left, Right : String) return String with
     Global => null,
     Pre    =>
       (Left'Length < Positive'Last / 2 and
          Right'Length < Positive'Last / 2),
     Post   => Concat'Result'Length = Left'Length + Right'Length;

   type Max_Hash_Map_Size_T is range 3 .. 2**31;

   type Bounded_String (Maximum_Length : Positive) is limited private with
      Default_Initial_Condition => Length (Bounded_String) = 0;

   procedure Initialize (This : in out Bounded_String; Text : String) with
      Global => null,
      Pre    => Text'Length <= This.Maximum_Length,
      Post   => Length (This) = Text'Length;

   procedure Initialize2 (This : out Bounded_String; Text : String) with
      Global => null,
      Pre    => Text'Length <= This.Maximum_Length,
      Post   => Length (This) = Text'Length;

   procedure Append (Target : in out Bounded_String; Source : String) with
      Global => null,
      Pre    => Source'Length <= Target.Maximum_Length - Length (Target),
      Post   => Length (Target) <= Target.Maximum_Length;

   function Length (This : Bounded_String) return Natural with
      Global => null;
   --  Compare the Length function with the Length function
   --  in Ada.Containers.Formal_Vectors. Notice the post-condition!

   function Equals (This : Bounded_String; Object : String) return Boolean with
      Global => null,
      Pre    => Length (This) <= This.Maximum_Length;

   function "=" (Left, Right : Bounded_String) return Boolean with
      Global => null,
      Pre    => Length (Left) <= Left.Maximum_Length and
      Length (Right) <= Right.Maximum_Length;

   function "=" (Left : Bounded_String; Right : String) return Boolean with
      Global => null,
      Pre    => Length (Left) <= Left.Maximum_Length;
      --  Although the arguments are of different types,
      --  they may still represent the same String.

   function To_Hash_Type
     (This : Bounded_String) return Ada.Containers.Hash_Type with
     Global => null,
     Pre    => Length (This) <= This.Maximum_Length;

   function To_String (This : Bounded_String) return String with
      Global => null,
      Pre    => Length (This) <= This.Maximum_Length;

   type Call_Result is tagged limited private with
     Default_Initial_Condition => Call_Result.Has_Failed = False;

   procedure Initialize (This   : in out Call_Result;
                         Code_1 : Integer;
                         Code_2 : Integer) with
     Global     => null,
     Pre'Class  => not Has_Failed (This),
     Post'Class => This.Has_Failed = True;

   function Has_Failed (This : Call_Result) return Boolean with
     Global => null;

   function Message (This : Call_Result) return String with
     Global => null;

private

   type Bounded_String (Maximum_Length : Positive) is limited record
      Text        : String (1 .. Maximum_Length) := (others => ' ');
      Text_Length : Natural                          := 0;
   end record;

   function Length (This : Bounded_String) return Natural is
     (This.Text_Length);

   function "=" (Left, Right : Bounded_String) return Boolean is
     (Length (Left) = Length (Right)
      and then
      (for all I in Positive range 1 .. Left.Text_Length =>
         Left.Text (I) = Right.Text (I)));

   function "=" (Left : Bounded_String; Right : String) return Boolean is
     (Equals (Left, Right));

   type Call_Result is tagged limited
      record
         My_Code_1 : Integer := 0;
         My_Code_2 : Integer := 0;
         My_Has_Failed : Boolean := False;
      end record;

   function Has_Failed (This : Call_Result) return Boolean is
     (This.My_Has_Failed);

end Aida;
