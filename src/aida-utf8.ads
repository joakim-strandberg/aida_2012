--  The code in this package originates from the work of Dmitry A. Kazakov,
--  the Simple Components library. The changes can be summarized:
--
--  - Conversion from Ada95 to SPARK (Ada2012)
--  - The subprograms have been grouped differently.
--
--  The code is shared under the following license:
--
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--
--
--  The original copyright notices:
--                                                                    --
--                                                                    --
--  package Strings_Edit.UTF8       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2005       --
--                                                                    --
--                                Last revision :  21:03 21 Apr 2009  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--
--
--
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Categorization            Luebeck            --
--  Interface                                      Spring, 2008       --
--                                                                    --
--                                Last revision :  21:03 21 Apr 2009  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--  ________________________________________________________________  --
--
--  This  package  provides  categorization of code points as defined by
--  UnicodeData file.
--

package Aida.UTF8 with SPARK_Mode is

   --
   --  General_Category of a code point according to the  Unicode  character
   --  database. The names of the enumeration correspond to the names in the
   --  database.
   --
   type General_Category is
     (Lu, --  Letter, Uppercase
      Ll, --         Lowercase
      Lt, --         Titlecase
      Lm, --         Modifier
      Lo, --         Other

      Mn, -- Mark, Nonspacing
      Mc, --       Spacing Combining
      Me, --       Enclosing

      Nd, -- Number, Decimal Digit
      Nl, --         Letter
      No, --         Other

      Pc, -- Punctuation, Connector
      Pd, --              Dash
      Ps, --              Open
      Pe, --              Close
      Pi, --              Initial quote
      Pf, --              Final quote
      Po, --              Other

      Sm, -- Symbol, Math
      Sc, --         Currency
      Sk, --         Modifier
      So, --         Other

      Zs, -- Separator, Space
      Zl, --            Line
      Zp, --            Paragraph

      Cc, -- Other, Control
      Cf, --        Format
      Cs, --        Surrogate
      Co, --        Private Use
      Cn  --        Not Assigned
     );
   --
   --  Classes of categories
   --
   subtype Letter      is General_Category range Lu .. Lo;
   subtype Mark        is General_Category range Mn .. Me;
   subtype Mumber      is General_Category range Nd .. No;
   subtype Punctuation is General_Category range Pc .. Po;
   subtype Symbol      is General_Category range Sm .. So;
   subtype Separator   is General_Category range Zs .. Zp;
   subtype Other       is General_Category range Cc .. Cn;

   type Code_Point_Base is mod 2**32;
   subtype Code_Point is Code_Point_Base range 0  .. 16#10FFFF#;

   subtype Code_Point_String_Length is Positive range 1 .. 4;
   --  Length of a String corresponding to a specific code point.

   --
   --  Image -- Of an UTF-8 code point
   --
   --    Value - The code point
   --
   --  Returns :
   --
   --    UTF-8 encoded equivalent
   --
   function Image (Value : Code_Point) return String with
     Global => null,
     Post   => Image'Result'Length in Code_Point_String_Length;

   --
   --  Has_Case -- Case test
   --
   --    Value - Code point
   --
   --  Returns :
   --
   --    True if Value has either an  upper  or  a  lower  case  equivalent
   --    different from Code.
   --
   function Has_Case (Value : Code_Point) return Boolean with
     Global => null;

   --
   --  Is_Lowercase -- Case test
   --
   --    Value - Code point
   --
   --  Returns :
   --
   --    True if Value is a lower case point
   --
   function Is_Lowercase (Value : Code_Point) return Boolean with
     Global => null;

   --
   --  Is_Uppercase -- Case test
   --
   --    Value - Code point
   --
   --  Returns :
   --
   --    True if Value is a lower case point
   --
   function Is_Uppercase (Value : Code_Point) return Boolean with
     Global => null;

   --
   --  To_Lowercase -- Convert to lower case
   --
   --    Value - Code point or UTF-8 encoded string
   --
   --  Returns :
   --
   --    The lower case eqivalent or else Value itself
   --
   function To_Lowercase (Value : Code_Point) return Code_Point with
     Global => null,
     Post   => Image (To_Lowercase'Result)'Length = Image (Value)'Length;

   --
   --  To_Uppercase -- Convert to upper case
   --
   --    Value - Code point or UTF-8 encoded string
   --
   --  Returns :
   --
   --    The upper case eqivalent or else Value itself
   --
   function To_Uppercase (Value : Code_Point) return Code_Point with
     Global => null,
     Post   => Image (To_Uppercase'Result)'Length = Image (Value)'Length;

   --
   --  Category -- Get category of a code point
   --
   --    Value - Code point
   --
   --  Returns :
   --
   --    The category of value
   --
   function Category (Value : Code_Point) return General_Category with
     Global => null;

   --
   --  Is_* -- Category tests
   --
   function Is_Alphanumeric (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Digit        (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Control      (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_ISO_646      (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Letter       (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Lower        (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Other_Format (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Space        (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Title        (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Upper        (Value : in Code_Point) return Boolean with
     Global => null;

   --
   --  Special digits
   --
   function Is_Subscript_Digit (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Superscript_Digit (Value : in Code_Point) return Boolean with
     Global => null;
   --
   --  Ada 2005 identifier sets
   --
   --    identifier_start,  see ARM 2.3(3/2)
   --    identifier_extend, see ARM 2.3(3.1/2)
   --
   function Is_Identifier_Start (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Identifier_Extend (Value : in Code_Point) return Boolean with
     Global => null;

   function Is_Valid_UTF8_Code_Point (Source  : String;
                                      Pointer : Integer) return Boolean is
     ((Source'First <= Pointer and Pointer <= Source'Last)
      and then
        (if Character'Pos (Source (Pointer)) in 0 .. 16#7F# then
              Pointer < Integer'Last
         else
           (Pointer < Source'Last
            and then
              (if
                   Character'Pos (Source (Pointer)) in 16#C2# .. 16#DF# and
                 Character'Pos (Source (Pointer + 1)) in 16#80# .. 16#BF#
               then Pointer < Integer'Last - 1
               else
                 (Pointer < Source'Last - 1
                  and then
                    (if
                         (Character'Pos (Source (Pointer)) = 16#E0# and
                              Character'Pos (Source (Pointer + 1)) in
                            16#A0# .. 16#BF# and
                              Character'Pos (Source (Pointer + 2)) in
                            16#80# .. 16#BF#)
                     then Pointer < Integer'Last - 2
                     elsif
                       (Character'Pos (Source (Pointer))
                        in 16#E1# .. 16#EF# and
                          Character'Pos (Source (Pointer + 1))
                        in 16#80# .. 16#BF# and
                          Character'Pos (Source (Pointer + 2))
                        in 16#80# .. 16#BF#)
                     then Pointer < Integer'Last - 2
                     else
                       (Pointer < Source'Last - 2
                        and then
                          (if
                               (Character'Pos (Source (Pointer)) = 16#F0# and
                                    Character'Pos (Source (Pointer + 1)) in
                                  16#90# .. 16#BF# and
                                    Character'Pos (Source (Pointer + 2)) in
                                  16#80# .. 16#BF# and
                                    Character'Pos (Source (Pointer + 3)) in
                                  16#80# .. 16#BF#)
                           then Pointer < Integer'Last - 3
                           elsif
                             (Character'Pos (Source (Pointer))
                              in 16#F1# .. 16#F3# and
                                  Character'Pos (Source (Pointer + 1)) in
                                16#80# .. 16#BF# and
                                  Character'Pos (Source (Pointer + 2)) in
                                16#80# .. 16#BF# and
                                  Character'Pos (Source (Pointer + 3)) in
                                16#80# .. 16#BF#)
                           then Pointer < Integer'Last - 3
                           elsif
                             (Character'Pos (Source (Pointer)) = 16#F4# and
                                  Character'Pos (Source (Pointer + 1)) in
                                16#80# .. 16#8F# and
                                  Character'Pos (Source (Pointer + 2)) in
                                16#80# .. 16#BF# and
                                  Character'Pos (Source (Pointer + 3)) in
                                16#80# .. 16#BF#)
                           then Pointer < Integer'Last - 3
                           else False))))))));

   --
   --  Get -- Get one UTF-8 code point
   --
   --    Source  - The source string
   --    Pointer - The string position to start at
   --    Value   - The result
   --
   --   This  procedure  decodes one UTF-8 code point from the string Source.
   --   It starts at Source (Pointer). After successful completion Pointer is
   --   advanced to the first character following the input.  The  result  is
   --   returned through the parameter Value.
   --
   procedure Get (Source :     String; Pointer : in out Integer;
                  Value              : out Code_Point) with
     Global => null,
     Pre    => Is_Valid_UTF8_Code_Point (Source, Pointer) and
     Pointer < Integer'Last - 4,
     Post => Pointer <= Pointer'Old + 4 and
     (if Character'Pos (Source (Pointer'Old)) in 0 .. 16#7F# then
        Character'Pos (Source (Pointer'Old)) = Value and
          Pointer = Pointer'Old + 1
            elsif
        (Pointer'Old < Source'Last
         and then
           (Character'Pos (Source (Pointer'Old)) in 16#C2# .. 16#DF# and
                Character'Pos (Source (Pointer'Old + 1)) in 16#80# .. 16#BF#))
        then Pointer = Pointer'Old + 2
          elsif
        (Pointer'Old < Source'Last - 1
         and then
           ((Character'Pos (Source (Pointer'Old)) = 16#E0# and
                 Character'Pos (Source (Pointer'Old + 1)) in 16#A0# .. 16#BF#
             and
               Character'Pos (Source (Pointer'Old + 2)) in 16#80# .. 16#BF#) or
                (Character'Pos (Source (Pointer'Old)) in 16#E1# .. 16#EF# and
                     Character'Pos (Source (Pointer'Old + 1))
                   in 16#80# .. 16#BF# and
                     Character'Pos (Source (Pointer'Old + 2))
                   in 16#80# .. 16#BF#)))
          then Pointer = Pointer'Old + 3
            elsif
        (Pointer < Source'Last - 2
         and then
           ((Character'Pos (Source (Pointer'Old)) = 16#F0# and
            Character'Pos (Source (Pointer'Old + 1)) in 16#90# .. 16#BF# and
          Character'Pos (Source (Pointer'Old + 2)) in 16#80# .. 16#BF# and
            Character'Pos (Source (Pointer'Old + 3)) in 16#80# .. 16#BF#) or
         (Character'Pos (Source (Pointer'Old)) in 16#F1# .. 16#F3# and
              Character'Pos (Source (Pointer'Old + 1)) in 16#80# .. 16#BF# and
            Character'Pos (Source (Pointer'Old + 2)) in 16#80# .. 16#BF# and
              Character'Pos (Source (Pointer'Old + 3)) in 16#80# .. 16#BF#) or
         (Character'Pos (Source (Pointer'Old)) = 16#F4# and
              Character'Pos (Source (Pointer'Old + 1)) in 16#80# .. 16#8F# and
            Character'Pos (Source (Pointer'Old + 2)) in 16#80# .. 16#BF# and
              Character'Pos (Source (Pointer'Old + 3)) in 16#80# .. 16#BF#)))
        then Pointer = Pointer'Old + 4);

   function Is_Valid_UTF8 (Source : String) return Boolean with
     Global => null,
     Pre    => Source'Last < Integer'Last - 4;

   --
   --  Length -- The length of an UTF-8 string
   --
   --    Source - The string containing UTF-8 encoded code points
   --
   --  Returns :
   --
   --    The number of UTF-8 encoded code points in Source
   --
   function Length (Source : String) return Natural with
     Global => null,
     Pre    => Source'Last < Integer'Last - 4;
   --     Post   => Length'Result <= Source'Length;

   --
   --  Put -- Put one UTF-8 code point
   --
   --    Destination - The target string
   --    Pointer     - The position where to place the character
   --    Value       - The code point to put
   --
   --  This  procedure  puts  one  UTF-8  code  point into the string Source
   --  starting from the position Source (Pointer). Pointer is then advanced
   --  to the first character following the output.
   --
   procedure Put (Destination : in out String;
                  Pointer     : in out Integer;
                  Value       : Code_Point) with
     Global => null,
     Pre    =>
       ((Pointer in Destination'Range and Destination'Last < Integer'Last - 4)
        and then
          (if Value <= 16#7F# then Pointer < Integer'Last
                 elsif Value <= 16#7FF# then
                   Pointer < Integer'Last - 1 and Pointer + 1
                     in Destination'Range
                     elsif Value <= 16#FFFF# then
             (Pointer < Integer'Last - 2
              and then
                (Pointer + 1 in Destination'Range and
                     Pointer + 2 in Destination'Range))
                 else Pointer < Integer'Last - 3
           and then
             (Pointer + 1 in Destination'Range and
                  Pointer + 2 in Destination'Range and
                    Pointer + 3 in Destination'Range))),
         Post =>
           (Pointer /= Pointer'Old and
              (Pointer in Destination'Range or Pointer =
                         Destination'Last + 1));

   function To_Lowercase (Value : String) return String with
     Global => null,
     Pre    => Value'Last < Integer'Last - 4;

   function To_Uppercase (Value : String) return String with
     Global => null,
     Pre    => Value'Last < Integer'Last - 4;

private
   pragma Inline
     (Is_Alphanumeric, Is_Control, Is_Digit, Is_ISO_646,
        Is_Letter,       Is_Lower,   Is_Title, Is_Upper,
        Is_Subscript_Digit,  Is_Superscript_Digit,
        Is_Identifier_Start, Is_Identifier_Extend
       );

end Aida.UTF8;
