-- The code in this package originates from the work of Dmitry A. Kazakov,
-- the Simple Components library. The changes can be summarized:
--
--  - Conversion from Ada95 to SPARK (Ada2012)
--  - The subprograms have been grouped differently.
--
-- The code is shared under the following license:
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
--____________________________________________________________________--
--
--  This  package  provides  categorization of code points as defined by
--  UnicodeData file.
--

package Aida.UTF8_Code_Point with SPARK_Mode is

   package Fs with SPARK_Mode is

      --
      -- General_Category of a code point according to the  Unicode  character
      -- database. The names of the enumeration correspond to the names in the
      -- database.
      --
      type General_Category is
        (  Lu, -- Letter, Uppercase
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
      -- Classes of categories
      --
      subtype Letter      is General_Category range Lu..Lo;
      subtype Mark        is General_Category range Mn..Me;
      subtype Mumber      is General_Category range Nd..No;
      subtype Punctuation is General_Category range Pc..Po;
      subtype Symbol      is General_Category range Sm..So;
      subtype Separator   is General_Category range Zs..Zp;
      subtype Other       is General_Category range Cc..Cn;

   end Fs;

   type Code_Point is mod 2**32;
   subtype T is Code_Point range 0..16#10FFFF#;

   --
   -- Image -- Of an UTF-8 code point
   --
   --    Value - The code point
   --
   -- Returns :
   --
   --    UTF-8 encoded equivalent
   --
   function Image (Value : T) return String_T with
     Global => null;

   --
   -- Has_Case -- Case test
   --
   --    Value - Code point
   --
   -- Returns :
   --
   --    True if Value has either an  upper  or  a  lower  case  equivalent
   --    different from Code.
   --
   function Has_Case (Value : T) return Boolean with
     Global => null;

   --
   -- Is_Lowercase -- Case test
   --
   --    Value - Code point
   --
   -- Returns :
   --
   --    True if Value is a lower case point
   --
   function Is_Lowercase (Value : T) return Boolean with
     Global => null;

   --
   -- Is_Uppercase -- Case test
   --
   --    Value - Code point
   --
   -- Returns :
   --
   --    True if Value is a lower case point
   --
   function Is_Uppercase (Value : T) return Boolean with
     Global => null;

   --
   -- To_Lowercase -- Convert to lower case
   --
   --    Value - Code point or UTF-8 encoded string
   --
   -- Returns :
   --
   --    The lower case eqivalent or else Value itself
   --
   function To_Lowercase (Value : T) return T with
     Global => null;

   --
   -- To_Uppercase -- Convert to upper case
   --
   --    Value - Code point or UTF-8 encoded string
   --
   -- Returns :
   --
   --    The upper case eqivalent or else Value itself
   --
   function To_Uppercase (Value : T) return T with
     Global => null;

   --
   -- Category -- Get category of a code point
   --
   --    Value - Code point
   --
   -- Returns :
   --
   --    The category of value
   --
   function Category (Value : T) return Fs.General_Category with
     Global => null;

   --
   -- Is_* -- Category tests
   --
   function Is_Alphanumeric (Value : in T) return Boolean with
     Global => null;

   function Is_Digit        (Value : in T) return Boolean with
     Global => null;

   function Is_Control      (Value : in T) return Boolean with
     Global => null;

   function Is_ISO_646      (Value : in T) return Boolean with
     Global => null;

   function Is_Letter       (Value : in T) return Boolean with
     Global => null;

   function Is_Lower        (Value : in T) return Boolean with
     Global => null;

   function Is_Other_Format (Value : in T) return Boolean with
     Global => null;

   function Is_Space        (Value : in T) return Boolean with
     Global => null;

   function Is_Title        (Value : in T) return Boolean with
     Global => null;

   function Is_Upper        (Value : in T) return Boolean with
     Global => null;

   --
   -- Special digits
   --
   function Is_Subscript_Digit (Value : in T) return Boolean with
     Global => null;

   function Is_Superscript_Digit (Value : in T) return Boolean with
     Global => null;
   --
   -- Ada 2005 identifier sets
   --
   --    identifier_start,  see ARM 2.3(3/2)
   --    identifier_extend, see ARM 2.3(3.1/2)
   --
   function Is_Identifier_Start (Value : in T) return Boolean with
     Global => null;

   function Is_Identifier_Extend (Value : in T) return Boolean with
     Global => null;

private
   pragma Inline
     (  Is_Alphanumeric, Is_Control, Is_Digit, Is_ISO_646,
        Is_Letter,       Is_Lower,   Is_Title, Is_Upper,
        Is_Subscript_Digit,  Is_Superscript_Digit,
        Is_Identifier_Start, Is_Identifier_Extend
       );

end Aida.UTF8_Code_Point;
