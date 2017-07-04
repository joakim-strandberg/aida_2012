-- Gnatprove can successfully formally verify the code in this package at Level=0.
package Aida_Z.Character with SPARK_Mode, Pure is

   type T is new Zzz_Char_T;

   function Is_Digit (C : T) return Boolean with
     Contract_Cases => (C = '0' => Is_Digit'Result,
                        C = '1' => Is_Digit'Result,
                        C = '2' => Is_Digit'Result,
                        C = '3' => Is_Digit'Result,
                        C = '4' => Is_Digit'Result,
                        C = '5' => Is_Digit'Result,
                        C = '6' => Is_Digit'Result,
                        C = '7' => Is_Digit'Result,
                        C = '8' => Is_Digit'Result,
                        C = '9' => Is_Digit'Result,
                        C > '9' => Is_Digit'Result = False,
                        C < '0' => Is_Digit'Result = False);

   function Check_Result (Result : Zzz_Int32_T; Expected : Integer) return Boolean with
     Ghost => True,
     Pre => Expected in 0..9;

   function To_Int32 (Source : in T) return Zzz_Int32_T with
     Pre  => Is_Digit (Source),
     Post => To_Int32'Result in 0..9,
     Contract_Cases => (Source = '0' => Check_Result (To_Int32'Result, 0),
                        Source = '1' => Check_Result (To_Int32'Result, 1),
                        Source = '2' => Check_Result (To_Int32'Result, 2),
                        Source = '3' => Check_Result (To_Int32'Result, 3),
                        Source = '4' => Check_Result (To_Int32'Result, 4),
                        Source = '5' => Check_Result (To_Int32'Result, 5),
                        Source = '6' => Check_Result (To_Int32'Result, 6),
                        Source = '7' => Check_Result (To_Int32'Result, 7),
                        Source = '8' => Check_Result (To_Int32'Result, 8),
                        Source = '9' => Check_Result (To_Int32'Result, 9));

   procedure To_Int32 (Source : in  T;
                       Target : out Zzz_Int32_T) with
     Pre  => Is_Digit (Source),
     Post => Target in 0 .. 9 and Target = To_Int32 (Source),
     Contract_Cases => (Source = '0' => Check_Result (Target, 0),
                        Source = '1' => Check_Result (Target, 1),
                        Source = '2' => Check_Result (Target, 2),
                        Source = '3' => Check_Result (Target, 3),
                        Source = '4' => Check_Result (Target, 4),
                        Source = '5' => Check_Result (Target, 5),
                        Source = '6' => Check_Result (Target, 6),
                        Source = '7' => Check_Result (Target, 7),
                        Source = '8' => Check_Result (Target, 8),
                        Source = '9' => Check_Result (Target, 9));

   function Is_One_Byte_UTF8 (C : T) return Boolean is (Standard.Character'Pos (Standard.Character (C)) <= 127);
   -- A UTF8 code point can be represented by 1-4 characters. The first 128 characters (US-ASCII) need only one byte.

private

   function Check_Result (Result : Zzz_Int32_T; Expected : Integer) return Boolean is (Result = Zzz_Int32_T (Expected));

end Aida_Z.Character;
