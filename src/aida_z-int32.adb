with Interfaces;
package body Aida_Z.Int32 with SPARK_Mode, Pure is

   use type Interfaces.Unsigned_32;

   function To_String (This : T) return Zzz_String_T is

      subtype Index_T is Integer range 1..16;

      subtype Result_T is Zzz_String_T (Index_T);

      procedure Make_Result (Temp   : in out T;
                             Result : in out Result_T;
                             P      : in out Index_T) with
        Pre  => Temp >= 0 and 300_000_000 > Temp and P = 15,
        Post => P >= 6,
        Inline_Always => True;

      procedure Make_Result (Temp   : in out T;
                             Result : in out Result_T;
                             P      : in out Index_T)
      is
         Digit : T;
      begin
         -- 1
         if Temp /= 0 then
            Digit := Temp mod 10;
            Result (P) := Character (To_Char (Digit));
            Temp := Temp / 10;
            P := P - 1;

            -- 2
            if Temp /= 0 then
               Digit := Temp mod 10;
               Result (P) := Character (To_Char (Digit));
               Temp := Temp / 10;
               P := P - 1;

               -- 3
               if Temp /= 0 then
                  Digit := Temp mod 10;
                  Result (P) := Character (To_Char (Digit));
                  Temp := Temp / 10;
                  P := P - 1;

                  -- 4
                  if Temp /= 0 then
                     Digit := Temp mod 10;
                     Result (P) := Character (To_Char (Digit));
                     Temp := Temp / 10;
                     P := P - 1;

                     -- 5
                     if Temp /= 0 then
                        Digit := Temp mod 10;
                        Result (P) := Character (To_Char (Digit));
                        Temp := Temp / 10;
                        P := P - 1;

                        -- 6
                        if Temp /= 0 then
                           Digit := Temp mod 10;
                           Result (P) := Character (To_Char (Digit));
                           Temp := Temp / 10;
                           P := P - 1;

                           -- 7
                           if Temp /= 0 then
                              Digit := Temp mod 10;
                              Result (P) := Character (To_Char (Digit));
                              Temp := Temp / 10;
                              P := P - 1;

                              -- 8
                              if Temp /= 0 then
                                 Digit := Temp mod 10;
                                 Result (P) := Character (To_Char (Digit));
                                 Temp := Temp / 10;
                                 P := P - 1;

                                 -- 9
                                 if Temp /= 0 then
                                    Digit := Temp mod 10;
                                    Result (P) := Character (To_Char (Digit));
                                    Temp := Temp / 10;
                                    P := P - 1;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end Make_Result;

      Result : Result_T := (others => ' ');

      P : Index_T := Index_T'Last;

      Temp : T := This;
   begin
      if Temp <= 0 then
         if Temp = -2_147_483_648 then
            Result (Index_T'Last - 10..Index_T'Last) := "-2147483648";
         else
            Temp := -Temp;

            pragma Assert (Temp >= 0 and 2_147_483_647 >= Temp);

            declare
               Digit : T := Temp mod 10;
            begin
               Result (P) := Character (To_Char (Digit));
            end;

            Temp := Temp / 10;

            P := P - 1;

            pragma Warnings (Off, "unused assignment to ""Temp""");
            Make_Result (Temp, Result, P);
            pragma Warnings (On, "unused assignment to ""Temp""");
         end if;
      else
         declare
            Digit : T;
         begin
            Digit := Temp mod 10;
            Result (P) := Character (To_Char (Digit));

            Temp := Temp / 10;

            P := P - 1;

            pragma Warnings (Off, "unused assignment to ""Temp""");
            Make_Result (Temp, Result, P);
            pragma Warnings (On, "unused assignment to ""Temp""");
         end;
      end if;

      if This < 0 then
         Result (P) := '-';
         P := P - 1;
      end if;

      return Result (P+1..Index_T'Last);
   end To_String;

   function Hash32 (This : T) return Zzz_Hash32_T is
      X : Interfaces.Unsigned_32 := (if This >= 0 then
                                        Interfaces.Unsigned_32 (This)
                                     else
                                        Interfaces.Unsigned_32 (-This));
   begin
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X) * 16#45d9f3b#;
      X := (Interfaces.Shift_Right (X, 16) xor X);

      return Zzz_Hash32_T (X);
   end Hash32;

end Aida_Z.Int32;
