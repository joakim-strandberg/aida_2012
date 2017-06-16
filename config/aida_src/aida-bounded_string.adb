package body Aida.Bounded_String with SPARK_Mode is

   procedure Initialize (This : in out T;
                         Text : Aida.Types.String_T) is
   begin
      Clear (This.Text);
      for I in Integer range 1..Text'Length loop
         Append (This.Text, Text (Text'First - 1 + I));
         pragma Loop_Invariant (for all J in Integer range 1..I => Element (This.Text, J) = Text (Text'First - 1 + J));
         pragma Loop_Invariant (Length (This.Text) = Ada.Containers.Count_Type (I));
         pragma Loop_Variant (Increases => I);
      end loop;
   end Initialize;

   procedure Append (Target : in out T;
                     Source : Aida.Types.String_T) is
   begin
      for I in Integer range Source'First..Source'Last loop
         pragma Loop_Invariant (Length (Target.Text) = Ada.Containers.Count_Type (I - Source'First) + Length (Target.Text)'Loop_Entry);
         pragma Loop_Variant (Increases => I);
         Append (Target.Text, Source (I));
      end loop;
   end Append;

   function Hash32 (This : T) return Aida.Types.Hash32_T is
      H : Aida.Types.Hash32_T := 0;
      A : Aida.Types.Hash32_T := 31_415;
      B : constant Aida.Types.Hash32_T := 27_183;

      use type Aida.Types.Hash32_T;
   begin
      for I in Positive range First_Index (This.Text)..Last_Index (This.Text) loop
         H := A*H + Standard.Character'Pos (Element (This.Text, I));
         A := A*B;
         pragma Loop_Variant (Increases => I);
      end loop;

      return H;
     end Hash32;

   procedure Act_On_Immutable_Text (This : in Bounded_String_T) is
   begin
      Do_Something (T (This).Text);
   end Act_On_Immutable_Text;

   function Check_Something_On_Immutable_Text (This  : Bounded_String_T;
                                               Arg   : Arg_T) return Return_T is
   begin
      return Check_Something (T (This).Text, Arg);
   end Check_Something_On_Immutable_Text;

   function Equals (This   : T;
                    Object : Standard.String) return Boolean
   is
      Result : Boolean := True;
   begin
      if Length (This) = Object'Length then
         if Object'Length > 0 then
            for I in Positive range 1..Last_Index (This.Text) loop
               if Element (This.Text, I) /= Object (Object'First + I - 1) then
                  Result := False;
                  exit;
               end if;
               pragma Loop_Variant (Increases => I);
            end loop;
         end if;
      else
         Result := False;
      end if;

      return Result;
   end Equals;

   function To_String (This : T) return Aida.Types.String_T is
      S : Aida.Types.String_T (1..Integer (Length (This.Text)));
   begin
      for I in Positive range First_Index (This.Text)..Last_Index (This.Text) loop
         S (I) := Element (This.Text, I);
      end loop;

      return S;
   end To_String;

end Aida.Bounded_String;
