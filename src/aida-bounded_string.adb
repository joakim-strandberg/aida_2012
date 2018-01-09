package body Aida.Bounded_String is
   pragma SPARK_Mode;

   use all type Aida.String_T;

   procedure Initialize (This : in out T;
                         Text : Aida.String_T) is
   begin
      for I in Integer range 1..Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
         pragma Loop_Invariant (for all J in Integer range 1..I => This.Text (J) = Text (Text'First - 1 + J));
         pragma Loop_Variant (Increases => I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize;

   procedure Initialize2 (This : out T;
                          Text : Aida.String_T)
   is
   begin
      This.Text := (others => ' ');
      for I in Integer range 1..Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
         pragma Loop_Invariant (for all J in Integer range 1..I => This.Text (J) = Text (Text'First - 1 + J));
         pragma Loop_Variant (Increases => I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize2;

   procedure Append (Target : in out T;
                     Source : Aida.String_T) is
   begin
      for I in Integer range Source'First..Source'Last loop
         Target.Text (Target.Text_Length + 1 + (I - Source'First)) := Source (I);
      end loop;
      Target.Text_Length := Target.Text_Length + Source'Length;
   end Append;

   function Hash32 (This : T) return Aida.Hash32_T is
   begin
      return Hash32 (Aida.String_T (This.Text (1..Length (This))));
   end Hash32;

   function Equals (This   : T;
                    Object : Standard.String) return Boolean
   is
      Result : Boolean := True;
   begin
      if Length (This) = Object'Length then
         if Object'Length > 0 then
            Result := String (This.Text (1..This.Text_Length)) = Object (Object'Range);
         end if;
      end if;

      return Result;
   end Equals;

   function To_String (This : T) return Aida.String_T is
   begin
      return This.Text (1..This.Text_Length);
   end To_String;

end Aida.Bounded_String;
