package body Aida.Bounded_String is
   pragma SPARK_Mode;

   use all type Standard.String;

   procedure Initialize (This : in out T;
                         Text : Standard.String) is
   begin
      for I in Int32_T range 1..Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
         pragma Loop_Invariant (for all J in Int32_T range 1..I => This.Text (J) = Text (Text'First - 1 + J));
         pragma Loop_Variant (Increases => I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize;

   procedure Initialize2 (This : out T;
                          Text : Standard.String)
   is
   begin
      This.Text := (others => ' ');
      for I in Int32_T range 1..Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
         pragma Loop_Invariant (for all J in Int32_T range 1..I => This.Text (J) = Text (Text'First - 1 + J));
         pragma Loop_Variant (Increases => I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize2;

   procedure Append (Target : in out T;
                     Source : Standard.String) is
   begin
      for I in Int32_T range Source'First..Source'Last loop
         Target.Text (Target.Text_Length + 1 + (I - Source'First)) := Source (I);
      end loop;
      Target.Text_Length := Target.Text_Length + Source'Length;
   end Append;

   function Hash32 (This : T) return Aida.Hash32_T is
   begin
      return Aida.String.Hash32 (This.Text (1..Length (This)));
   end Hash32;

   function Equals (This   : T;
                    Object : Standard.String) return Boolean
   is
      Result : Boolean := True;
   begin
      if Length (This) = Object'Length then
         if Object'Length > 0 then
            Result := This.Text (1..This.Text_Length) = Object (Object'Range);
         end if;
      end if;

      return Result;
   end Equals;

   function To_String (This : T) return Standard.String is
   begin
      return This.Text (1..This.Text_Length);
   end To_String;

end Aida.Bounded_String;
