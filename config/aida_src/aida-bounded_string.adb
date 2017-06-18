package body Aida.Bounded_String with SPARK_Mode is

   use all type Aida.Types.String_T;

   procedure Initialize (This : in out T;
                         Text : Aida.Types.String_T) is
   begin
      for I in Integer range 1..Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
         pragma Loop_Invariant (for all J in Integer range 1..I => This.Text (J) = Text (Text'First - 1 + J));
         pragma Loop_Variant (Increases => I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize;

   procedure Append (Target : in out T;
                     Source : Aida.Types.String_T) is
   begin
      for I in Integer range Source'First..Source'Last loop
         Target.Text (Target.Text_Length + 1 + (I - Source'First)) := Source (I);
      end loop;
      Target.Text_Length := Target.Text_Length + Source'Length;
   end Append;

   function Hash32 (This : T) return Aida.Types.Hash32_T is
   begin
      return Hash32 (Aida.Types.String_T (This.Text (1..Length (This))));
   end Hash32;

   procedure Act_On_Immutable_Text (This : in T) is
   begin
      Do_Something (This.Text (1..This.Text_Length));
   end Act_On_Immutable_Text;

   function Check_Something_On_Immutable_Text (This  : T;
                                               Arg   : Arg_T) return Return_T is
   begin
      return Check_Something (This.Text (1..This.Text_Length), Arg);
   end Check_Something_On_Immutable_Text;

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

   function To_String (This : T) return Aida.Types.String_T is
   begin
      return This.Text (1..This.Text_Length);
   end To_String;

end Aida.Bounded_String;
