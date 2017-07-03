package body Aida.Subprogram_Call_Result is

   use all type Bounded_String.T;
   use all type Aida.String_T;

   procedure Initialize (This    : in out T;
                         Message : Aida.String_T) is
   begin
      Initialize (This => This.My_Message,
                  Text => Message);
      This.My_Has_Failed := True;
   end Initialize;

--     procedure Initialize (This    : in out T;
--                           Message : String) is
--     begin
--        Initialize (This => This.My_Message,
--                    Text => Aida.String_T (Message));
--        This.My_Has_Failed := True;
--     end Initialize;

   procedure Initialize (This : in out T;
                         M1   : String;
                         M2   : String) is
   begin
      Initialize (This => This.My_Message,
                  Text => Concat (Aida.String_T (M1), Aida.String_T (M2)));
      This.My_Has_Failed := True;
   end Initialize;

   procedure Act_On_Immutable_Text (This : in T) is
      procedure Act is new Aida.Bounded_String.Act_On_Immutable_Text (Bounded_String_T => Bounded_String_T, Do_Something => Do_Something);
   begin
      Act (This.My_Message);
   end Act_On_Immutable_Text;

   function Message (This : T) return Aida.String_T is
   begin
      return To_String (This.My_Message);
   end Message;


end Aida.Subprogram_Call_Result;
