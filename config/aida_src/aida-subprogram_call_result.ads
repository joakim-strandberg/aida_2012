with Aida.Types;
with Aida.Bounded_String;

-- Useful for subprograms that can either succeed or report an error message,
-- and under the contraint of satisfying the "one exit"-rule in GNATCheck.
generic
   Capacity : Positive;
package Aida.Subprogram_Call_Result is

   subtype Length_T is Natural range 0 .. Capacity;

   type T is limited private with Default_Initial_Condition => Length (T) = 0 and Has_Failed (T) = False;

   procedure Initialize (This    : in out T;
                         Message : Aida.Types.String_T) with
     Global => null,
     Pre    => not Has_Failed (This) and Message'Length < Capacity,
     Post   => Has_Failed (This) = True and Length (This) = Message'Length;

--     procedure Initialize (This    : in out T;
--                           Message : String) with
--       Global => null,
--       Pre    => Length (This) = 0 and Message'Length < Capacity,
--       Post   => Has_Failed (This) = True;

   procedure Initialize (This : in out T;
                         M1   : String;
                         M2   : String) with
     Global => null,
     Pre    => not Has_Failed (This) and ((M1'Length < Positive'Last/2 and M2'Length < Positive'Last/2) and then (M1'Length + M2'Length < Capacity)),
     Post   => Has_Failed (This) = True and Length (This) = M1'Length + M2'Length;

   function Has_Failed (This : T) return Boolean with
     Global => null;

   function Length (This : T) return Length_T with
     Global => null;

   generic
      with procedure Do_Something (Text : Aida.Types.String_T);
   procedure Act_On_Immutable_Text (This : in T) with
     Global => null;

private

   package Bounded_String is new Aida.Bounded_String (Capacity);

   type T is limited
      record
         My_Message    : Bounded_String.T;
         My_Has_Failed : Boolean := False;
      end record;

   function Length (This : T) return Length_T is (Bounded_String.Length (This.My_Message));

   function Has_Failed (This : T) return Boolean is (This.My_Has_Failed);

end Aida.Subprogram_Call_Result;
