-- Useful for subprograms that can either succeed or report an error message,
-- and under the contraint of satisfying the "one exit"-rule in GNATCheck.
package Aida.Subprogram_Call_Result with SPARK_Mode is

   type T is tagged limited private with Default_Initial_Condition => T.Has_Failed = False;

   procedure Initialize (This   : in out T;
                         Code_1 : Int32_T;
                         Code_2 : Int32_T) with
     Global     => null,
     Pre'Class  => not Has_Failed (This),
     Post'Class => This.Has_Failed = True;

   function Has_Failed (This : T) return Boolean with
     Global => null;

   function Message (This : T) return String_T with
     Global => null;

private

   type T is tagged limited
      record
         My_Code_1 : Int32_T := 0;
         My_Code_2 : Int32_T := 0;
         My_Has_Failed : Boolean := False;
      end record;

   function Has_Failed (This : T) return Boolean is (This.My_Has_Failed);

end Aida.Subprogram_Call_Result;
