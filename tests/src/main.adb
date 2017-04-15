--with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ahven.Framework;
with Ahven.Text_Runner;
--with Application_Locator;
with Aida.Types.Tests;
with Aida.UTF8.Tests;
with Aida.Bounded_String_Tests;
with Aida.Bounded_Hash_Map_Tests;

procedure Main is
   use all type Ada.Strings.Unbounded.Unbounded_String;

   S : aliased Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite ("All");

   Converstion_Test : Aida.Types.Tests.Test;

   Bounded_String_Test : Aida.Bounded_String_Tests.Test;

   Bounded_Hash_Map_Test : Aida.Bounded_Hash_Map_Tests.Test;


   UTF8_Test : Aida.UTF8.Tests.Test;

--      Executable_Directory : Ada.Strings.Unbounded.Unboun
begin
--     declare
--        Has_Failed : Boolean;
--        Exe_Dir_Temp : constant String := Application_Locator.Get_Executable_Directory (Has_Failed_To_Find_Executable_Directory => Has_Failed);
--     begin
--        if Has_Failed then
--           Ada.Text_IO.Put_Line("Failed to identify the executable directory ../bin");
--           return;
--        end if;
--        Set_Unbounded_String (Target => Executable_Directory,
--                              Source => Exe_Dir_Temp);
--     end;

--   Ahven.Framework.Add_Static_Test (S, Std_Integer_Test);
   Ahven.Framework.Add_Static_Test (S, Bounded_String_Test);
   Ahven.Framework.Add_Static_Test (S, Bounded_Hash_Map_Test);
   Ahven.Framework.Add_Static_Test (S, Converstion_Test);
   Ahven.Framework.Add_Static_Test (S, UTF8_Test);
   Ahven.Text_Runner.Run (S);
end Main;
