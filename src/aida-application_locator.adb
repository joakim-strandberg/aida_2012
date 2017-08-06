with Aida.Directories;
with Aida.OS_Lib;

package body Aida.Application_Locator is

   use type Aida.Directories.File_Kind;

   function Get_Executable_Directory (Has_Failed_To_Find_Executable_Directory : out Boolean) return String
   is
      Current_Directory                : constant String := Aida.Directories.Current_Directory;
      Simple_Name_Of_Current_Directory : constant String := Aida.Directories.Simple_Name (Current_Directory);
   begin
      if Simple_Name_Of_Current_Directory = "bin" then
         Has_Failed_To_Find_Executable_Directory := False;
         return Current_Directory;
      end if;

      declare
         Potential_Executable_Directory : constant String := Current_Directory & Aida.OS_Lib.Directory_Separator & "bin";
      begin
         if Aida.Directories.Exists (Potential_Executable_Directory) and then Aida.Directories.Kind (Potential_Executable_Directory) = Aida.Directories.Directory then
            Has_Failed_To_Find_Executable_Directory := False;
            return Potential_Executable_Directory;
         else
            Has_Failed_To_Find_Executable_Directory := True;
            return Current_Directory;
         end if;
      end;
   end Get_Executable_Directory;

end Aida.Application_Locator;
