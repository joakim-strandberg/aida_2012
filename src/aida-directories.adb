package body Aida.Directories is

   use all type Aida.String_T;
   use all type Aida.Int32_T;

   function Exists (Name : String) return Boolean renames Ada.Directories.Exists;

   function Current_Directory return String renames Ada.Directories.Current_Directory;

   function Compose (Containing_Directory : String := "";
                     Name                 : String;
                     Extension            : String := "") return String renames Ada.Directories.Compose;

   function Containing_Directory (Name : String) return String renames Ada.Directories.Containing_Directory;

   procedure Set_Directory (Directory : String) renames Ada.Directories.Set_Directory;

   procedure Create_Directory
     (New_Directory : String;
      Form          : String := "") renames Ada.Directories.Create_Directory;

   procedure Delete_Directory (Directory : String) renames Ada.Directories.Delete_Directory;

   procedure Create_Path
     (New_Directory : String;
      Form          : String := "") renames Ada.Directories.Create_Path;

   procedure Delete_Tree (Directory : String) renames Ada.Directories.Delete_Tree;

   procedure Delete_File (Name : String) renames Ada.Directories.Delete_File;

   procedure Rename (Old_Name, New_Name : String) renames Ada.Directories.Rename;

   procedure Copy_File
     (Source_Name   : String;
      Target_Name   : String;
      Form          : String := "") renames Ada.Directories.Copy_File;

   function Full_Name (Name : String) return String renames Ada.Directories.Full_Name;

   function Simple_Name (Name : String) return String renames Ada.Directories.Simple_Name;

   function Extension (Name : String) return String renames Ada.Directories.Extension;

   function Base_Name (Name : String) return String renames Ada.Directories.Base_Name;

   function Kind (Name : String) return File_Kind renames Ada.Directories.Kind;

   function Size (Name : String) return File_Size renames Ada.Directories.Size;

   function Modification_Time (Name : String) return Ada.Calendar.Time renames Ada.Directories.Modification_Time;

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True)) renames Ada.Directories.Start_Search;

   procedure End_Search (Search : in out Search_Type) renames Ada.Directories.End_Search;

   function More_Entries (Search : Search_Type) return Boolean renames Ada.Directories.More_Entries;

   procedure Get_Next_Entry
     (Search          : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type) renames Ada.Directories.Get_Next_Entry;

--     procedure Search
--       (Directory : String;
--        Pattern   : String;
--        Filter    : Filter_Type := (others => True);
--        Process   : not null access procedure (Directory_Entry : Directory_Entry_Type)) renames Ada.Directories.Search;

   function Simple_Name (Directory_Entry : Directory_Entry_Type) return String renames Ada.Directories.Simple_Name;

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String renames Ada.Directories.Full_Name;

   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind renames Ada.Directories.Kind;

   function Size (Directory_Entry : Directory_Entry_Type) return File_Size renames Ada.Directories.Size;

   function Modification_Time
     (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time renames Ada.Directories.Modification_Time;

   function Exists (Simple_Filename     : Aida.String_T;
                    Directory_To_Search : String) return Boolean with
     SPARK_Mode => On
   is
      Search : Aida.Directories.Search_Type;

      Filter : constant Aida.Directories.Filter_Type := (
                                                         Directory     => False,
                                                         Ordinary_File => True,
                                                         Special_File  => False
                                                        );

      Directory_Entry : Aida.Directories.Directory_Entry_Type;

      Is_Success : Boolean := False;
   begin
      Aida.Directories.Start_Search (Search    => Search,
                                     Directory => Directory_To_Search,
                                     Pattern   => "*",
                                     Filter    => Filter);
      while Aida.Directories.More_Entries (Search) and not Is_Success loop
         Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
         declare
            Filename : constant String := Aida.Directories.Simple_Name (Directory_Entry);
         begin
            if Equivalent (Simple_Filename, Filename) then
               Is_Success := True;
               exit;
            end if;
         end;
      end loop;
      Aida.Directories.End_Search (Search);

      return Is_Success;
   end Exists;

end Aida.Directories;
