with Ada.Directories;
with Ada.Calendar;
with Aida.Types;

package Aida.Directories with SPARK_Mode is

   -----------------------------------
   -- Directory and File Operations --
   -----------------------------------

   function Current_Directory return String with
     Global => null,
     Post   => Exists (Current_Directory'Result);
   --  Returns the full directory name for the current default directory. The
   --  name returned must be suitable for a future call to Set_Directory.
   --  The exception Use_Error is propagated if a default directory is not
   --  supported by the external environment.

   procedure Set_Directory (Directory : String) with
     Global => null,
     Pre    => Exists (Directory);
   --  Sets the current default directory. The exception Name_Error is
   --  propagated if the string given as Directory does not identify an
   --  existing directory. The exception Use_Error is propagated if the
   --  external environment does not support making Directory (in the absence
   --  of Name_Error) a default directory.

   procedure Create_Directory
     (New_Directory : String;
      Form          : String := "") with
     Global => null,
     Post   => Exists (New_Directory);
   --  Creates a directory with name New_Directory. The Form parameter can be
   --  used to give system-dependent characteristics of the directory; the
   --  interpretation of the Form parameter is implementation-defined. A null
   --  string for Form specifies the use of the default options of the
   --  implementation of the new directory. The exception Name_Error is
   --  propagated if the string given as New_Directory does not allow the
   --  identification of a directory. The exception Use_Error is propagated if
   --  the external environment does not support the creation of a directory
   --  with the given name (in the absence of Name_Error) and form.
   --
   --  The Form parameter is ignored

   procedure Delete_Directory (Directory : String) with
     Global => null,
     Pre    => Exists (Directory);
   --  Deletes an existing empty directory with name Directory. The exception
   --  Name_Error is propagated if the string given as Directory does not
   --  identify an existing directory. The exception Use_Error is propagated
   --  if the external environment does not support the deletion of the
   --  directory (or some portion of its contents) with the given name (in the
   --  absence of Name_Error).

   procedure Create_Path
     (New_Directory : String;
      Form          : String := "") with
     Global => null;
   --  Creates zero or more directories with name New_Directory. Each
   --  non-existent directory named by New_Directory is created. For example,
   --  on a typical Unix system, Create_Path ("/usr/me/my"); would create
   --  directory "me" in directory "usr", then create directory "my"
   --  in directory "me". The Form can be used to give system-dependent
   --  characteristics of the directory; the interpretation of the Form
   --  parameter is implementation-defined. A null string for Form specifies
   --  the use of the default options of the implementation of the new
   --  directory. The exception Name_Error is propagated if the string given
   --  as New_Directory does not allow the identification of any directory. The
   --  exception Use_Error is propagated if the external environment does not
   --  support the creation of any directories with the given name (in the
   --  absence of Name_Error) and form.
   --
   --  The Form parameter is ignored

   procedure Delete_Tree (Directory : String) with
     Global => null,
     Pre    => Exists (Directory);
   --  Deletes an existing directory with name Directory. The directory and
   --  all of its contents (possibly including other directories) are deleted.
   --  The exception Name_Error is propagated if the string given as Directory
   --  does not identify an existing directory. The exception Use_Error is
   --  propagated if the external environment does not support the deletion
   --  of the directory or some portion of its contents with the given name
   --  (in the absence of Name_Error). If Use_Error is propagated, it is
   --  unspecified if a portion of the contents of the directory are deleted.

   procedure Delete_File (Name : String) with
     Global => null,
     Pre    => Exists (Name);
   --  Deletes an existing ordinary or special file with Name. The exception
   --  Name_Error is propagated if the string given as Name does not identify
   --  an existing ordinary or special external file. The exception Use_Error
   --  is propagated if the external environment does not support the deletion
   --  of the file with the given name (in the absence of Name_Error).

   procedure Rename (Old_Name, New_Name : String) with
     Global => null,
     Pre    => Exists (Old_Name) and not Exists (New_Name);
   --  Renames an existing external file (including directories) with Old_Name
   --  to New_Name. The exception Name_Error is propagated if the string given
   --  as Old_Name does not identify an existing external file. The exception
   --  Use_Error is propagated if the external environment does not support the
   --  renaming of the file with the given name (in the absence of Name_Error).
   --  In particular, Use_Error is propagated if a file or directory already
   --  exists with New_Name.

   procedure Copy_File
     (Source_Name   : String;
      Target_Name   : String;
      Form          : String := "") with
     Global => null,
     Pre    => Exists (Source_Name) and not Exists (Target_Name);
   --  Copies the contents of the existing external file with Source_Name to
   --  Target_Name. The resulting external file is a duplicate of the source
   --  external file. The Form argument can be used to give system-dependent
   --  characteristics of the resulting external file; the interpretation of
   --  the Form parameter is implementation-defined. Exception Name_Error is
   --  propagated if the string given as Source_Name does not identify an
   --  existing external ordinary or special file or if the string given as
   --  Target_Name does not allow the identification of an external file. The
   --  exception Use_Error is propagated if the external environment does not
   --  support the creating of the file with the name given by Target_Name and
   --  form given by Form, or copying of the file with the name given by
   --  Source_Name (in the absence of Name_Error).
   --
   --  Interpretation of the Form parameter:
   --
   --    The Form parameter is case-insensitive
   --
   --    Two fields are recognized in the Form parameter:
   --      preserve=<value>
   --      mode=<value>
   --
   --      <value> starts immediately after the character '=' and ends with the
   --      character immediately preceding the next comma (',') or with the
   --      last character of the parameter.
   --
   --      The allowed values for preserve= are:
   --
   --        no_attributes:  Do not try to preserve any file attributes. This
   --                        is the default if no preserve= is found in Form.
   --
   --        all_attributes: Try to preserve all file attributes (timestamps,
   --                        access rights).
   --
   --        timestamps:     Preserve the timestamp of the copied file, but not
   --                        the other file attributes.
   --
   --      The allowed values for mode= are:
   --
   --        copy:           Only copy if the destination file does not already
   --                        exist. If it already exists, Copy_File will fail.
   --
   --        overwrite:      Copy the file in all cases. Overwrite an already
   --                        existing destination file. This is the default if
   --                        no mode= is found in Form.
   --
   --        append:         Append the original file to the destination file.
   --                        If the destination file does not exist, the
   --                        destination file is a copy of the source file.
   --                        When mode=append, the field preserve=, if it
   --                        exists, is not taken into account.
   --
   --    If the Form parameter includes one or both of the fields and the value
   --    or values are incorrect, Copy_File fails with Use_Error.
   --
   --    Examples of correct Forms:
   --       Form => "preserve=no_attributes,mode=overwrite" (the default)
   --       Form => "mode=append"
   --       Form => "mode=copy,preserve=all_attributes"
   --
   --    Examples of incorrect Forms:
   --       Form => "preserve=junk"
   --       Form => "mode=internal,preserve=timestamps"

   ----------------------------------------
   -- File and directory name operations --
   ----------------------------------------

   function Full_Name (Name : String) return String with
     Global => null,
     Pre    => Exists (Name);
   --  Returns the full name corresponding to the file name specified by Name.
   --  The exception Name_Error is propagated if the string given as Name does
   --  not allow the identification of an external file (including directories
   --  and special files).

   function Simple_Name (Name : String) return String with
     Global => null,
     Pre    => Exists (Name);
   --  Returns the simple name portion of the file name specified by Name. The
   --  exception Name_Error is propagated if the string given as Name does not
   --  allow the identification of an external file (including directories and
   --  special files).

   function Containing_Directory (Name : String) return String with
     Global => null,
     Post   => Exists (Containing_Directory'Result);
   --  Returns the name of the containing directory of the external file
   --  (including directories) identified by Name. If more than one directory
   --  can contain Name, the directory name returned is implementation-defined.
   --  The exception Name_Error is propagated if the string given as Name does
   --  not allow the identification of an external file. The exception
   --  Use_Error is propagated if the external file does not have a containing
   --  directory.

   function Extension (Name : String) return String with
     Global => null,
     Pre    => Exists (Name);
   --  Returns the extension name corresponding to Name. The extension name is
   --  a portion of a simple name (not including any separator characters),
   --  typically used to identify the file class. If the external environment
   --  does not have extension names, then the null string is returned.
   --  The exception Name_Error is propagated if the string given as Name does
   --  not allow the identification of an external file.

   function Base_Name (Name : String) return String with
     Global => null,
     Pre    => Exists (Name);
   --  Returns the base name corresponding to Name. The base name is the
   --  remainder of a simple name after removing any extension and extension
   --  separators. The exception Name_Error is propagated if the string given
   --  as Name does not allow the identification of an external file
   --  (including directories and special files).

   function Compose (Containing_Directory : String := "";
                     Name                 : String;
                     Extension            : String := "") return String with
     Global => null,
     Pre    => Exists (Containing_Directory);
   --  Returns the name of the external file with the specified
   --  Containing_Directory, Name, and Extension. If Extension is the null
   --  string, then Name is interpreted as a simple name; otherwise Name is
   --  interpreted as a base name. The exception Name_Error is propagated if
   --  the string given as Containing_Directory is not null and does not allow
   --  the identification of a directory, or if the string given as Extension
   --  is not null and is not a possible extension, or if the string given as
   --  Name is not a possible simple name (if Extension is null) or base name
   --  (if Extension is non-null).

   --------------------------------
   -- File and directory queries --
   --------------------------------

   subtype File_Kind is Ada.Directories.File_Kind;
   --  The type File_Kind represents the kind of file represented by an
   --  external file or directory.

   Directory     : File_Kind renames Ada.Directories.Directory;
   Ordinary_File : File_Kind renames Ada.Directories.Ordinary_File;
   Special_File  : File_Kind renames Ada.Directories.Special_File;

   subtype File_Size is Ada.Directories.File_Size;
   --  The type File_Size represents the size of an external file

   function Exists (Name : String) return Boolean with
     Global => null;
   --  Returns True if external file represented by Name exists, and False
   --  otherwise. The exception Name_Error is propagated if the string given as
   --  Name does not allow the identification of an external file (including
   --  directories and special files).

   function Exists (Simple_Filename     : Aida.Types.String_T;
                    Directory_To_Search : String) return Boolean with
     Global => null,
     Pre    => Exists (Directory_To_Search);
   --  Returns True if external file represented by Simple_Filename exists
   --  in the directory Directory, and False otherwise.

   function Kind (Name : String) return File_Kind with
     Global => null,
     Pre    => Exists (Name);
   --  Returns the kind of external file represented by Name. The exception
   --  Name_Error is propagated if the string given as Name does not allow the
   --  identification of an existing external file.

   function Size (Name : String) return File_Size with
     Global => null,
     Pre    => Exists (Name);
   --  Returns the size of the external file represented by Name. The size of
   --  an external file is the number of stream elements contained in the file.
   --  If the external file is discontiguous (not all elements exist), the
   --  result is implementation-defined. If the external file is not an
   --  ordinary file, the result is implementation-defined. The exception
   --  Name_Error is propagated if the string given as Name does not allow the
   --  identification of an existing external file. The exception
   --  Constraint_Error is propagated if the file size is not a value of type
   --  File_Size.

   function Modification_Time (Name : String) return Ada.Calendar.Time with
     Global => null,
     Pre    => Exists (Name);
   --  Returns the time that the external file represented by Name was most
   --  recently modified. If the external file is not an ordinary file, the
   --  result is implementation-defined. The exception Name_Error is propagated
   --  if the string given as Name does not allow the identification of an
   --  existing external file. The exception Use_Error is propagated if the
   --  external environment does not support the reading the modification time
   --  of the file with the name given by Name (in the absence of Name_Error).

   -------------------------
   -- Directory Searching --
   -------------------------

   subtype Directory_Entry_Type is Ada.Directories.Directory_Entry_Type;
   --  The type Directory_Entry_Type represents a single item in a directory.
   --  These items can only be created by the Get_Next_Entry procedure in this
   --  package. Information about the item can be obtained from the functions
   --  declared in this package. A default initialized object of this type is
   --  invalid; objects returned from Get_Next_Entry are valid.

   subtype Filter_Type is Ada.Directories.Filter_Type;
   --  The type Filter_Type specifies which directory entries are provided from
   --  a search operation. If the Directory component is True, directory
   --  entries representing directories are provided. If the Ordinary_File
   --  component is True, directory entries representing ordinary files are
   --  provided. If the Special_File component is True, directory entries
   --  representing special files are provided.

   subtype Search_Type is Ada.Directories.Search_Type;
   --  The type Search_Type contains the state of a directory search. A
   --  default-initialized Search_Type object has no entries available
   --  (More_Entries returns False).

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True)) with
     Global => null,
     Pre    => Exists (Directory);
   --  Starts a search in the directory entry in the directory named by
   --  Directory for entries matching Pattern. Pattern represents a file name
   --  matching pattern. If Pattern is null, all items in the directory are
   --  matched; otherwise, the interpretation of Pattern is implementation-
   --  defined. Only items which match Filter will be returned. After a
   --  successful call on Start_Search, the object Search may have entries
   --  available, but it may have no entries available if no files or
   --  directories match Pattern and Filter. The exception Name_Error is
   --  propagated if the string given by Directory does not identify an
   --  existing directory, or if Pattern does not allow the identification of
   --  any possible external file or directory. The exception Use_Error is
   --  propagated if the external environment does not support the searching
   --  of the directory with the given name (in the absence of Name_Error).

   procedure End_Search (Search : in out Search_Type) with
     Global => null;
   --  Ends the search represented by Search. After a successful call on
   --  End_Search, the object Search will have no entries available. Note
   --  that it is not necessary to call End_Search if the call to Start_Search
   --  was unsuccessful and raised an exception (but it is harmless to make
   --  the call in this case).

   function More_Entries (Search : Search_Type) return Boolean with
     Global => null;
   --  Returns True if more entries are available to be returned by a call
   --  to Get_Next_Entry for the specified search object, and False otherwise.

   procedure Get_Next_Entry
     (Search          : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type) with
     Global => null;
   --  Returns the next Directory_Entry for the search described by Search that
   --  matches the pattern and filter. If no further matches are available,
   --  Status_Error is raised. It is implementation-defined as to whether the
   --  results returned by this routine are altered if the contents of the
   --  directory are altered while the Search object is valid (for example, by
   --  another program). The exception Use_Error is propagated if the external
   --  environment does not support continued searching of the directory
   --  represented by Search.

--     procedure Search
--       (Directory : String;
--        Pattern   : String;
--        Filter    : Filter_Type := (others => True);
--        Process   : not null access procedure (Directory_Entry : Directory_Entry_Type)) with
--         SPARK_Mode => Off,
--         Global => null,
--         Pre    => Exists (Directory);
   --  Searches in the directory named by Directory for entries matching
   --  Pattern. The subprogram designated by Process is called with each
   --  matching entry in turn. Pattern represents a pattern for matching file
   --  names. If Pattern is null, all items in the directory are matched;
   --  otherwise, the interpretation of Pattern is implementation-defined.
   --  Only items that match Filter will be returned. The exception Name_Error
   --  is propagated if the string given by Directory does not identify
   --  an existing directory, or if Pattern does not allow the identification
   --  of any possible external file or directory. The exception Use_Error is
   --  propagated if the external environment does not support the searching
   --  of the directory with the given name (in the absence of Name_Error).

   -------------------------------------
   -- Operations on Directory Entries --
   -------------------------------------

   function Simple_Name (Directory_Entry : Directory_Entry_Type) return String with
     Global => null;
   --  TODO: Can this contract be refined?
   --
   --  Returns the simple external name of the external file (including
   --  directories) represented by Directory_Entry. The format of the name
   --  returned is implementation-defined. The exception Status_Error is
   --  propagated if Directory_Entry is invalid.

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String with
     Global => null,
     Post   => Exists (Full_Name'Result);
   --  Returns the full external name of the external file (including
   --  directories) represented by Directory_Entry. The format of the name
   --  returned is implementation-defined. The exception Status_Error is
   --  propagated if Directory_Entry is invalid.

   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind with
     Global => null;
   --  Returns the kind of external file represented by Directory_Entry. The
   --  exception Status_Error is propagated if Directory_Entry is invalid.

   function Size (Directory_Entry : Directory_Entry_Type) return File_Size with
     Global => null;
   --  Returns the size of the external file represented by Directory_Entry.
   --  The size of an external file is the number of stream elements contained
   --  in the file. If the external file is discontiguous (not all elements
   --  exist), the result is implementation-defined. If the external file
   --  represented by Directory_Entry is not an ordinary file, the result is
   --  implementation-defined. The exception Status_Error is propagated if
   --  Directory_Entry is invalid. The exception Constraint_Error is propagated
   --  if the file size is not a value of type File_Size.

   function Modification_Time
     (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time with
     Global => null;
   --  Returns the time that the external file represented by Directory_Entry
   --  was most recently modified. If the external file represented by
   --  Directory_Entry is not an ordinary file, the result is
   --  implementation-defined. The exception Status_Error is propagated if
   --  Directory_Entry is invalid. The exception Use_Error is propagated if
   --  the external environment does not support the reading the modification
   --  time of the file represented by Directory_Entry.

--  private
--     pragma SPARK_Mode (Off);

--     function Exists (Name : String) return Boolean renames Ada.Directories.Exists;
--
--     function Current_Directory return String renames Ada.Directories.Current_Directory;
--
--     function Compose (Containing_Directory : String := "";
--                       Name                 : String;
--                       Extension            : String := "") return String renames Ada.Directories.Compose;
--
--     function Containing_Directory (Name : String) return String renames Ada.Directories.Containing_Directory;
--
--     procedure Set_Directory (Directory : String) renames Ada.Directories.Set_Directory;
--
--     procedure Create_Directory
--       (New_Directory : String;
--        Form          : String := "") renames Ada.Directories.Create_Directory;
--
--     procedure Delete_Directory (Directory : String) renames Ada.Directories.Delete_Directory;
--
--     procedure Create_Path
--       (New_Directory : String;
--        Form          : String := "") renames Ada.Directories.Create_Path;
--
--     procedure Delete_Tree (Directory : String) renames Ada.Directories.Delete_Tree;
--
--     procedure Delete_File (Name : String) renames Ada.Directories.Delete_File;
--
--     procedure Rename (Old_Name, New_Name : String) renames Ada.Directories.Rename;
--
--     procedure Copy_File
--       (Source_Name   : String;
--        Target_Name   : String;
--        Form          : String := "") renames Ada.Directories.Copy_File;
--
--     function Full_Name (Name : String) return String renames Ada.Directories.Full_Name;
--
--     function Simple_Name (Name : String) return String renames Ada.Directories.Simple_Name;
--
--     function Containing_Directory (Name : String) return String renames Ada.Directories.Containing_Directory;
--
--     function Extension (Name : String) return String renames Ada.Directories.Extension;
--
--     function Base_Name (Name : String) return String renames Ada.Directories.Base_Name;
--
--     function Compose (Containing_Directory : String := "";
--                       Name                 : String;
--                       Extension            : String := "") return String renames Ada.Directories.Compose;
--
--     function Exists (Name : String) return Boolean renames Ada.Directories.Exists;
--
--     function Kind (Name : String) return File_Kind renames Ada.Directories.Kind;
--
--     function Size (Name : String) return File_Size renames Ada.Directories.Size;
--
--     function Modification_Time (Name : String) return Ada.Calendar.Time renames Ada.Directories.Modification_Time;
--
--     procedure Start_Search
--       (Search    : in out Search_Type;
--        Directory : String;
--        Pattern   : String;
--        Filter    : Filter_Type := (others => True)) renames Ada.Directories.Start_Search;
--
--     procedure End_Search (Search : in out Search_Type) renames Ada.Directories.End_Search;
--
--     function More_Entries (Search : Search_Type) return Boolean renames Ada.Directories.More_Entries;
--
--     procedure Get_Next_Entry
--       (Search          : in out Search_Type;
--        Directory_Entry : out Directory_Entry_Type) renames Ada.Directories.Get_Next_Entry;
--
--  --     procedure Search
--  --       (Directory : String;
--  --        Pattern   : String;
--  --        Filter    : Filter_Type := (others => True);
--  --        Process   : not null access procedure (Directory_Entry : Directory_Entry_Type)) renames Ada.Directories.Search;
--
--     function Simple_Name (Directory_Entry : Directory_Entry_Type) return String renames Ada.Directories.Simple_Name;
--
--     function Full_Name (Directory_Entry : Directory_Entry_Type) return String renames Ada.Directories.Full_Name;
--
--     function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind renames Ada.Directories.Kind;
--
--     function Size (Directory_Entry : Directory_Entry_Type) return File_Size renames Ada.Directories.Size;
--
--     function Modification_Time
--       (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time renames Ada.Directories.Modification_Time;

end Aida.Directories;
