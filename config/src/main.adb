with Aida.Text_IO;
with Aida.Directories;
with Aida.Containers.Integer_To_String_Map;
with Aida.Types;
with Aida.Sequential_Stream_IO;
with Ada.Streams;

procedure Main with SPARK_Mode is

   use all type Aida.Types.String_T;
   use all type Aida.Types.Int32_T;
   use all type Aida.Types.Hash32_T;
   use all type Aida.Directories.File_Kind;

   procedure Report_Could_Not_Find_Directory (Directory : String) with
     Global => null;

   procedure Report_Could_Not_Find_Directory (Directory : String) is
   begin
      Aida.Text_IO.Put ("Could not find directory: ");
      Aida.Text_IO.Put_Line (Directory);
   end Report_Could_Not_Find_Directory;

   type Specified_OS_T is (
                           Linux,
                           Mac_OS_X,
                           Windows
                           );

   type File_Index_T is range 1..37;

   package OS_Agnostic_Files_Vector is new Aida.Containers.Integer_To_String_Map (Index_T    => File_Index_T,
                                                                                  Element_T  => Aida.Types.String_T,
                                                                                  "="        => Aida.Types."=");

   use type OS_Agnostic_Files_Vector.Length_T;

   OS_AGNOSTIC_MAX : constant := 3_000;

   OS_Agnostic_Files : OS_Agnostic_Files_Vector.T (OS_AGNOSTIC_MAX);


   type OS_Specific_File_Index_T is range 1..1;

   package OS_Specific_Files_Vector is new Aida.Containers.Integer_To_String_Map (Index_T    => OS_Specific_File_Index_T,
                                                                                  Element_T  => Aida.Types.String_T,
                                                                                  "="        => Aida.Types."=");

   use type OS_Specific_Files_Vector.Length_T;

   OS_SPECIFIC_MAX : constant := 100;

   Linux_Files : OS_Specific_Files_Vector.T (OS_SPECIFIC_MAX);

   Mac_OS_X_Files : OS_Specific_Files_Vector.T (OS_SPECIFIC_MAX);

   Windows_Files : OS_Specific_Files_Vector.T (OS_SPECIFIC_MAX);

   package Expected_Files is

      use all type OS_Agnostic_Files_Vector.T;

      use all type OS_Specific_Files_Vector.T;

      procedure Initialize_Data_Structures with
        Global => (In_Out => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Length (OS_Agnostic_Files) = 0 and (Hidden_Index (OS_Agnostic_Files) = 0 and then Available_Capacity (OS_Agnostic_Files) = OS_AGNOSTIC_MAX) and
        Length (Linux_Files) = 0 and (Hidden_Index (Linux_Files) = 0 and then Available_Capacity (Linux_Files) = OS_SPECIFIC_MAX) and
        Length (Mac_OS_X_Files) = 0 and (Hidden_Index (Mac_OS_X_Files) = 0 and then Available_Capacity (Mac_OS_X_Files) = OS_SPECIFIC_MAX) and
        Length (Windows_Files) = 0 and (Hidden_Index (Windows_Files) = 0 and then Available_Capacity (Windows_Files) = OS_SPECIFIC_MAX);

      function Is_File_Among_OS_Agnostic (Searched_For : String) return Boolean with
        Global => OS_Agnostic_Files;

      function Is_File_Among_Linux_Specific (Searched_For : String) return Boolean with
        Global => Linux_Files;

      function Is_File_Among_Mac_OS_X_Specific (Searched_For : String) return Boolean with
        Global => Mac_OS_X_Files;

      function Is_File_Among_Windows_Specific (Searched_For : String) return Boolean with
        Global => Windows_Files;

   private

      function Is_File_Among_OS_Specific (Searched_For : String; OS_Specific_Files : OS_Specific_Files_Vector.T) return Boolean with
        Global => null;

   end Expected_Files;

   package body Expected_Files is

      procedure Initialize_Data_Structures is
      begin
         Append (OS_Agnostic_Files, "aida-bounded_string.ads");
         Append (OS_Agnostic_Files, "aida-bounded_string.adb");
         Append (OS_Agnostic_Files, "aida-character.ads");
         Append (OS_Agnostic_Files, "aida-character.adb");
         Append (OS_Agnostic_Files, "aida-containers-bounded_hash_map.ads");
         Append (OS_Agnostic_Files, "aida-containers-bounded_hash_map.adb");
         Append (OS_Agnostic_Files, "aida-containers-bounded_vector.ads");
         Append (OS_Agnostic_Files, "aida-containers-bounded_vector.adb");
         Append (OS_Agnostic_Files, "aida-containers-integer_to_string_map.ads");
         Append (OS_Agnostic_Files, "aida-containers-integer_to_string_map.adb");
         Append (OS_Agnostic_Files, "aida-containers.ads");
         Append (OS_Agnostic_Files, "aida-directories.ads");
         Append (OS_Agnostic_Files, "aida-directories.adb");
         Append (OS_Agnostic_Files, "aida-hash32.ads");
         Append (OS_Agnostic_Files, "aida-int32.ads");
         Append (OS_Agnostic_Files, "aida-int32.adb");
         Append (OS_Agnostic_Files, "aida-json-generic_parse_json.adb");
         Append (OS_Agnostic_Files, "aida-json-generic_parse_json.ads");
         Append (OS_Agnostic_Files, "aida-json.ads");
         Append (OS_Agnostic_Files, "aida-sequential_stream_io.ads");
         Append (OS_Agnostic_Files, "aida-sequential_stream_io.adb");
         Append (OS_Agnostic_Files, "aida-string.ads");
         Append (OS_Agnostic_Files, "aida-string.adb");
         Append (OS_Agnostic_Files, "aida-subprogram_call_result.ads");
         Append (OS_Agnostic_Files, "aida-subprogram_call_result.adb");
         Append (OS_Agnostic_Files, "aida-text_io.ads");
         Append (OS_Agnostic_Files, "aida-text_io.adb");
         Append (OS_Agnostic_Files, "aida-types.ads");
         Append (OS_Agnostic_Files, "aida-types.adb");
         Append (OS_Agnostic_Files, "aida-utf8.ads");
         Append (OS_Agnostic_Files, "aida-utf8.adb");
         Append (OS_Agnostic_Files, "aida-utf8_code_point.ads");
         Append (OS_Agnostic_Files, "aida-utf8_code_point.adb");
         Append (OS_Agnostic_Files, "aida-xml.ads");
         Append (OS_Agnostic_Files, "aida-xml-generic_parse_xml_file.ads");
         Append (OS_Agnostic_Files, "aida-xml-generic_parse_xml_file.adb");
         Append (OS_Agnostic_Files, "aida.ads");

         Append (Linux_Files, "aida-os_lib.ads");
         Append (Mac_OS_X_Files, "aida-os_lib.ads");
         Append (Windows_Files, "aida-os_lib.ads");
      end Initialize_Data_Structures;

      function Is_Equal (Text         : Aida.Types.String_T;
                         Searched_For : String) return Boolean with
        Global => null;

      function Is_Equal (Text         : Aida.Types.String_T;
                         Searched_For : String) return Boolean is
      begin
         return Equivalent (Searched_For, Text);
      end Is_Equal;

      function Is_File_Among_OS_Specific (Searched_For      : String;
                                          OS_Specific_Files : OS_Specific_Files_Vector.T) return Boolean
      is

         subtype Arg_T is String (Searched_For'First..Searched_For'Last);

         function Is_Equal is new OS_Specific_Files_Vector.Check_Something_On_Immutable_Text (Return_T            => Boolean,
                                                                                              Arg_T               => Arg_T,
                                                                                              Check_Something     => Is_Equal);

         Result : Boolean := False;

      begin
         for I in OS_Specific_File_Index_T range First_Index (OS_Specific_Files)..Last_Index (OS_Specific_Files) loop
            if Is_Equal (OS_Specific_Files, I, Searched_For) then
               Result := True;
               exit;
            end if;
         end loop;

         return Result;
      end Is_File_Among_OS_Specific;

      function Is_File_Among_Linux_Specific (Searched_For : String) return Boolean is (Is_File_Among_OS_Specific (Searched_For, Linux_Files));

      function Is_File_Among_Mac_OS_X_Specific (Searched_For : String) return Boolean is (Is_File_Among_OS_Specific (Searched_For, Mac_OS_X_Files));

      function Is_File_Among_Windows_Specific (Searched_For : String) return Boolean is (Is_File_Among_OS_Specific (Searched_For, Windows_Files));

      function Is_File_Among_OS_Agnostic (Searched_For : String) return Boolean is

         subtype Arg_T is String (Searched_For'First..Searched_For'Last);

         function Is_Equal is new OS_Agnostic_Files_Vector.Check_Something_On_Immutable_Text (Return_T            => Boolean,
                                                                                              Arg_T               => Arg_T,
                                                                                              Check_Something     => Is_Equal);

         Result : Boolean := False;

      begin
         for I in File_Index_T range First_Index (OS_Agnostic_Files)..Last_Index (OS_Agnostic_Files) loop
            if Is_Equal (OS_Agnostic_Files, I, Searched_For) then
               Result := True;
               exit;
            end if;
         end loop;

         return Result;
      end Is_File_Among_OS_Agnostic;

   end Expected_Files;

   use all type OS_Agnostic_Files_Vector.T;

   use all type OS_Specific_Files_Vector.T;

   procedure Copy_Files_From_Source_To_Target_Directory (Source_Directory   : String;
                                                         Target_Directory   : String) with
     Global => null,
     Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory);

   procedure Copy_Files_From_Source_To_Target_Directory (Source_Directory   : String;
                                                         Target_Directory   : String)
   is
      Search : Aida.Directories.Search_Type;

      Filter : constant Aida.Directories.Filter_Type := (
                                                         Directory     => False,
                                                         Ordinary_File => True,
                                                         Special_File  => False
                                                        );

      Directory_Entry : Aida.Directories.Directory_Entry_Type;

      Is_Success : Boolean := True;
   begin
      Aida.Text_IO.Put_Line ("Will copy files from ", Source_Directory, " to ", Target_Directory);

      Aida.Directories.Start_Search (Search    => Search,
                                     Directory => Source_Directory,
                                     Pattern   => "*",
                                     Filter    => Filter);
      while Aida.Directories.More_Entries (Search) and Is_Success loop
         Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
         declare
            Source_File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Source_Directory,
                                                                            Name                 => Aida.Directories.Simple_Name (Directory_Entry));
            Target_File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Target_Directory,
                                                                            Name                 => Aida.Directories.Simple_Name (Directory_Entry));
         begin
            if Aida.Directories.Exists (Source_File_Name) then
               if not Aida.Directories.Exists (Target_File_Name) then
                  Aida.Text_IO.Put_Line ("Copying ", Aida.Directories.Simple_Name (Directory_Entry));
                  Aida.Directories.Copy_File (Source_Name => Source_File_Name,
                                              Target_Name => Target_File_Name);
               else
                  Aida.Text_IO.Put_Line ("Error. Target file exists: ", Target_File_Name);
               end if;
            else
               Aida.Text_IO.Put_Line ("Error. File does not exist: ", Source_File_Name);
               Is_Success := False;
            end if;
         end;
      end loop;
      if Is_Success then
         Aida.Text_IO.Put_Line ("Done.");
      end if;
   end Copy_Files_From_Source_To_Target_Directory;

   procedure Delete_Files_In_Target_Directory (Source_Directory   : String;
                                               Target_Directory   : String;
                                               Linux_Directory    : String;
                                               Mac_OS_X_Directory : String;
                                               Windows_Directory  : String;
                                               Specified_OS       : Specified_OS_T) with
     Global => null,
     Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

   procedure Delete_Files_In_Target_Directory (Source_Directory   : String;
                                               Target_Directory   : String;
                                               Linux_Directory    : String;
                                               Mac_OS_X_Directory : String;
                                               Windows_Directory  : String;
                                               Specified_OS       : Specified_OS_T)
   is
      Search : Aida.Directories.Search_Type;

      Filter : constant Aida.Directories.Filter_Type := (
                                                         Directory     => False,
                                                         Ordinary_File => True,
                                                         Special_File  => False
                                                        );

      Directory_Entry : Aida.Directories.Directory_Entry_Type;

      Is_Success : Boolean := True;
   begin
      Aida.Text_IO.Put_Line ("Will delete files in target directory...");
      Aida.Directories.Start_Search (Search    => Search,
                                     Directory => Target_Directory,
                                     Pattern   => "*",
                                     Filter    => Filter);
      while Aida.Directories.More_Entries (Search) and Is_Success loop
         Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
         declare
            File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Target_Directory,
                                                                     Name                 => Aida.Directories.Simple_Name (Directory_Entry));
         begin
            if Aida.Directories.Exists (File_Name) then
               Aida.Directories.Delete_File (File_Name);
            else
               Aida.Text_IO.Put_Line ("Cannot delete file. Will abort");
               Is_Success := False;
            end if;
         end;
      end loop;

      if Is_Success then
         Copy_Files_From_Source_To_Target_Directory (Source_Directory, Target_Directory);

         case Specified_OS is
            when Linux    => Copy_Files_From_Source_To_Target_Directory (Linux_Directory, Target_Directory);
            when Mac_OS_X => Copy_Files_From_Source_To_Target_Directory (Mac_OS_X_Directory, Target_Directory);
            when Windows  => Copy_Files_From_Source_To_Target_Directory (Windows_Directory, Target_Directory);
         end case;
      end if;
   end Delete_Files_In_Target_Directory;

   procedure Check_Expected_Files_In_Target_Directory (Source_Directory   : String;
                                                       Target_Directory   : String;
                                                       Linux_Directory    : String;
                                                       Mac_OS_X_Directory : String;
                                                       Windows_Directory  : String;
                                                       Specified_OS       : Specified_OS_T) with
     Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
     Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
     Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

   procedure Check_Expected_Files_In_Target_Directory (Source_Directory   : String;
                                                       Target_Directory   : String;
                                                       Linux_Directory    : String;
                                                       Mac_OS_X_Directory : String;
                                                       Windows_Directory  : String;
                                                       Specified_OS       : Specified_OS_T)
   is
      Search : Aida.Directories.Search_Type;

      Filter : constant Aida.Directories.Filter_Type := (
                                                         Directory     => False,
                                                         Ordinary_File => True,
                                                         Special_File  => False
                                                        );

      Directory_Entry : Aida.Directories.Directory_Entry_Type;

      Is_Success : Boolean := True;
   begin
      Aida.Directories.Start_Search (Search    => Search,
                                     Directory => Target_Directory,
                                     Pattern   => "*",
                                     Filter    => Filter);
      while Aida.Directories.More_Entries (Search) and Is_Success loop
         Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
         declare
            Target_Simple_File_Name : constant String := Aida.Directories.Simple_Name (Directory_Entry);

            Target_File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Target_Directory,
                                                                            Name                 => Target_Simple_File_Name);
         begin
            if
              Expected_Files.Is_File_Among_OS_Agnostic (Target_Simple_File_Name) or
              Expected_Files.Is_File_Among_Linux_Specific (Target_Simple_File_Name) or
              Expected_Files.Is_File_Among_Mac_OS_X_Specific (Target_Simple_File_Name) or
              Expected_Files.Is_File_Among_Windows_Specific (Target_Simple_File_Name)
            then
               if Aida.Directories.Exists (Target_File_Name) then
                  if Expected_Files.Is_File_Among_OS_Agnostic (Target_Simple_File_Name) then
                     declare
                        Source_File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Source_Directory,
                                                                                        Name                 => Target_Simple_File_Name);
                     begin
                        if Aida.Directories.Exists (Source_File_Name) then
                           declare
                              Target_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Target_File_Name);
                              Source_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Source_File_Name);
                           begin
                              Is_Success := Target_Hash = Source_Hash;
                              if not Is_Success then
                                 Aida.Text_IO.Put_Line ("Files differ ", Target_File_Name, " and ", Source_File_Name, ". Will abort.");
                              end if;
                           end;
                        else
                           Aida.Text_IO.Put_Line ("File unexpectedly disappeared: ", Source_File_Name, ". Will abort");
                           Is_Success := False;
                        end if;
                     end;
                  else
                     declare
                        Are_Files_Equal : Boolean := False;
                     begin
                        declare
                           Source_File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Linux_Directory,
                                                                                           Name                 => Target_Simple_File_Name);
                        begin
                           if Aida.Directories.Exists (Source_File_Name) then
                              declare
                                 Target_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Target_File_Name);
                                 Source_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Source_File_Name);
                              begin
                                 Are_Files_Equal := Target_Hash = Source_Hash;
                              end;
                           else
                              Are_Files_Equal := False;
                           end if;
                        end;

                        if not Are_Files_Equal then
                           declare
                              Source_File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Mac_OS_X_Directory,
                                                                                              Name                 => Target_Simple_File_Name);
                           begin
                              if Aida.Directories.Exists (Source_File_Name) then
                                 declare
                                    Target_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Target_File_Name);
                                    Source_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Source_File_Name);
                                 begin
                                    Are_Files_Equal := Target_Hash = Source_Hash;
                                 end;
                              else
                                 Are_Files_Equal := False;
                              end if;
                           end;
                        end if;

                        if not Are_Files_Equal then
                           declare
                              Source_File_Name : constant String := Aida.Directories.Compose (Containing_Directory => Windows_Directory,
                                                                                              Name                 => Target_Simple_File_Name);
                           begin
                              if Aida.Directories.Exists (Source_File_Name) then
                                 declare
                                    Target_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Target_File_Name);
                                    Source_Hash : constant Aida.Types.Hash32_T := Aida.Sequential_Stream_IO.Calculate_Hash32 (Source_File_Name);
                                 begin
                                    Are_Files_Equal := Target_Hash = Source_Hash;
                                 end;
                              else
                                 Are_Files_Equal := False;
                              end if;
                           end;
                        end if;

                        Is_Success := Are_Files_Equal;

                        if not Is_Success then
                           Aida.Text_IO.Put_Line ("Have not been able to match file ", Target_Simple_File_Name, " with any source file. Will abort.");
                        end if;
                     end;
                  end if;
               else
                  Aida.Text_IO.Put_Line ("File unexpectedly disappeared: ", Target_File_Name, ". Will abort");
                  Is_Success := False;
               end if;
            else
               Aida.Text_IO.Put_Line ("Unexpected file ", Target_File_Name, " in target directory. Will abort.");
               Is_Success := False;
            end if;
         end;
      end loop;
      Aida.Directories.End_Search (Search);

      if Is_Success then
         Delete_Files_In_Target_Directory (Source_Directory,
                                           Target_Directory,
                                           Linux_Directory,
                                           Mac_OS_X_Directory,
                                           Windows_Directory,
                                           Specified_OS);
      end if;
   end Check_Expected_Files_In_Target_Directory;

   procedure Check_Expected_Files_In_Source_Directory (Source_Directory   : String;
                                                       Target_Directory   : String;
                                                       Linux_Directory    : String;
                                                       Mac_OS_X_Directory : String;
                                                       Windows_Directory  : String;
                                                       Specified_OS       : Specified_OS_T) with
     Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
     Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
     Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

   procedure Check_Expected_Files_In_Source_Directory (Source_Directory   : String;
                                                       Target_Directory   : String;
                                                       Linux_Directory    : String;
                                                       Mac_OS_X_Directory : String;
                                                       Windows_Directory  : String;
                                                       Specified_OS       : Specified_OS_T)
   is
      function Exists (Text                 : Aida.Types.String_T;
                       Containing_Directory : String) return Boolean with
        Global => null;

      function Exists (Text                 : Aida.Types.String_T;
                       Containing_Directory : String) return Boolean
      is
         Is_Found : Boolean := False;
      begin
         if Aida.Directories.Exists (Containing_Directory) then
            Is_Found := Aida.Directories.Exists (Simple_Filename     => Text,
                                                 Directory_To_Search => Containing_Directory);

            if not Is_Found then
               Aida.Text_IO.Put_Line ("Could not find expected file ", String (Text), " in directory ", Containing_Directory);
            end if;
         end if;

         return Is_Found;
      end Exists;

      procedure Compare_Windows_Directory_With_Expected_Files (Source_Directory   : String;
                                                                Target_Directory   : String;
                                                                Linux_Directory    : String;
                                                                Mac_OS_X_Directory : String;
                                                                Windows_Directory  : String;
                                                                Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_Windows_Directory_With_Expected_Files (Source_Directory   : String;
                                                                Target_Directory   : String;
                                                                Linux_Directory    : String;
                                                                Mac_OS_X_Directory : String;
                                                                Windows_Directory  : String;
                                                                Specified_OS       : Specified_OS_T)
      is
         Search : Aida.Directories.Search_Type;

         Filter : constant Aida.Directories.Filter_Type := (
                                                            Directory     => False,
                                                            Ordinary_File => True,
                                                            Special_File  => False
                                                           );

         Directory_Entry : Aida.Directories.Directory_Entry_Type;

         Is_Success : Boolean := True;
      begin
         Aida.Directories.Start_Search (Search    => Search,
                                        Directory => Windows_Directory,
                                        Pattern   => "*",
                                        Filter    => Filter);
         while Aida.Directories.More_Entries (Search) and Is_Success loop
            Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
            declare
               Simple_File_Name : constant String := Aida.Directories.Simple_Name (Directory_Entry);
            begin
               if not Expected_Files.Is_File_Among_Windows_Specific (Simple_File_Name) then
                  Aida.Text_IO.Put_Line ("Found unexpected file ", Simple_File_Name, " in directory ", Linux_Directory);
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;
         Aida.Directories.End_Search (Search);

         if Is_Success then
            Check_Expected_Files_In_Target_Directory (Source_Directory,
                                                      Target_Directory,
                                                      Linux_Directory,
                                                      Mac_OS_X_Directory,
                                                      Windows_Directory,
                                                      Specified_OS);
         end if;
      end Compare_Windows_Directory_With_Expected_Files;

      procedure Compare_Expected_Windows_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                           Target_Directory   : String;
                                                                           Linux_Directory    : String;
                                                                           Mac_OS_X_Directory : String;
                                                                           Windows_Directory  : String;
                                                                           Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_Expected_Windows_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                           Target_Directory   : String;
                                                                           Linux_Directory    : String;
                                                                           Mac_OS_X_Directory : String;
                                                                           Windows_Directory  : String;
                                                                           Specified_OS       : Specified_OS_T)
      is
         Is_Success : Boolean := True;
      begin
         for I in OS_Specific_File_Index_T range First_Index (Windows_Files)..Last_Index (Windows_Files) loop
            declare
               subtype Arg_T is String (Windows_Directory'First..Windows_Directory'Last);

               function Exists is new OS_Specific_Files_Vector.Check_Something_On_Immutable_Text (Boolean, Arg_T, Exists);
            begin
               if not Exists (Windows_Files, I, Windows_Directory) then
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;

         if Is_Success then
            Compare_Windows_Directory_With_Expected_Files (Source_Directory,
                                                           Target_Directory,
                                                           Linux_Directory,
                                                           Mac_OS_X_Directory,
                                                           Windows_Directory,
                                                           Specified_OS);
         end if;
      end Compare_Expected_Windows_Files_With_Contents_Of_Directory;

      procedure Compare_Mac_OS_X_Directory_With_Expected_Files (Source_Directory   : String;
                                                                Target_Directory   : String;
                                                                Linux_Directory    : String;
                                                                Mac_OS_X_Directory : String;
                                                                Windows_Directory  : String;
                                                                Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_Mac_OS_X_Directory_With_Expected_Files (Source_Directory   : String;
                                                                Target_Directory   : String;
                                                                Linux_Directory    : String;
                                                                Mac_OS_X_Directory : String;
                                                                Windows_Directory  : String;
                                                                Specified_OS       : Specified_OS_T)
      is
         Search : Aida.Directories.Search_Type;

         Filter : constant Aida.Directories.Filter_Type := (
                                                            Directory     => False,
                                                            Ordinary_File => True,
                                                            Special_File  => False
                                                           );

         Directory_Entry : Aida.Directories.Directory_Entry_Type;

         Is_Success : Boolean := True;
      begin
         Aida.Directories.Start_Search (Search    => Search,
                                        Directory => Mac_OS_X_Directory,
                                        Pattern   => "*",
                                        Filter    => Filter);
         while Aida.Directories.More_Entries (Search) and Is_Success loop
            Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
            declare
               Simple_File_Name : constant String := Aida.Directories.Simple_Name (Directory_Entry);
            begin
               if not Expected_Files.Is_File_Among_Mac_OS_X_Specific (Simple_File_Name) then
                  Aida.Text_IO.Put_Line ("Found unexpected file ", Simple_File_Name, " in directory ", Linux_Directory);
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;
         Aida.Directories.End_Search (Search);

         if Is_Success then
            Compare_Expected_Windows_Files_With_Contents_Of_Directory (Source_Directory,
                                                                       Target_Directory,
                                                                       Linux_Directory,
                                                                       Mac_OS_X_Directory,
                                                                       Windows_Directory,
                                                                       Specified_OS);
         end if;
      end Compare_Mac_OS_X_Directory_With_Expected_Files;

      procedure Compare_Expected_Mac_OS_X_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                            Target_Directory   : String;
                                                                            Linux_Directory    : String;
                                                                            Mac_OS_X_Directory : String;
                                                                            Windows_Directory  : String;
                                                                            Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_Expected_Mac_OS_X_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                            Target_Directory   : String;
                                                                            Linux_Directory    : String;
                                                                            Mac_OS_X_Directory : String;
                                                                            Windows_Directory  : String;
                                                                            Specified_OS       : Specified_OS_T)
      is
         Is_Success : Boolean := True;
      begin
         for I in OS_Specific_File_Index_T range First_Index (Mac_OS_X_Files)..Last_Index (Mac_OS_X_Files) loop
            declare
               subtype Arg_T is String (Mac_OS_X_Directory'First..Mac_OS_X_Directory'Last);

               function Exists is new OS_Specific_Files_Vector.Check_Something_On_Immutable_Text (Boolean, Arg_T, Exists);
            begin
               if not Exists (Mac_OS_X_Files, I, Mac_OS_X_Directory) then
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;

         if Is_Success then
            Compare_Mac_OS_X_Directory_With_Expected_Files (Source_Directory,
                                                            Target_Directory,
                                                            Linux_Directory,
                                                            Mac_OS_X_Directory,
                                                            Windows_Directory,
                                                            Specified_OS);
         end if;
      end Compare_Expected_Mac_OS_X_Files_With_Contents_Of_Directory;

      procedure Compare_Linux_Directory_With_Expected_Files (Source_Directory   : String;
                                                             Target_Directory   : String;
                                                             Linux_Directory    : String;
                                                             Mac_OS_X_Directory : String;
                                                             Windows_Directory  : String;
                                                             Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_Linux_Directory_With_Expected_Files (Source_Directory   : String;
                                                             Target_Directory   : String;
                                                             Linux_Directory    : String;
                                                             Mac_OS_X_Directory : String;
                                                             Windows_Directory  : String;
                                                             Specified_OS       : Specified_OS_T)
      is
         Search : Aida.Directories.Search_Type;

         Filter : constant Aida.Directories.Filter_Type := (
                                                            Directory     => False,
                                                            Ordinary_File => True,
                                                            Special_File  => False
                                                           );

         Directory_Entry : Aida.Directories.Directory_Entry_Type;

         Is_Success : Boolean := True;
      begin
         Aida.Directories.Start_Search (Search    => Search,
                                        Directory => Linux_Directory,
                                        Pattern   => "*",
                                        Filter    => Filter);
         while Aida.Directories.More_Entries (Search) and Is_Success loop
            Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
            declare
               Simple_File_Name : constant String := Aida.Directories.Simple_Name (Directory_Entry);
            begin
               if not Expected_Files.Is_File_Among_Linux_Specific (Simple_File_Name) then
                  Aida.Text_IO.Put_Line ("Found unexpected file ", Simple_File_Name, " in directory ", Linux_Directory);
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;
         Aida.Directories.End_Search (Search);

         if Is_Success then
            Compare_Expected_Mac_OS_X_Files_With_Contents_Of_Directory (Source_Directory,
                                                                        Target_Directory,
                                                                        Linux_Directory,
                                                                        Mac_OS_X_Directory,
                                                                        Windows_Directory,
                                                                        Specified_OS);
         end if;
      end Compare_Linux_Directory_With_Expected_Files;

      procedure Compare_Expected_Linux_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                         Target_Directory   : String;
                                                                         Linux_Directory    : String;
                                                                         Mac_OS_X_Directory : String;
                                                                         Windows_Directory  : String;
                                                                         Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_Expected_Linux_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                            Target_Directory   : String;
                                                                            Linux_Directory    : String;
                                                                            Mac_OS_X_Directory : String;
                                                                            Windows_Directory  : String;
                                                                            Specified_OS       : Specified_OS_T)
      is
         Is_Success : Boolean := True;
      begin
         for I in OS_Specific_File_Index_T range First_Index (Linux_Files)..Last_Index (Linux_Files) loop
            declare
               subtype Arg_T is String (Linux_Directory'First..Linux_Directory'Last);

               function Exists is new OS_Specific_Files_Vector.Check_Something_On_Immutable_Text (Boolean, Arg_T, Exists);
            begin
               if not Exists (Linux_Files, I, Linux_Directory) then
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;

         if Is_Success then
            Compare_Linux_Directory_With_Expected_Files (Source_Directory,
                                                         Target_Directory,
                                                         Linux_Directory,
                                                         Mac_OS_X_Directory,
                                                         Windows_Directory,
                                                         Specified_OS);
         end if;
      end Compare_Expected_Linux_Files_With_Contents_Of_Directory;

      procedure Compare_OS_Agnostic_Directory_With_Expected_Files (Source_Directory   : String;
                                                                   Target_Directory   : String;
                                                                   Linux_Directory    : String;
                                                                   Mac_OS_X_Directory : String;
                                                                   Windows_Directory  : String;
                                                                   Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_OS_Agnostic_Directory_With_Expected_Files (Source_Directory   : String;
                                                                   Target_Directory   : String;
                                                                   Linux_Directory    : String;
                                                                   Mac_OS_X_Directory : String;
                                                                   Windows_Directory  : String;
                                                                   Specified_OS       : Specified_OS_T) is
         Search : Aida.Directories.Search_Type;

         Filter : constant Aida.Directories.Filter_Type := (
                                                            Directory     => False,
                                                            Ordinary_File => True,
                                                            Special_File  => False
                                                           );

         Directory_Entry : Aida.Directories.Directory_Entry_Type;

         Is_Success : Boolean := True;
      begin
         Aida.Directories.Start_Search (Search    => Search,
                                        Directory => Source_Directory,
                                        Pattern   => "*",
                                        Filter    => Filter);
         while Aida.Directories.More_Entries (Search) and Is_Success loop
            Aida.Directories.Get_Next_Entry (Search, Directory_Entry);
            declare
               Simple_File_Name : constant String := Aida.Directories.Simple_Name (Directory_Entry);
            begin
               if not Expected_Files.Is_File_Among_OS_Agnostic (Simple_File_Name) then
                  Aida.Text_IO.Put_Line ("Found unexpected file ", Simple_File_Name, " in directory ", Source_Directory);
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;
         Aida.Directories.End_Search (Search);

         if Is_Success then
            Compare_Expected_Linux_Files_With_Contents_Of_Directory (Source_Directory,
                                                                        Target_Directory,
                                                                        Linux_Directory,
                                                                        Mac_OS_X_Directory,
                                                                        Windows_Directory,
                                                                        Specified_OS);
         end if;
      end Compare_OS_Agnostic_Directory_With_Expected_Files;

      procedure Compare_Expected_OS_Agnostic_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                               Target_Directory   : String;
                                                                               Linux_Directory    : String;
                                                                               Mac_OS_X_Directory : String;
                                                                               Windows_Directory  : String;
                                                                               Specified_OS       : Specified_OS_T) with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
        Pre    => Aida.Directories.Exists (Source_Directory) and Aida.Directories.Exists (Target_Directory) and
        Aida.Directories.Exists (Linux_Directory) and Aida.Directories.Exists (Mac_OS_X_Directory) and Aida.Directories.Exists (Windows_Directory);

      procedure Compare_Expected_OS_Agnostic_Files_With_Contents_Of_Directory (Source_Directory   : String;
                                                                               Target_Directory   : String;
                                                                               Linux_Directory    : String;
                                                                               Mac_OS_X_Directory : String;
                                                                               Windows_Directory  : String;
                                                                               Specified_OS       : Specified_OS_T)
      is
         Is_Success : Boolean := True;
      begin
         for I in File_Index_T range First_Index (OS_Agnostic_Files)..Last_Index (OS_Agnostic_Files) loop
            declare
               subtype Arg_T is String (Source_Directory'First..Source_Directory'Last);

               function Exists is new OS_Agnostic_Files_Vector.Check_Something_On_Immutable_Text (Boolean, Arg_T, Exists);
            begin
               if not Exists (OS_Agnostic_Files, I, Source_Directory) then
                  Is_Success := False;
                  exit;
               end if;
            end;
         end loop;

         if Is_Success then
            Compare_OS_Agnostic_Directory_With_Expected_Files (Source_Directory,
                                                               Target_Directory,
                                                               Linux_Directory,
                                                               Mac_OS_X_Directory,
                                                               Windows_Directory,
                                                               Specified_OS);
         end if;
      end Compare_Expected_OS_Agnostic_Files_With_Contents_Of_Directory;

   begin
      Compare_Expected_OS_Agnostic_Files_With_Contents_Of_Directory (Source_Directory,
                                                                     Target_Directory,
                                                                     Linux_Directory,
                                                                     Mac_OS_X_Directory,
                                                                     Windows_Directory,
                                                                     Specified_OS);
   end Check_Expected_Files_In_Source_Directory;

   procedure Check_Existence_Of_Expected_Directories (Specified_OS : Specified_OS_T) with
     Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files));

   procedure Check_Existence_Of_Expected_Directories (Specified_OS : Specified_OS_T) is
      GNAT_Directory : constant String := Aida.Directories.Current_Directory;
   begin
      if Aida.Directories.Exists (GNAT_Directory) then
         declare
            Build_System_Directory : constant String := Aida.Directories.Compose (Containing_Directory => GNAT_Directory,
                                                                                  Name                 => "..");
         begin
            if Aida.Directories.Exists (Build_System_Directory) then
               declare
                  Config_Directory : constant String := Aida.Directories.Compose (Containing_Directory => Build_System_Directory,
                                                                                  Name                 => "..");
               begin
                  if Aida.Directories.Exists (Config_Directory) then
                     declare
                        Aida_Sources_Directory : constant String := Aida.Directories.Compose (Containing_Directory => Config_Directory,
                                                                                              Name                 => "aida_src");
                     begin
                        if Aida.Directories.Exists (Aida_Sources_Directory) then
                           declare
                              Windows_Directory : constant String := Aida.Directories.Compose (Containing_Directory => Aida_Sources_Directory,
                                                                                               Name                 => "windows");
                           begin
                              if Aida.Directories.Exists (Windows_Directory) then
                                 declare
                                    Linux_Directory : constant String := Aida.Directories.Compose (Containing_Directory => Aida_Sources_Directory,
                                                                                                   Name                 => "linux");
                                 begin
                                    if Aida.Directories.Exists (Linux_Directory) then
                                       declare
                                          Mac_OS_X_Directory : constant String := Aida.Directories.Compose (Containing_Directory => Aida_Sources_Directory,
                                                                                                            Name                 => "mac");
                                       begin
                                          if Aida.Directories.Exists (Mac_OS_X_Directory) then
                                             declare
                                                Repository_Root_Directory : constant String := Aida.Directories.Compose (Containing_Directory => Config_Directory,
                                                                                                                         Name                 => "..");
                                             begin
                                                if Aida.Directories.Exists (Repository_Root_Directory) then
                                                   declare
                                                      Target_Directory : constant String := Aida.Directories.Compose (Containing_Directory => Repository_Root_Directory,
                                                                                                                      Name                 => "src");
                                                   begin
                                                      if Aida.Directories.Exists (Target_Directory) then
                                                         Check_Expected_Files_In_Source_Directory (Source_Directory   => Aida_Sources_Directory,
                                                                                                   Target_Directory   => Target_Directory,
                                                                                                   Linux_Directory    => Linux_Directory,
                                                                                                   Mac_OS_X_Directory => Mac_OS_X_Directory,
                                                                                                   Windows_Directory  => Windows_Directory,
                                                                                                   Specified_OS       => Specified_OS);
                                                      else
                                                         Aida.Directories.Create_Directory (Target_Directory);
                                                         Delete_Files_In_Target_Directory (Source_Directory   => Aida_Sources_Directory,
                                                                                           Target_Directory   => Target_Directory,
                                                                                           Linux_Directory    => Linux_Directory,
                                                                                           Mac_OS_X_Directory => Mac_OS_X_Directory,
                                                                                           Windows_Directory  => Windows_Directory,
                                                                                           Specified_OS       => Specified_OS);
                                                      end if;
                                                   end;
                                                else
                                                   Report_Could_Not_Find_Directory (Repository_Root_Directory);
                                                end if;
                                             end;
                                          else
                                             Report_Could_Not_Find_Directory (Mac_OS_X_Directory);
                                          end if;
                                       end;
                                    else
                                       Report_Could_Not_Find_Directory (Linux_Directory);
                                    end if;
                                 end;
                              else
                                 Report_Could_Not_Find_Directory (Windows_Directory);
                              end if;
                           end;
                        else
                           Report_Could_Not_Find_Directory (Aida_Sources_Directory);
                        end if;
                     end;
                  else
                     Report_Could_Not_Find_Directory (Config_Directory);
                  end if;
               end;
            else
               Report_Could_Not_Find_Directory (Build_System_Directory);
            end if;
         end;
      else
         Report_Could_Not_Find_Directory (GNAT_Directory);
      end if;
   end Check_Existence_Of_Expected_Directories;

   procedure Query_User_For_OS with
     Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files));

   procedure Query_User_For_OS is

      procedure Get_Input_From_User with
        Global => (Input => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files));

      procedure Get_Input_From_User is
         User_Input : constant Aida.Types.String_T := Aida.Types.String_T (Aida.Text_IO.Get_Line);
         Has_Failed : Boolean;

         type Alternative_T is new Aida.Types.Int32_T range 1..4;

         Target : Aida.Types.Int32_T;

         Alternative : Alternative_T;
      begin
         To_Int32 (Source     => User_Input,
                   Target     => Target,
                   Has_failed => Has_Failed);

         if Has_Failed then
            Aida.Text_IO.Put_Line ("Could not interpret user input as an integer: ", String (User_Input));
         else
            if Target >= 1 and Target <= 4 then
               Alternative := Alternative_T (Target);

               case Alternative is
                 when 1 => Check_Existence_Of_Expected_Directories (Linux);
                 when 2 => Check_Existence_Of_Expected_Directories (Mac_OS_X);
                 when 3 => Check_Existence_Of_Expected_Directories (Windows);
                 when 4 => Aida.Text_IO.Put_Line ("Don't know what to do in this case...");
               end case;
            else
               Aida.Text_IO.Put_Line ("Please specify an integer from 1 to 4, not: ", String (User_Input));
            end if;
         end if;
      end Get_Input_From_User;

   begin
      Aida.Text_IO.Put_Line ("There is no function in Ada's standard library that can tell which Operating System an application is running in.");
      Aida.Text_IO.Put_Line ("For which Operating System shall the Aida library be configured for:");
      Aida.Text_IO.Put_Line ("");
      Aida.Text_IO.Put_Line ("1. Linux");
      Aida.Text_IO.Put_Line ("2. Mac OS X");
      Aida.Text_IO.Put_Line ("3. Windows");
      Aida.Text_IO.Put_Line ("4. None of above");
      Get_Input_From_User;
   end Query_User_For_OS;

   procedure Initialize_Data_Structures with
     Global => (In_Out => (OS_Agnostic_Files, Linux_Files, Mac_OS_X_Files, Windows_Files)),
     Pre    => Length (OS_Agnostic_Files) = 0 and (Hidden_Index (OS_Agnostic_Files) = 0 and then Available_Capacity (OS_Agnostic_Files) = OS_AGNOSTIC_MAX) and
     Length (Linux_Files) = 0 and (Hidden_Index (Linux_Files) = 0 and then Available_Capacity (Linux_Files) = OS_SPECIFIC_MAX) and
     Length (Mac_OS_X_Files) = 0 and (Hidden_Index (Mac_OS_X_Files) = 0 and then Available_Capacity (Mac_OS_X_Files) = OS_SPECIFIC_MAX) and
     Length (Windows_Files) = 0 and (Hidden_Index (Windows_Files) = 0 and then Available_Capacity (Windows_Files) = OS_SPECIFIC_MAX);

   procedure Initialize_Data_Structures is
   begin
      Expected_Files.Initialize_Data_Structures;

      Query_User_For_OS;
   end Initialize_Data_Structures;

begin
   Initialize_Data_Structures;
end Main;
