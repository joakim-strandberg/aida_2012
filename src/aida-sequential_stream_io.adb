package body Aida.Sequential_Stream_IO is

   function Calculate_Hash
     (Filename : String) return Ada.Containers.Hash_Type with
     SPARK_Mode => On
   is
      File : File_Type;

      Element : Ada.Streams.Stream_Element;

      H : Ada.Containers.Hash_Type := 0;

      use type Ada.Containers.Hash_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Filename);

      while not End_Of_File (File) loop
         Read (File, Element);
         H := 3*H + Ada.Containers.Hash_Type (Element);
      end loop;

      Close (File);

      if Is_Open (File) then
         raise File_IO.Status_Error with "File should be closed: " & Filename;
      end if;

      return H;
   end Calculate_Hash;

   procedure Create (File : in out File_Type;
                     Mode : File_Mode := Out_File;
                     Name : String;
                     Form : String := "") is
   begin
      case Mode is
         when In_File  => File_IO.Create (File => File.File,
                                          Mode => File_IO.In_File,
                                          Name => Name,
                                          Form => Form);
         when Out_File  => File_IO.Create (File => File.File,
                                           Mode => File_IO.Out_File,
                                           Name => Name,
                                           Form => Form);
         when Append_File => File_IO.Create (File => File.File,
                                             Mode => File_IO.Append_File,
                                             Name => Name,
                                             Form => Form);
      end case;
   end Create;

   procedure Open (File : in out File_Type;
                   Mode : File_Mode;
                   Name : String) is
   begin
      case Mode is
         when In_File  => File_IO.Open (File => File.File,
                                        Mode => File_IO.In_File,
                                        Name => Name);
         when Out_File  => File_IO.Open (File => File.File,
                                        Mode => File_IO.Out_File,
                                        Name => Name);
         when Append_File => File_IO.Open (File => File.File,
                                        Mode => File_IO.Append_File,
                                        Name => Name);
      end case;
   end Open;

   procedure Close (File : in out File_Type) is
   begin
      File_IO.Close (File.File);
   end Close;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return File_IO.Is_Open (File.File);
   end Is_Open;

   procedure Read (File : File_Type; Item : out Ada.Streams.Stream_Element) is
   begin
      File_IO.Read (File.File, Item);
   end Read;

   procedure Write (File : File_Type; Item : Ada.Streams.Stream_Element) is
   begin
      File_IO.Write (File.File, Item);
   end Write;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return File_IO.End_Of_File (File.File);
   end End_Of_File;

end Aida.Sequential_Stream_IO;
