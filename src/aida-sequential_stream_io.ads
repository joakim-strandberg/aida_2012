with Ada.Sequential_IO;
with Ada.Streams;
with Aida.Directories;

package Aida.Sequential_Stream_IO with SPARK_Mode is

   function Calculate_Hash32 (Filename : String) return Aida.Hash32_T with
     Global => null,
     Pre    => Aida.Directories.Exists (Filename);

   type File_Type is limited private with Default_Initial_Condition => not Is_Open (File_Type);

   type File_Mode is (In_File, Out_File, Append_File);

   procedure Open (File : in out File_Type;
                   Mode : File_Mode;
                   Name : String) with
     Global => null,
     Pre    => not Is_Open (File),
     Post   => Is_Open (File);

   procedure Close  (File : in out File_Type) with
     Global => null,
     Pre    => Is_Open (File),
     Post   => not Is_Open (File);

   function Is_Open (File : File_Type) return Boolean with
     Global => null;

   ---------------------------------
   -- Input and output operations --
   ---------------------------------

   procedure Read  (File : File_Type; Item : out Ada.Streams.Stream_Element) with
     Global => null,
     Pre    => Is_Open (File);

   procedure Write (File : File_Type; Item : Ada.Streams.Stream_Element) with
     Global => null,
     Pre    => Is_Open (File);

   function End_Of_File (File : File_Type) return Boolean with
     Global => null;

private
   pragma SPARK_Mode (Off);

   package File_IO is new Ada.Sequential_IO (Ada.Streams.Stream_Element);

   type File_Type is record
      File : File_IO.File_Type;
   end record;

end Aida.Sequential_Stream_IO;
