About the Aida library
----------------------

If you are considering making an Ada or SPARK application then the Aida library may
be of interest to you. Developing formally verified software is very time consuming and thus very expensive.
The goal of the Aida library is to lower the cost of mission-critical and security-critical
applications by providing common functionality implemented in formally verified software.

The Aida library consists of (mostly) formally verified SPARK and conforms to the Ada 2012 standard.
It has successfully been compiled using GNAT GPL 2016 on both Ubuntu 16.04 and Windows 10.

Design goals:

 - Minimize external dependencies. If you have an Ada 2012 compiler you are good to go.
 - Use SPARK whenever possible.

The Aida library provides for example basic types (integers, strings,...) for use in projects.
Consider:
```
with Aida.Types;
with Aida.Text_IO;

procedure Main is
   use all type Aida.Types.Int32_T;
   use all type Aida.Types.String_T;

   User_Input : constant Aida.Types.String_T := "123";

   I : constant Aida.Types.Int32_T := To_Int32 (User_Input);

   Result : constant Aida.Types.String_T := To_String (I);
begin
   Aida.Text_IO.Put_Line ("User entered: ", Result);
end Main;
```
The output of the application is:
```
User entered: 123
```
If one would like to formally verify the above code using the SPARK tools (GNAT GPL 2016)
one would need to adjust the source code:
```
with Aida.Types;
with Aida.Text_IO;

procedure Main with SPARK_Mode is
   use all type Aida.Types.Int32_T;
   use all type Aida.Types.Character_T;
   use all type Aida.Types.String_T;

   User_Input : constant Aida.Types.String_T := "123";
begin
   if (for all I in User_Input'Range => Is_Digit (Aida.Types.Character_T (User_Input (I)))) then
      declare
         I : constant Aida.Types.Int32_T := To_Int32 (User_Input);

         Result : constant Aida.Types.String_T := To_String (I);
      begin
         Aida.Text_IO.Put_Line ("User entered: ", Result);
      end;
   else
      Aida.Text_IO.Put_Line (User_Input, " contains non-integer types.");
   end if;
end Main;
```
One possible interpretation of this result is that from the definition of the variable
User_Input the SPARK tools only pay attention to the type of User_Input (it is of Aida.Types.String_T type)
and the length of the "string", not the exact contents. Thus, to satisfy the
preconditions of the To_Int32 (..) function one must first check that all characters
are indeed integers by an explicit if-statement. Also note that keeping track on
the exact contents of "string" types are usually not required during static code
analysis since the exacts contents of user input are not known until run-time.

The traditional way of converting an integer to a string is to use the Integer'Image (..)
function and to use the Integer'Value (..) function to convert in the other direction.
These functions lack the pre- and post-conditions that the SPARK tools need
for static code analysis. The functions To_String (..) and
To_Int32 (..) in the Aida library have all the pre- and post-conditions needed.

Important! When using the basic types, use the types defined in the Aida.Types package.
Do not use the types starting with Zzz_ defined in the Aida package nor the types
defined in the packages Aida.String, Aida.Int32 and so on. These types are defined in order to
define the types in Aida.Types. The reason these "auxiliary" types have been defined
is to avoid using "limited with" to be able to formally verify the software using
the SPARK tools.

Tip! If one is curious about the subprograms available by using "use all type"
for the type Aida.Types.Int32, take a look at the subprograms defined in
the package Aida.Int32, but as explained above, do not use the type Aida.Int32.T type directly.

| Package                                 | Description                                                            |
|-----------------------------------------|------------------------------------------------------------------------|
| Aida.Bounded_String                     | Bounded string implementation in pure SPARK                            |
| Aida.Character                          | Do not use this package directly, use the type Aida.Types.Character_T  |
| Aida.Containers.Bounded_Hash_Map        | Bounded hash map implementation in pure SPARK                          |
| Aida.Containers.Bounded_Vector          | Bounded vector implementation in pure SPARK                            |
| Aida.Containers.Integer_To_String_Map   | Integer to String map in pure SPARK (no usage of bounded strings)      |
| Aida.Directories                        | Think Ada.Directories but suitable for SPARK analysable code           |
| Aida.Hash32                             | Do not use this package directly, use the type Aida.Types.Hash32_T     |
| Aida.Int32                              | Do not use this package directly, use the type Aida.Types.Int32_T      |
| Aida.Sequential_Stream_IO               | Think Ada.Sequential_IO but suitable for SPARK analysable code         |
| Aida.String                             | Do not use this package directly, use the type Aida.Types.String_T     |
| Aida.Text_IO                            | Think Ada.Text_IO but suitable for SPARK analysable code               |
| Aida.Types                              | Contains the definitions of the basic types.                           |
| Aida.Utf8                               | Subprograms providing UTF8 support implementation in pure SPARK        |
| Aida.Utf8_Code_Point                    | UTF8 code point definition and subprograms implemented in pure SPARK   |

Installation using the GNAT compiler
------------------------------------
To use the source code in this library do the following:

1) Configure the Aida library by:

   cd config/compiler/gnat
   gprbuild -p -P configure.gpr

   Then execute ./configure or configure.exe depending on OS. Instruct the application for
   which OS it should configure the Aida library

   In the Ada standard there is no function that can be called that will tell the application
   in which OS it is being run in. This information must therefore be supplied by the user,
   unles

2) The source code can now be found in the src/ directory.
   To use the source code in your project simply 'with' the file compiler/gnat/aida.gpr.
   An example of this can be found in tests/compiler/gnat/aida_tests.gpr file.

3) To run the tests:

   cd tests/compiler/gnat
   gprbuild -p -P aida_tests.gpr
   cd bin
   ./run_tests or runtests.exe depending on OS.

Description of the directory structure
--------------------------------------
```
 |
 |-- src (after configuration of the Aida library, all the source code files can be found in this directory)
 |
 |-- compiler
 |    |
 |    |-- gnat (contains gnat specific files)
 |         |
 |         |-- aida.gpr (with this file in your project to use the Aida library after configuration)
 |
 |-- config (contains the source code for the application that does the configuration of the Aida library)
 |    |
 |    |-- src (contains source code and main procedure for the configuration application)
 |    |
 |    |-- compiler
 |    |    |
 |    |    |-- gnat (contains gnat specific files)
 |    |         |
 |    |         |-- configure.gpr (use this file to build the configuration application)
 |    |
 |    |-- aida_src (during configuration source code from this directory is copied into the src/ directory)
 |         |
 |         |-- linux (linux specific source code)
 |         |
 |         |-- mac_os_x (mac os x specific source code)
 |         |
 |         |-- windows (windows specific source code)
 |
 |-- tests (contains the source code the tests of the aida library)
      |
      |-- src (contains source code and main procedure for the tests)
      |    |
      |    |-- ahven (contains the source code for the ahven test framework by Tero Koskinen)
      |
      |-- compiler
           |
           |-- gnat (contains gnat specific files)
                |
                |-- aida_tests.gpr (use this file to build the tests)
```

The are several directories called 'compiler', maybe a better name would be 'build_system'?

