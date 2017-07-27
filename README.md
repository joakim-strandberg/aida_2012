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
with Aida.Text_IO;

procedure Main is
   use all type Aida.Int32_T;
   use all type Aida.String_T;

   User_Input : constant Aida.String_T := "123";

   I : constant Aida.Int32_T := To_Int32 (User_Input);

   Result : constant Aida.String_T := To_String (I);
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
with Aida.Text_IO;

procedure Main with SPARK_Mode is
   use all type Aida.Int32_T;
   use all type Aida.String_T;
   use all type Aida.Character_T;

   function Get_User_Input return Aida.String_T with
     Post => Get_User_Input'Result'Length = 3 and (for all I in Get_User_Input'Result'Range =>
                                                     Is_Digit (Aida.Character_T (Get_User_Input'Result (I))));
   
   function Get_User_Input return Aida.String_T is (1 => '1',
                                                    2 => '2',
                                                    3 => '3');
   
   User_Input : constant Aida.String_T := Get_User_Input;
   
   I : constant Aida.Int32_T := To_Int32 (User_Input);

   Result : constant Aida.String_T := To_String (I);
begin
   Aida.Text_IO.Put_Line ("User entered: ", Result);
end Main;
```
The adjustments to the code is to allow the SPARK tools to verify that the
preconditions of the To_Int32 (..) function are satisfied i.e.
it is not enough to know that the User_Input variable is of type Aida.String_T, the SPARK tools
need to verify that all characters are indeed integers and the length of User_Input is within acceptable range.

The traditional way of converting an integer to a string is to use the Integer'Image (..)
function and to use the Integer'Value (..) function to convert in the other direction.
These functions lack the pre- and post-conditions that the SPARK tools need
for static code analysis. The functions To_String (..) and
To_Int32 (..) in the Aida library have all the pre- and post-conditions needed.

Important! When using the basic types, use the types defined in the Aida package.
Do not use the types starting with Zzz_ defined in the Aida_Z package nor its child packages.
These types in Aida_Z are defined in order to define the types in the Aida package.
The reason these "auxiliary" types have been defined
is to avoid using "limited with" to be able to formally verify the software using
the SPARK tools.

Tip! If one is curious about the subprograms available by using "use all type"
for the type Aida.Int32_T, take a look at the subprograms defined in
the package Aida_Z.Int32, but as explained above, do not use the type Aida_Z.Int32.T type directly.

| Package                                 | Description                                                            |
|-----------------------------------------|------------------------------------------------------------------------|
| Aida                                    | Contains the definitions of the basic types.                           |
| Aida.Bounded_String                     | Bounded string implementation in pure SPARK                            |
| Aida.Containers.Bounded_Hash_Map        | Bounded hash map implementation in pure SPARK                          |
| Aida.Containers.Bounded_Vector          | Bounded vector implementation in pure SPARK                            |
| Aida.Containers.Integer_To_String_Map   | Integer to String map in pure SPARK (no usage of bounded strings)      |
| Aida.Directories                        | Think Ada.Directories but suitable for SPARK analysable code           |
| Aida.Sequential_Stream_IO               | Think Ada.Sequential_IO but suitable for SPARK analysable code         |
| Aida.Text_IO                            | Think Ada.Text_IO but suitable for SPARK analysable code               |
| Aida.Utf8                               | Subprograms providing UTF8 support implementation in pure SPARK        |
| Aida.Utf8_Code_Point                    | UTF8 code point definition and subprograms implemented in pure SPARK   |
| Aida_Z                                  | Do not use this package directly, nor any of its child packages        |
| Aida_Z.Character                        | Do not use this package directly, use the type Aida.Character_T        |
| Aida_Z.String                           | Do not use this package directly, use the type Aida.String_T           |
| Aida_Z.Int32                            | Do not use this package directly, use the type Aida.Int32_T            |
| Aida_Z.Hash32                           | Do not use this package directly, use the type Aida.Hash32_T           |
| Aida_Z.Float                            | Do not use this package directly, use the type Aida.Float_T            |

Installation using the GNAT compiler
------------------------------------
The source code can be compiled by any Ada 2012 compiler, but gpr-files are provided to easily build the source code using the GNAT compiler.
Since some of the source code are OS specific the user must instruct gprbuild on which OS the source code is built for:

Build on Linux:    gprbuild -XOS=Linux   -p -P aida.gpr
Build on Mac OS X: gprbuild -XOS=Mac     -p -P aida.gpr
Build on Windows:  gprbuild -XOS=Windows -p -P aida.gpr

To run the tests:

   gprbuild -XOS=... -p -P run_aida_tests.gpr
   cd bin
   ./run_aida_tests or run_aida_tests.exe depending on OS.

Description of the directory structure
--------------------------------------
```
 |
 |-- src (all the source code files can be found in this directory)
 |    |
 |    |-- linux (linux specific source code)
 |    |
 |    |-- mac_os_x (mac os x specific source code)
 |    |
 |    |-- windows (windows specific source code)
 |
 |-- aida.gpr (with this file in your project to use the Aida library)
 |
 |-- aida_basic_types.gpr (with this file in your project to only use the basic types Aida.Int32_T, Aida.String_T, Aida.Float_T,...)
 |
 |-- ahven.gpr (with this file in your project to use the ahven test framework by Tero Koskinen)
 |
 |-- run_aida_tests.gpr (use this file to build the tests)
```