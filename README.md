About the Aida library
----------------------

If you are considering making an Ada or SPARK application then the Aida library may
be of interest to you. Developing formally verified software is very time consuming and thus very expensive.
The goal of the Aida library is to lower the cost of mission-critical and security-critical
applications by providing common functionality implemented in formally verified software.

The Aida library consists of (mostly) formally verified SPARK and conforms to the Ada 2012 standard.
It has successfully been compiled using GNAT Community Edition 2016 and 2017 on both Ubuntu 16.04 and Windows 10.

Design goals:

 - Minimize external dependencies. If you have an Ada 2012 compiler you are good to go.
 - Use SPARK whenever possible.

The Aida library provides for example conversion routines for basic types.
Consider:
```
with Aida.Text_IO;

procedure Main is
   User_Input : constant String := "123";

   I : constant Aida.Int32_T := Aida.String.To_Int32 (User_Input);

   Result : constant String := Aida.Int32.To_String (I);
begin
   Aida.Text_IO.Put_Line ("User entered: ", Result);
end Main;
```
The output of the application is:
```
User entered: 123
```
If one would like to formally verify the above code using the SPARK tools (SPARK Discovery 2017)
one would need to add the SPARK_Mode aspect to the Main procedure:
```
with Aida.Text_IO;

procedure Main with SPARK_Mode is
   User_Input : constant String := "123";

   I : constant Aida.Int32_T := Aida.String.To_Int32 (User_Input);

   Result : constant String := Aida.Int32.To_String (I);
begin
   Aida.Text_IO.Put_Line ("User entered: ", Result);
end Main;
```
The traditional way of converting an integer to a string is to use the Integer'Image (..)
function and to use the Integer'Value (..) function to convert in the other direction.
These functions lack the pre- and post-conditions that the SPARK tools need
for static code analysis. The functions Aida.Int32.To_String (..) and
Aida.String.To_Int32 (..) in the Aida library have all the pre- and post-conditions needed.

Pure SPARK packages:

| Package                                 | Description                                                            |
|-----------------------------------------|------------------------------------------------------------------------|
| Aida                                    | Contains conversion subprograms for Standard value types.              |
| Aida.Bounded_String                     | Bounded string implementation                                          |
| Aida.Containers.Bounded_Hash_Map        | Bounded hash map implementation                                        |
| Aida.Containers.Bounded_Vector          | Bounded vector implementation                                          |
| Aida.Containers.Tagged_Bounded_Vector   | Bounded vector implementation                                          |
| Aida.Containers.Integer_To_String_Map   | Integer to String map (no usage of Aida.Bounded_String)                |
| Aida.Utf8                               | Subprograms providing UTF8 support implementation                      |
| Aida.Utf8_Code_Point                    | UTF8 code point definition and subprograms                             |
| Aida.XML_SAX_Parse                      | Suitable for XML SAX parsing                                           |
| Aida.XML_DOM_Parser                     | XML DOM parser in pure SPARK                                           |
| Aida.JSON_SAX_Parse                     | Suitable for JSON SAX parsing                                          |
| Aida.JSON_DOM_Parser                    | JSON DOM parser                                                        |

Wrapper packages for use in SPARK analysable code:

| Package                                 | Description                                                            |
|-----------------------------------------|------------------------------------------------------------------------|
| Aida.Directories                        | Think Ada.Directories but suitable for SPARK analysis                  |
| Aida.Sequential_Stream_IO               | Think Ada.Sequential_IO but suitable for SPARK analysis                |
| Aida.Text_IO                            | Think Ada.Text_IO but suitable for SPARK analysis                      |

Full Ada 2012 packages:

| Package                                 | Description                                                              |
|-----------------------------------------|--------------------------------------------------------------------------|
| Aida.Deepend_XML_SAX_Parse              | Suitable for XML SAX parsing. Uses Brad Moore's Deepend for convenience. |
| Aida.Deepend_XML_DOM_Parser             | XML DOM parser. Uses Brad Moore's Deepend for convenience.               |

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
 |-- aida.gpr (with this file in your project to use the Aida library for developing SPARK applications)
 |
 |-- deepend.gpr (contains Brad Moore's Deepend)
 |
 |-- aida_deepend.gpr (contains the parts of the Aida library that depends upon Brad Moore's Deepend i.e. XML DOM Parser)
 |
 |-- ahven.gpr (with this file in your project to use the ahven test framework by Tero Koskinen)
 |
 |-- run_aida_tests.gpr (use this file to build the tests)
```
