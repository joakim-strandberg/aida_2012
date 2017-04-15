About the Aida library
----------------------

The source code in the Aida library conforms to the Ada 2012 standard.
It has successfully been compiled using GNAT GPL 2016 on both Ubuntu 16.04 and Windows 10.

Design goals:

 - Minimize external dependencies. In particular, no dependency on make to configure the library.
   Before being able to use the library it must be configured. The traditional way to do
   this is using make and makefiles. This is cumbersome on Windows where make is
   not part of the OS out of the box. It means one must first install MSYS or something.
   This is not necessary with the Aida library. If you have an Ada 2012 compiler you
   are good to go.
 - Use SPARK whenever possible.

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

