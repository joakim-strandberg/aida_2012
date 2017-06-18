with Aida.Bounded_String;

package Aida.Json_Parsing_Tests_Model with SPARK_Mode, Initializes => (Max_Indices) is

   type Person_Array_Index_T is new Integer range 1..10;

   subtype Extended_Person_Array_Index_T is Person_Array_Index_T'Base range 0..Person_Array_Index_T'Last;

   package Max_Indices_Def is

      type T is private;

      function Person_Id_Max (This : T) return Extended_Person_Array_Index_T;

      procedure Allocate_Person_Id (This      : in out T;
                                    Person_Id : out Person_Array_Index_T) with
        Global => null,
        Pre    => Person_Id_Max (This) < Extended_Person_Array_Index_T'Last,
        Post   => Person_Id_Max (This) = Person_Id_Max (This)'Old + 1;

      procedure Clear (This : in out T) with
        Global => null,
        Post   => Person_Id_Max (This) = 0;

   private

      type T is record
         My_Person_Id_Max : Extended_Person_Array_Index_T := 0;
      end record;

      function Person_Id_Max (This : T) return Extended_Person_Array_Index_T is (This.My_Person_Id_Max);

   end Max_Indices_Def;

   use Max_Indices_Def;

   Max_Indices : Max_Indices_Def.T;

   subtype Person_Id_T is Person_Array_Index_T with
     Dynamic_Predicate => Person_Id_T <= Person_Id_Max (Max_Indices);

   package Person_Def is

      type Age_T is new Integer range 0..130;

      NAME_MAX : constant := 30;

      type Name_T is new Aida.Bounded_String.T (NAME_MAX);

      function Make return Name_T with
        Global => null;

      type T is record
         Age  : Age_T := 0;
         Name : Name_T;
      end record;

   end Person_Def;

   use all type Person_Def.Name_T;

   subtype Person_T is Person_Def.T;

   type People_T is array (Person_Array_Index_T) of Person_T;

end Aida.Json_Parsing_Tests_Model;
