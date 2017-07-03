with Aida.Bounded_String;
with Aida.Containers.Bounded_Vector;

package Aida.Json_Parsing_Tests_Model with SPARK_Mode is

   type Person_Array_Index_T is new Integer range 1..10;

   subtype Extended_Person_Array_Index_T is Person_Array_Index_T'Base range 0..Person_Array_Index_T'Last;

   type Hand_Array_Index_T is new Integer range 1..10;

   subtype Extended_Hand_Array_Index_T is Hand_Array_Index_T'Base range 0..Hand_Array_Index_T'Last;

   type Vehicle_Array_Index_T is new Integer range 1..10;

   subtype Extended_Vehicle_Array_Index_T is Vehicle_Array_Index_T'Base range 0..Vehicle_Array_Index_T'Last;

   package Max_Indices_Def is

      type T is limited private;

      function Person_Id_Max (This : T) return Extended_Person_Array_Index_T;

      function Hand_Id_Max (This : T) return Extended_Hand_Array_Index_T;

      function Vehicle_Id_Max (This : T) return Extended_Vehicle_Array_Index_T;

      procedure Allocate_Person_Id (This      : in out T;
                                    Person_Id : out Person_Array_Index_T) with
        Global => null,
        Pre    => Person_Id_Max (This) < Extended_Person_Array_Index_T'Last,
        Post   => Person_Id_Max (This) = Person_Id_Max (This)'Old + 1;

      procedure Allocate_Hand_Id (This    : in out T;
                                  Hand_Id : out Hand_Array_Index_T) with
        Global => null,
        Pre    => Hand_Id_Max (This) < Extended_Hand_Array_Index_T'Last,
        Post   => Hand_Id_Max (This) = Hand_Id_Max (This)'Old + 1;

      procedure Allocate_Vehicle_Id (This : in out T;
                                     Id   : out Vehicle_Array_Index_T) with
        Global => null,
        Pre    => Vehicle_Id_Max (This) < Extended_Vehicle_Array_Index_T'Last,
        Post   => Vehicle_Id_Max (This) = Vehicle_Id_Max (This)'Old + 1;

      procedure Clear (This : in out T) with
        Global => null,
        Post   => Person_Id_Max (This) = 0 and Hand_Id_Max (This) = 0 and Vehicle_Id_Max (This) = 0;

   private

      type T is limited record
         My_Person_Id_Max  : Extended_Person_Array_Index_T  := 0;
         My_Hand_Id_Max    : Extended_Hand_Array_Index_T    := 0;
         My_Vehicle_Id_Max : Extended_Vehicle_Array_Index_T := 0;
      end record;

      function Person_Id_Max (This : T) return Extended_Person_Array_Index_T is (This.My_Person_Id_Max);

      function Hand_Id_Max (This : T) return Extended_Hand_Array_Index_T is (This.My_Hand_Id_Max);

      function Vehicle_Id_Max (This : T) return Extended_Vehicle_Array_Index_T is (This.My_Vehicle_Id_Max);

   end Max_Indices_Def;

   Max_Indices : Max_Indices_Def.T;

   subtype Person_Id_T is Person_Array_Index_T;

-- This is what we would wish to express:
--     subtype Person_Id_T is Person_Array_Index_T with
--       Dynamic_Predicate => Person_Id_T <= Aida.Json_Parsing_Tests_Model.Max_Indices_Def.Person_Id_Max (Max_Indices);

   subtype Hand_Id_T is Hand_Array_Index_T;

   subtype Vehicle_Id_T is Vehicle_Array_Index_T;

   package Hand_Def is

      type Number_Of_Fingers_T is new Integer range 0..5;

      type T is record
         Number_Of_Fingers : Number_Of_Fingers_T := 5;
      end record;

   end Hand_Def;

   package Vehicle_Def with SPARK_Mode is

      type Wheels_T is new Integer;

      type T is limited record
         Wheels : Wheels_T := 0;
      end record;

   end Vehicle_Def;

   package Person_Def is

      type Age_T is new Integer range 0..130;

      NAME_MAX : constant := 30;

      type Name_T is new Aida.Bounded_String.T (NAME_MAX);

      function Make return Name_T with
        Global => null;

      function Default_Hand_Id return Hand_Id_T is (5);

      type Hand_Vector_Index_T is new Positive range 1..2;

      package Hand_Vector is new Aida.Containers.Bounded_Vector (Index_T         => Hand_Vector_Index_T,
                                                                 Element_T       => Hand_Id_T,
                                                                 "="             => "=",
                                                                 Default_Element => Default_Hand_Id);

      type Vehicle_Vector_Index_T is new Positive range 1..5;

      function Default_Vehicle_Id return Vehicle_Id_T is (1);

      package Vehicle_Vector is new Aida.Containers.Bounded_Vector (Index_T         => Vehicle_Vector_Index_T,
                                                                    Element_T       => Vehicle_Id_T,
                                                                    "="             => "=",
                                                                    Default_Element => Default_Vehicle_Id);

      type T is record
         Age      : Age_T := 0;
         Name     : Name_T;
         Hands    : Hand_Vector.T;
         Vehicles : Vehicle_Vector.T;
      end record;

   end Person_Def;

   use all type Person_Def.Name_T;

   subtype Hand_T is Hand_Def.T;

   subtype Vehicle_T is Vehicle_Def.T;

   subtype Person_T is Person_Def.T;

   type People_T is array (Person_Array_Index_T) of Person_T;

   type Hands_T is array (Hand_Array_Index_T) of Hand_T;

   type Vehicles_T is array (Vehicle_Array_Index_T) of Vehicle_T;

end Aida.Json_Parsing_Tests_Model;
