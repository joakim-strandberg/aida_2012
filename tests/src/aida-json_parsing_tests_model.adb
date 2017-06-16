package body Aida.Json_Parsing_Tests_Model with SPARK_Mode is

   package body Max_Indices_Def is

      procedure Allocate_Person_Id (This      : in out T;
                                    Person_Id : out Person_Array_Index_T) is
      begin
         This.My_Person_Id_Max := This.My_Person_Id_Max + 1;
         Person_Id := This.My_Person_Id_Max;
      end Allocate_Person_Id;

      procedure Clear (This : in out T) is
      begin
         This.My_Person_Id_Max := 0;
      end Clear;

   end Max_Indices_Def;

   package body Person_Def is

      function Make return Name_T is
      begin
         return This : Name_T do
            Initialize (This, "");
         end return;
      end Make;

   end Person_Def;

end Aida.Json_Parsing_Tests_Model;
