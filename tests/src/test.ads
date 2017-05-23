with Aida.Containers.Bounded_Hash_Map;
with Aida.Types;

package Test with SPARK_Mode is

   function Default_Key     return Aida.Types.Int32_T is (0);
   function Default_Element return Aida.Types.Int32_T is (0);

   package Int_To_Int_Hash_Map is new Aida.Containers.Bounded_Hash_Map (Key_T                   => Aida.Types.Int32_T,
                                                                        Element_T               => Aida.Types.Int32_T,
                                                                        Default_Key             => Default_Key,
                                                                        Default_Element         => Default_Element,
                                                                        Hash                    => Aida.Types.Hash32,
                                                                        Equivalent_Keys         => Aida.Types."=",
                                                                        Max_Hash_Map_Size       => 10,
                                                                        Max_Collision_List_Size => 10);

   HM : Int_To_Int_Hash_Map.T;

end Test;
