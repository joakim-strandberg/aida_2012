package Aida.Containers is

   Container_Is_Empty_Exception : exception;
   -- Raised when trying to get an element from an empty container.

   Out_Of_Bounds_Exception : exception;
   -- Raised when trying to get an element that does not exist from a non-empty
   -- container.

   End_Of_Container_Exception : exception;
   -- Raised when trying to add more items to a container that has reached
   -- full capacity.

   Key_Not_Found_Exception : exception;

   Key_Already_Present_Exception : exception;

   type Max_Hash_Map_Size_T is range 3..(2**31);

end Aida.Containers;
