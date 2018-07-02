with Ada.Exceptions;

package body Aida.Bounded_Hash_Map is

   use all type Collision_Vector.T;

   function Default_Node return Node_T is (Key => Default_Key, Element => Default_Element);

   -- It is possible for a more detailed analysis by the SPARK tools
   -- if the usage of Aida.Bounded_Vector was dropped and
   -- a custom implementation would be used. It would then be possible
   -- to express that the length of the collision list would increase if
   -- the key would not already be present in the hash map. This is on the TODO list.
   procedure Insert (This        : in out T;
                     Key         : Key_T;
                     New_Element : Element_T)
   is
      BI : constant Bucket_Index_T := Normalize_Index (Hash (Key));

      N : Node_T := (Key     => Key,
                     Element => New_Element);
   begin
      if This.Buckets (BI).Exists then
         if This.Buckets (BI).Value.Key = Key then
            This.Buckets (BI) := (Exists => True,
                                  Value  => N);
         else
            declare
               Is_Found : Boolean := False;
               I : Collision_Vector.Index_T := First_Index (This.Collision_List);
            begin
               while not Is_Found and I <= Last_Index (This.Collision_List) loop
                  pragma Loop_Variant (Increases => I);
                  pragma Loop_Invariant (I <= Last_Index (This.Collision_List));
                  if Element (This.Collision_List, I).Key = Key then
                     Is_Found := True;
                  else
                     I := I + 1;
                  end if;
               end loop;

               if Is_Found then
                  Replace_Element (This.Collision_List, I, N);
               else
                  Append (This.Collision_List, N);
               end if;
            end;
         end if;
      else
         This.Buckets (BI) := (Exists => True,
                               Value  => N);
      end if;
   end Insert;

   procedure Delete (This : in out T;
                     Key  : Key_T)
   is
      BI : constant Bucket_Index_T := Normalize_Index (Hash (Key));
   begin
      if
        This.Buckets (BI).Value.Key = Key
      then
         if Is_Empty (This.Collision_List) then
            This.Buckets (BI) := (Exists => False);
         else
            This.Buckets (BI) := (Exists => True,
                                  Value  => Last_Element (This.Collision_List));
            Delete_Last (This.Collision_List);
         end if;
      else
         declare
            I : Collision_Vector.Extended_Index_T
              := Collision_Vector.Extended_Index_T'First;
         begin
--              pragma Assert (for some I in Collision_Index_T range Collision_Vector.First_Index (This.Collision_List)..Collision_Vector.Last_Index (This.Collision_List) =>
--                               Collision_Vector.Element (This.Collision_List, I).Key = Key);
            while I < Last_Index (This.Collision_List) loop
               I := I + 1;

               if Element (This.Collision_List, I).Key = Key then
                  exit;
               end if;

               pragma Loop_Variant (Increases => I);
               pragma Loop_Invariant (I <= Last_Index (This.Collision_List));
               pragma Loop_Invariant (for all J in Collision_Vector.Index_T range Collision_Vector.Index_T'First..I => Element (This.Collision_List, J).Key /= Key);
            end loop;

            Collision_Vector.Replace_Element
              (This.Collision_List, I, Last_Element (This.Collision_List));

            Delete_Last (This.Collision_List);
         end;
      end if;
   end Delete;

   function Element (This : T;
                     Key  : Key_T) return Element_T
   is
      BI : constant Bucket_Index_T := Normalize_Index (Hash (Key));

      Result : Element_T;
   begin
      if This.Buckets (BI).Value.Key = Key then
         Result := This.Buckets (BI).Value.Element;
      else
         declare
            I : Collision_Vector.Extended_Index_T := Collision_Vector.Extended_Index_T'First;
         begin
--              pragma Assert (for some I in Collision_Index_T range Collision_Vector.First_Index (This.Collision_List)..Collision_Vector.Last_Index (This.Collision_List) =>
--                               Collision_Vector.Element (This.Collision_List, I).Key = Key);
            while I < Last_Index (This.Collision_List) loop
               I := I + 1;

               if Element (This.Collision_List, I).Key = Key then
                  exit;
               end if;

               pragma Loop_Variant (Increases => I);
               pragma Loop_Invariant (I <= Last_Index (This.Collision_List));
               pragma Loop_Invariant (for all J in Collision_Vector.Index_T range Collision_Vector.Index_T'First..I => Element (This.Collision_List, J).Key /= Key);
            end loop;

            Result := Collision_Vector.Element (This.Collision_List, I).Element;
         end;
      end if;

      return Result;
   end Element;

   function Find_Element (This : T;
                          Key  : Key_T) return Find_Element_Result_T
   is
      BI : constant Bucket_Index_T := Normalize_Index (Hash (Key));

      I : Collision_Vector.Extended_Index_T := Collision_Vector.Extended_Index_T'First;
      Is_Found : Boolean := False;
   begin
      if This.Buckets (BI).Exists then
         if This.Buckets (BI).Value.Key = Key then
            return (Exists  => True,
                    Element => This.Buckets (BI).Value.Element);
         else
            while I < Last_Index (This.Collision_List) loop
               I := I + 1;

               if Element (This.Collision_List, I).Key = Key then
                  Is_Found := True;
                  exit;
               end if;

               pragma Loop_Variant (Increases => I);
               pragma Loop_Invariant (I <= Last_Index (This.Collision_List));
               pragma Loop_Invariant (for all J in Collision_Vector.Index_T range Collision_Vector.Index_T'First..I => Element (This.Collision_List, J).Key /= Key);
            end loop;
         end if;
      end if;

      declare
         FR : constant Find_Element_Result_T := (if Is_Found then
                                                   (Exists  => True,
                                                    Element => Collision_Vector.Element (This.Collision_List, I).Element)
                                                 else
                                                   (Exists => False));
      begin
         return FR;
      end;
   end Find_Element;

end Aida.Bounded_Hash_Map;
