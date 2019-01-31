------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--                 B O U N D E D   D Y N A M I C   P O O L S
--
--                                B o d y
--
--                  Copyright (C) 2011, Bradley J. Moore
--
--  Deepend is free software;  you can  redistribute it  and/or modify it
--  under  terms of the  GNU General Public License  as  published  by the
--  Free Software  Foundation;  either version 2,  or (at your option) any
--  later  version.  Paraffin is  distributed in the hope that it  will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
--  General Public License for  more details.  You should have  received a
--  copy of the GNU General Public License distributed with Deepend;  see
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
--
--  As a  special exception, if other files  instantiate generics from
--  this unit,  or you link this  unit with other files  to produce an
--  executable,  this unit  does  not by  itself  cause the  resulting
--  executable to be covered by  the GNU General Public License.  This
--  exception does  not however invalidate  any other reasons  why the
--  executable file might be covered by the GNU Public License.
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body Bounded_Dynamic_Pools is
   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Dynamic_Subpool,
      Name => Dynamic_Subpool_Access);

   function Storage_Size
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count is
     (Subpool.Size);

   function Storage_Used
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count is
     (Subpool.Next_Allocation - 1);

   protected body Subpool_Set is

      function Active_Subpools return Containers.Count_Type is
        (Count);

      --------------------------------------------------------------

      procedure Add (Subpool : Dynamic_Subpool_Access) is
      begin
         Count := Count + 1;
         Subpools (Count) := Subpool;
      end Add;

      --------------------------------------------------------------

      procedure Delete (Subpool : Dynamic_Subpool_Access)
      is
         Position : Containers.Count_Type := 0;
      begin
         for I in Subpools'Range loop
            if Subpools (I) = Subpool then
               Position := I;
               exit;
            end if;
         end loop;

         if Position /= 0 then
            Subpools (Position .. Count - 1) :=
              Subpools (Position + 1 .. Count);
            Count := Count - 1;
         end if;
      end Delete;

      --------------------------------------------------------------

      function Storage_Total return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin

         for E in 1 .. Count loop
            Result := Result + Storage_Size (Subpools (E));
         end loop;

         return Result;
      end Storage_Total;

      --------------------------------------------------------------

      function Storage_Usage return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin

         for E in 1 .. Count loop
            Result := Result + Storage_Used (Subpools (E));
         end loop;

         return Result;
      end Storage_Usage;

   end Subpool_Set;

   --------------------------------------------------------------

   overriding
   procedure Allocate
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Pool.Allocate_From_Subpool
        (Storage_Address,
         Size_In_Storage_Elements,
         Alignment,
         Pool.Default_Subpool_For_Pool);
   end Allocate;

   --------------------------------------------------------------

   overriding
   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle)
   is
      pragma Unreferenced (Alignment, Pool);
      Sub : Dynamic_Subpool renames Dynamic_Subpool (Subpool.all);
   begin

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements > Sub.Active'Length - Sub.Next_Allocation
      then
         raise Storage_Error;
      end if;

      Storage_Address := Sub.Active (Sub.Next_Allocation)'Address;
      Sub.Next_Allocation := Sub.Next_Allocation + Size_In_Storage_Elements;
   end Allocate_From_Subpool;

   --------------------------------------------------------------

   procedure Create_Default_Subpool
     (Pool : in out Dynamic_Pool) is
   begin
      Pool.Default_Subpool := Dynamic_Subpool (Pool.Create_Subpool.all)'Access;
   end Create_Default_Subpool;

   --------------------------------------------------------------

   overriding
   function Create_Subpool
     (Pool : in out Dynamic_Pool) return not null Subpool_Handle is
   begin

      return Create_Subpool
        (Pool,
         (if Pool.Default_Subpool_Size = 0 then
             Default_Subpool_Default_Size
          else
             Pool.Default_Subpool_Size));

   end Create_Subpool;

   --------------------------------------------------------------

   not overriding
   function Create_Subpool
     (Pool : in out Dynamic_Pool;
      Size : Storage_Elements.Storage_Count)
      return not null Subpool_Handle
   is
      New_Pool : constant Dynamic_Subpool_Access
        := new Dynamic_Subpool'
          (Storage_Pools.Subpools.Root_Subpool with
           Size => Size,
           Reusable => False,
           Active => <>,
           Next_Allocation => 1,
           Owner => Ada.Task_Identification.Current_Task,
           Reclaimed => False);

      Result : constant Subpool_Handle := Subpool_Handle (New_Pool);
   begin

      Pool.Subpools.Add (New_Pool);

      Storage_Pools.Subpools.Set_Pool_Of_Subpool
        (Subpool => Result,
         To => Pool);

      return Result;

   end Create_Subpool;

   --------------------------------------------------------------

   package body Scoped_Subpools is

      function Create_Subpool
        (Pool : in out Dynamic_Pool;
         Size : Storage_Elements.Storage_Count;
         Heap_Allocated : Boolean := True) return Scoped_Subpool is
      begin
         if Heap_Allocated then
            return Result : constant Scoped_Subpool
              (Size => Size,
               Heap_Allocated => True) :=
              (Finalization.Limited_Controlled with Size => Size,
               Heap_Allocated => True,
               Subpool => Create_Subpool (Pool, Size)
              )
            do
               null;
            end return;

         else
            return Result : Scoped_Subpool
              (Size => Size, Heap_Allocated => False) :=
              (Finalization.Limited_Controlled with Size => Size,
               Heap_Allocated => False,
               Subpool => <>,
               Storage =>
                 (Storage_Pools.Subpools.Root_Subpool with
                  Size => Size,
                  Reusable => True,
                  Active => <>,
                  Next_Allocation => 1,
                  Owner => Ada.Task_Identification.Current_Task,
                  Reclaimed => False))
            do

               Result.Subpool := Result.Storage'Unchecked_Access;

               Pool.Subpools.Add (Result.Storage'Unchecked_Access);

               Storage_Pools.Subpools.Set_Pool_Of_Subpool
                 (Subpool => Result.Subpool,
                  To => Pool);

            end return;
         end if;

      end Create_Subpool;

   end Scoped_Subpools;

   --------------------------------------------------------------

   overriding
   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle)
   is
      The_Subpool : Dynamic_Subpool_Access := Dynamic_Subpool_Access (Subpool);
   begin

      --  Only removes the access value from the Subpools container
      --  Does not actually delete the object which we still have a
      --  reference to above
      Pool.Subpools.Delete (The_Subpool);

      --  Handle case when deallocating the default pool
      --  Should only occur if client attempts to obtain the default
      --  subpool, then calls Unchecked_Deallocate_Subpool on that object
      if Pool.Default_Subpool /= null and then
        The_Subpool = Pool.Default_Subpool
      then
         Pool.Default_Subpool := null;
      end if;

      The_Subpool.Next_Allocation := 1;

      if The_Subpool.Reusable then
         The_Subpool.Reclaimed := True;
      else
         Free_Subpool (The_Subpool);
      end if;

   end Deallocate_Subpool;

   --------------------------------------------------------------

   overriding
   procedure Finalize (Subpool : in out Scoped_Subpool) is
   begin
      --  pragma Warnings (Off, "*Subpool*modified*but*never referenced*");
      Unchecked_Deallocate_Subpool (Subpool.Subpool);
      --  pragma Warnings (On, "*Subpool*modified*but*never referenced*");
   end Finalize;

   --------------------------------------------------------------

   overriding procedure Initialize (Pool : in out Dynamic_Pool) is
   begin
      Pool.Default_Subpool :=
        (if Pool.Default_Subpool_Size > 0
         then Dynamic_Subpool (Pool.Create_Subpool.all)'Access else null);

      Pool.Owner := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      Pool.Owner := T;
   end Set_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) is
   begin
      Dynamic_Subpool (Subpool.all).Owner := T;
   end Set_Owner;

   --------------------------------------------------------------

   package body Subpool_Allocators is

      package Subpool_Handle_Conversions is new
        Address_To_Access_Conversions (Object => Allocation_Type);

      function Allocate
        (Subpool : Subpool_Handle;
         Value : Allocation_Type := Default_Value)
      return Allocation_Type_Access
      is
         Location : System.Address;
      begin

         Storage_Pools.Subpools.Pool_Of_Subpool (Subpool).Allocate_From_Subpool
           (Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment => Allocation_Type'Alignment,
            Subpool => Subpool);

         declare
            Result : constant Allocation_Type_Access :=
              Allocation_Type_Access
                (Subpool_Handle_Conversions.To_Pointer (Location));
         begin
            Result.all := Value;
            return Result;
         end;

      end Allocate;

      --------------------------------------------------------------

      function Allocate
        (Subpool : Scoped_Subpool;
         Value   : Allocation_Type := Default_Value)
      return Allocation_Type_Access
      is
         Location : System.Address;
      begin

         Storage_Pools.
           Subpools.Pool_Of_Subpool (Subpool.Handle).Allocate_From_Subpool
           (Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment => Allocation_Type'Alignment,
            Subpool => Subpool.Handle);

         declare
            Result : constant Allocation_Type_Access :=
              Allocation_Type_Access
                (Subpool_Handle_Conversions.To_Pointer (Location));
         begin
            Result.all := Value;
            return Result;
         end;

      end Allocate;

   end Subpool_Allocators;

end Bounded_Dynamic_Pools;
