------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--                        D Y N A M I C   P O O L S
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

package body Dynamic_Pools is

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Dynamic_Subpool,
      Name => Dynamic_Subpool_Access);

   function Storage_Size
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   function Storage_Used
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   protected body Subpool_Set is

      procedure Add (Subpool : Dynamic_Subpool_Access) is
      begin
         Subpools.Append (Subpool);
      end Add;

      --------------------------------------------------------------

      procedure Delete
        (Subpool : in out Dynamic_Subpool_Access)
      is
         Position : Subpool_Vector.Cursor := Subpools.Find (Subpool);
         use type Subpool_Vector.Cursor;
      begin

         if Position /= Subpool_Vector.No_Element then
            Subpools.Delete (Position);
         end if;

      end Delete;

      --------------------------------------------------------------

      function Storage_Total return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin

         for E in Subpools.Iterate loop
            Result := Result + Storage_Size (Subpools (E));
         end loop;

         return Result;
      end Storage_Total;

      --------------------------------------------------------------

      function Storage_Usage return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin

         for E in Subpools.Iterate loop
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
      use type Ada.Containers.Count_Type;
      Sub : Dynamic_Subpool renames Dynamic_Subpool (Subpool.all);
   begin

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements > Sub.Active'Length - Sub.Next_Allocation
      then

         Sub.Used_List.Append (New_Item => Sub.Active);

         if Sub.Free_List.Length > 0 and then
           Sub.Free_List.First_Element'Length >= Size_In_Storage_Elements
         then
            Sub.Active := Sub.Free_List.First_Element;
            Sub.Free_List.Delete_First;
         else
            Sub.Active := new System.Storage_Elements.Storage_Array
              (1 .. Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Sub.Block_Size));
         end if;

         Sub.Next_Allocation := Sub.Active'First;

      end if;

      Storage_Address := Sub.Active (Sub.Next_Allocation)'Address;
      Sub.Next_Allocation := Sub.Next_Allocation + Size_In_Storage_Elements;
   end Allocate_From_Subpool;

   --------------------------------------------------------------

   procedure Create_Default_Subpool
     (Pool : in out Dynamic_Pool) is
   begin
      Pool.Default_Subpool := Pool.Create_Subpool;
   end Create_Default_Subpool;

   --------------------------------------------------------------

   overriding
   function Create_Subpool
     (Pool : in out Dynamic_Pool) return not null Subpool_Handle is
   begin

      return Create_Subpool
        (Pool,
         (if Pool.Default_Block_Size = 0 then
             Default_Allocation_Block_Size
          else
             Pool.Default_Block_Size));

   end Create_Subpool;

   --------------------------------------------------------------

   not overriding
   function Create_Subpool
     (Pool : in out Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count)
      return not null Subpool_Handle
   is
      New_Pool : constant Dynamic_Subpool_Access
        := new Dynamic_Subpool'
          (Storage_Pools.Subpools.Root_Subpool with
           Block_Size => Block_Size,
           Used_List => <>,
           Free_List => <>,
           Active => new System.Storage_Elements.Storage_Array
             (1 .. Block_Size),
           Next_Allocation => 1,
           Owner => Ada.Task_Identification.Current_Task);

      Result : constant Subpool_Handle := Subpool_Handle (New_Pool);
   begin

      Pool.Subpools.Add (New_Pool);

      Storage_Pools.Subpools.Set_Pool_Of_Subpool
        (Subpool => Result,
         To => Pool);

      return Result;

   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Pool : in out Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count :=
        Default_Allocation_Block_Size) return Scoped_Subpool
   is
      New_Subpool : constant Subpool_Handle :=
        Create_Subpool (Pool, Block_Size);
   begin
      return  Result : Scoped_Subpool (Handle => New_Subpool);
   end Create_Subpool;

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

      The_Subpool.Used_List.Iterate
        (Process => Free_Storage_Element'Access);

      The_Subpool.Used_List.Clear;

      The_Subpool.Free_List.Iterate
        (Process => Free_Storage_Element'Access);

      The_Subpool.Free_List.Clear;
      Free_Storage_Array (The_Subpool.Active);

      --  Handle case when deallocating the default pool
      --  Should only occur if client attempts to obtain the default
      --  subpool, then calls Unchecked_Deallocate_Subpool on that object
      if Pool.Default_Subpool /= null and then Subpool = Pool.Default_Subpool
      then
         Pool.Default_Subpool := null;
      end if;

      Free_Subpool (The_Subpool);

   end Deallocate_Subpool;

   --------------------------------------------------------------

   package body Scoped_Subpools is
      overriding
      procedure Finalize (Subpool : in out Scoped_Subpool) is
         Handle : Subpool_Handle := Subpool.Handle;
      begin
         pragma Warnings (Off, "*Handle*modified*but*never referenced*");
         Unchecked_Deallocate_Subpool (Handle);
         pragma Warnings (On, "*Handle*modified*but*never referenced*");
      end Finalize;
   end Scoped_Subpools;

   --------------------------------------------------------------

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor) is
       Storage : Storage_Array_Access := Storage_Vector.Element (Position);
   begin
      Free_Storage_Array (Storage);
   end Free_Storage_Element;

   --------------------------------------------------------------

   overriding procedure Initialize (Pool : in out Dynamic_Pool) is
   begin
      Pool.Default_Subpool :=
        (if Pool.Default_Block_Size > 0 then Pool.Create_Subpool else null);

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

   function Storage_Size
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;
   begin

      for E in Subpool.Used_List.Iterate loop
         Result := Result + Subpool.Used_List (E).all'Length;
      end loop;

      for E in Subpool.Free_List.Iterate loop
         Result := Result + Subpool.Free_List (E).all'Length;
      end loop;

      return Result + Subpool.Active'Length;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access :=
        Dynamic_Subpool_Access (Subpool);
   begin
      return Storage_Size (The_Subpool);
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;
   begin

      for E in Subpool.Used_List.Iterate loop
         Result := Result + Subpool.Used_List (E).all'Length;
      end loop;

      return Result + Subpool.Next_Allocation - 1;
   end Storage_Used;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access :=
        Dynamic_Subpool_Access (Subpool);
   begin
      return Storage_Used (The_Subpool);
   end Storage_Used;

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
            Alignment       => Allocation_Type'Alignment,
            Subpool         => Subpool);

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
            Alignment       => Allocation_Type'Alignment,
            Subpool         => Subpool.Handle);

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

end Dynamic_Pools;
