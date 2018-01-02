------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--           B A S I C   B O U N D E D   D Y N A M I C   P O O L S
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

package body Basic_Bounded_Dynamic_Pools is

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   --------------------------------------------------------------

   overriding
   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);
   begin

      if Pool.Heap_Allocated then

         if Size_In_Storage_Elements >
           Pool.Active_Access'Length - Pool.Next_Allocation
         then

            raise Storage_Error;
         end if;

         Storage_Address := Pool.Active_Access (Pool.Next_Allocation)'Address;

      else
         if Size_In_Storage_Elements >
           Pool.Active'Length - Pool.Next_Allocation
         then

            raise Storage_Error;
         end if;

         Storage_Address := Pool.Active (Pool.Next_Allocation)'Address;
      end if;

      Pool.Next_Allocation := Pool.Next_Allocation + Size_In_Storage_Elements;

   end Allocate;

   --------------------------------------------------------------

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
   begin
      if Pool.Heap_Allocated then
         Free_Storage_Array (Pool.Active_Access);
      end if;
   end Finalize;

   --------------------------------------------------------------

   overriding
   procedure Initialize (Pool : in out Basic_Dynamic_Pool) is
   begin
      if Pool.Heap_Allocated then
         Pool.Active_Access := new System.Storage_Elements.Storage_Array
           (1 .. Pool.Size);
      end if;

      Pool.Next_Allocation := 1;
      Pool.Owner := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      Pool.Owner := T;
   end Set_Owner;

   --------------------------------------------------------------

   overriding
   function Storage_Size
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count is
   begin
      return Pool.Size;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count is
   begin
      return Pool.Next_Allocation - 1;
   end Storage_Used;

end Basic_Bounded_Dynamic_Pools;
