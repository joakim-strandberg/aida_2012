------------------------------------------------------------------------------
--
--     Deepend - Dynamic Storage Pools for Ada 95, Ada 2005 and Ada 2012
--
--            B A S I C   B O U N D E D   D Y N A M I C   P O O L S
--
--                                S p e c
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

--  Deepend is a suite of dynamic storage pools with subpool capabilities for
--  Ada 95, Ada 2005, and Ada 2012. The Deepend storage pools were designed to
--  provide efficient concurrency for use in both single core and multicore
--  environments. The Deepend pools also provide flexibility such that
--  the storage for the pools may be placed entirely in static memory,
--  on the stack, or on the heap, or in various combinations of the above
--  (See the Bounded Pools below for more details on how this can be applied).
--
--  Further, Deepend ensures that each pool and subpool is "owned" by a
--  single Ada task, and thus only that task may allocate objects from a
--  particular subpool, and only one task may allocate objects directly to the
--  pool. Ownership of a pool and subpools may be relinquished and transferred
--  to other tasks. This capability eliminates the need for expensive locking
--  when allocating and deallocating objects. Attempts by other tasks to
--  allocate from a pool or subpool owned by another task results in a
--  Program_Error exception being raised.
--
--  Storage pools with subpool capabilities allow all objects in a subpool
--  to be reclaimed all at once, instead of requiring each object to be
--  individually reclaimed one at a time. A Dynamic Pool may have any number of
--  subpools. If subpools are not reclaimed prior to finalization of the pool,
--  then they are reclaimed when the pool is finalized.
--
--  With this Storage pool, Unchecked_Deallocation is implemented as a No-Op
--  (null procedure), because it is not needed or intended to be used.
--  If early finalization is needed, Unchecked_Deallocate_Subpool may be
--  used, which has similar issues as Unchecked_Deallocation, but is
--  safer, since it can be applied more globally, and less frequently. Even
--  Unchecked_Deallocate_Subpool is unnecessary for reclaiming subpools in
--  nested scopes with Deepend, as a scoped subpool facility is also provided,
--  which automatically finalizes subpools, when leaving the scope of their
--  declaration.
--
--  Subpool based storage management provides a safer means of memory
--  management, which can outperform other mechanisms for storage
--  reclamation including garbage collection.
--
--  There are 4 Storage Pool packages to choose from in Deepend.
--
--     1) Dynamic_Pools
--     2) Bounded_Dynamic_Pools
--     3) Basic_Dynamic_Pools
--     4) Basic_Bounded_Dynamic_Pools
--
--  The Dynamic_Pools package has subpool capabilities where the storage
--  in each Subpool object is unbounded. If the current block of memory
--  is fully allocated to objects and further objects are allocated,
--  then another block of storage is allocated to the subpool, and further
--  allocations to that subpool are carved out of that new storage block.
--
--  The Bounded_Dynamic_Pools package has subpool capabilities where the
--  storage in each Subpool object is bounded, and the number of subpools
--  that may be allocated is also bounded. If the Subpool is fully
--  allocated with objects and an attempt is made to allocate further objects
--  from the same subpool, then a Storage_Error exception is raised. Similarly,
--  if an attempt is made to allocate more subpools than the maximum number
--  the pool was configured to manage, then a Storage_Error exception is
--  raised. A Bounded_Dynamic_Pools pool does not utilize the heap for its
--  management of subpools. Scoped_Subpool objects are provided that may be
--  configured to allocate their storage from the heap, or declared on the
--  stack, or statically at library level. In particular, Scoped_Subpool
--  objects are included that have discriminants that provide this control.
--  A scoped subpool automatically is finalized when execution leaves the scope
--  of the declaration. For a scoped subpool declared at library level, the
--  the storage remains available while the partition is active.
--
--  The Basic_Dynamic_Pool package does not have subpool capabilities, and
--  each allocation is managed instead by the pool object. When the pool is
--  finalized, all objects allocated from the pool that need finalization are
--  also finalized. A Basic_Dynamic_Pool is an unbounded pool such that if the
--  current block of storage is fully allocated with objects and further
--  objects are allocated, then another block of memory is allocated to the
--  pool, and further object allocations are carved out of that new block.
--
--  The Basic_Bounded_Dynamic_Pool package does not have subpool capabilities,
--  and each allocation is managed instead by the pool object. Like the
--  Basic_Dynamic_Pool, when the pool is finalized, all objects allocated from
--  the pool are also finalized. A Basic_Dynamic_Pool is a bounded pool such
--  that if the pool's storage has been fully allocated with objects and an
--  attempt is made to allocate further objects, then a Storage_Error exception
--  is raised. A Basic_Bounded_Dynamic_Pool pool has discriminants that
--  indicate whether the storage for the pool resides on the heap or
--  on the stack, or statically at library level.
--
--  Both Basic_Dynamic_Pools and Bounded_Basic_Dynamic_Pools are completely
--  forward compatible with the Ada 2012 standard for Storage_Pools, since they
--  only allow allocations via the existing "new" operator (without subpool
--  specifications). This facility relies on access type finalization to
--  finalize all the objects from a pool, and does not otherwise support
--  subpools.
--
--  In Ada 2012, the new allocation syntax may be used with this pool, to
--  specify the subpool that will contain the allocated objects.
--
--     e.g.
--          Object := new (subpool_name) Object_Type'(Value);
--
--  For Ada 95 and Ada 2005, a similar effect can be obtained by using the
--  Allocation and Initialized_Allocation generics provided by this package.
--  However, these generics only allow allocating non-controlled objects of
--  definite types to a particular subpool, whereas in Ada 2012, indefinite
--  types and controlled types, and other types needing finalization such as
--  protected types may also be allocated to a subpool. Only task types or
--  types that have tasks cannot be allocated to a subpool.
--
--  In addition, for Ada 95, Ada 2005, and Ada 2012, the "new" keyword may be
--  used without specifying a subpool, which results in an object being
--  allocated to the default subpool for the storage pool.
--
--  A Dynamic_Pool allows objects allocated from a subpool to be reclaimed
--  all at once, instead of requiring each object to be individually
--  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
--  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
--  with this storage pool (and has no effect).
--
--  Tasks can create subpools from the same Dynamic Pool object at the
--  same time, but only one task may allocate objects from a specific subpool
--  instance at a time. A task "owns" its subpools, and attempts by other
--  tasks to allocate from subpool owned by another task results in
--  a Program_Error exception being raised.
--
--  There are separate versions of these packages for Ada95, Ada 2005, and
--  Ada 2012. The three versions were designed to have mostly compatible
--  interfaces, but there are slight differences in each newer language
--  version that takes advantages of newer language features.
--  In particular,
--    - for Ada 2005, the Scoped_Subpool type has a Create_Subpool constructor,
--      which allows the Block_Size to have a defaulted value.
--    - for Ada 2012, Pool parameters are in out parameters, rather than
--      access parameters, which eliminates the need to declare the pool
--      object as an aliased object.
--
--
--  Allocation strategy:
--
--    Deallocate is not needed or used, and is implemented as a null
--    procedure. Use of this storage pool means that there is no need for
--    calls to Ada.Unchecked_Deallocation (as it has no effect).
--
--    The strategy is to provide an efficient storage pool that allocates
--    objects quickly with minimal overhead, and very fast dealloction.
--    Tasks "own" its subpool objects, which allows allocation from the
--    subpools to be more efficient, since there is no need for task
--    synchronization.
--
--    The intent is that the subpool strategy should generally outperform
--    other strategies such as garbage collection, or individual object
--    reclamation in a more deterministic fashion.
--
--  ** NOTE: In the Ada 95 and Ada 2005 version of Dynamic_Pools, it is
--    erroneous to allocate objects to a subpool that need finalization eg.
--    (Tasks, protected types, or objects of types inherited from types defined
--    in Ada.Finalization) and then deallocate the subpool associated with
--    those objects before they would have otherwise been finalized.
--
--  For Ada 2012, it is only erroneous to allocate task objects or objects
--  containing task components to a subpool.
--

with Ada.Task_Identification; use Ada.Task_Identification;
with System.Storage_Elements; use System;
with System.Storage_Pools;

package Basic_Bounded_Dynamic_Pools is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   Default_Size : constant := 16#FFFF#;
   --  A Block Size is the size of the heap allocation used when more
   --  storage is needed for the pool. Larger block sizes imply less
   --  heap allocations. Generally, better performance involves using larger
   --  block sizes.

   type Basic_Dynamic_Pool
     (Size : Storage_Elements.Storage_Count := Default_Size;
      Heap_Allocated : Boolean := True)
     is new Storage_Pools.Root_Storage_Pool with private;
   --  The Size specifies how much storage is managed by the pool.
   --  If Heap_Allocated is true, the storage is allocated from
   --  heap, otherwise the storage is directly in the Pool object.

   overriding
   function Storage_Size
     (Pool : Basic_Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the amount of storage managed by the pool.

   function Storage_Used
     (Pool : Basic_Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated to
   --  objects from the pool.

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean with Inline;
   --  Returns True if the specified task "owns" the pool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task)
   with
     Inline,
     Pre => (Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
       or else (Is_Owner (Pool) and then T = Null_Task_Id),
     Post => Is_Owner (Pool, T);
   --  An Owning task can relinquish ownership of a pool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a pool,
   --  provided that the pool has no owner.

private

   subtype Storage_Array is System.Storage_Elements.Storage_Array;

   type Storage_Array_Access is access Storage_Array;

   subtype Storage_Array_Index is System.Storage_Elements.Storage_Offset
   with Static_Predicate => Storage_Array_Index >= 1;

   type Basic_Dynamic_Pool
     (Size : Storage_Elements.Storage_Count := Default_Size;
      Heap_Allocated : Boolean := True)
     is new Storage_Pools.Root_Storage_Pool with
      record
         Next_Allocation : Storage_Array_Index;
         Owner : Ada.Task_Identification.Task_Id;
         case Heap_Allocated is
            when True =>
               Active_Access : Storage_Array_Access;

            when False =>
               Active : Storage_Array (1 .. Size);
         end case;
      end record
   with Type_Invariant =>
     (Basic_Dynamic_Pool.Heap_Allocated and then
        Basic_Dynamic_Pool.Active_Access /= null and then
          Basic_Dynamic_Pool.Next_Allocation <=
            Basic_Dynamic_Pool.Active_Access'Length)
       or else (not Basic_Dynamic_Pool.Heap_Allocated and then
                      Basic_Dynamic_Pool.Next_Allocation <=
                        Basic_Dynamic_Pool.Active'Length);

   use type Storage_Elements.Storage_Count;

   overriding
   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   with Inline, Pre => Is_Owner (Pool, Current_Task);

   overriding
   procedure Deallocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is null;

   overriding
   procedure Initialize (Pool : in out Basic_Dynamic_Pool) with Inline;

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool) with Inline;

end Basic_Bounded_Dynamic_Pools;
