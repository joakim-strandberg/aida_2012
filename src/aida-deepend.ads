with Dynamic_Pools;

pragma Elaborate_All (Dynamic_Pools);

package Aida.Deepend is

   Default_Subpool : Dynamic_Pools.Dynamic_Pool (0);
   -- Allocations are done in subpools, not the default subpool

   type String_Ptr is access all String with
     Storage_Pool => Default_Subpool;

   Empty_String : aliased String := "";

end Aida.Deepend;
