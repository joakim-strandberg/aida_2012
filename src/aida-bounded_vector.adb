package body Aida.Bounded_Vector is

   function Default_Vector (C : Capacity_T) return T is
   begin
      return This : T (C) do
         This.Items := (others => Default_Element);
         This.Last_Index := Extended_Index_T'First;
      end return;
   end Default_Vector;

   procedure Append (This     : in out T;
                     New_Item : Element_T) is
   begin
      if This.Last_Index = Extended_Index_T'First then
         This.Last_Index := Index_T'First;
         This.Items (1) := New_Item;
      else
         This.Last_Index := This.Last_Index + 1;
         This.Items (Index_T (This.Last_Index)) := New_Item;
      end if;
   end Append;

   function Contains (This    : T;
                      Element : Element_T) return Boolean
   is
      Result : Boolean := False;
   begin
      for I in Extended_Index_T range 1..This.Last_Index loop
         if This.Items (I) = Element then
            Result := True;
            exit;
         end if;
         pragma Loop_Variant (Increases => I);
      end loop;
      return Result;
   end Contains;

   function Element (This  : T;
                     Index : Index_T) return Element_T is
   begin
      return This.Items (Index);
   end Element;

   function First_Index (This : T) return Index_T is
      pragma Unreferenced (This);
   begin
      return Index_T'First;
   end First_Index;

   function Last_Element (This : T) return Element_T is
   begin
      return This.Items (Index_T (This.Last_Index));
   end Last_Element;

   procedure Delete_Last (This : in out T) is
   begin
      This.Last_Index := This.Last_Index - 1;
   end Delete_Last;

   procedure Act_On_Immutable_Elements (This : T) is
   begin
      Do_Something (This.Items (Index_T'First..Index_T(This.Last_Index)));
   end Act_On_Immutable_Elements;

   procedure Act_On_Mutable_Elements (This : in out T) is
   begin
      Do_Something (This.Items (Index_T'First..Index_T(This.Last_Index)));
   end Act_On_Mutable_Elements;

   procedure Clear (This : in out T) is
   begin
      This.Last_Index := Extended_Index_T'First;
   end Clear;

   procedure Replace_Element (This        : in out T;
                              Index       : Index_T;
                              New_Element : Element_T) is
   begin
      This.Items (Index) := New_Element;
   end Replace_Element;

end Aida.Bounded_Vector;
