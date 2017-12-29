package body Aida.Tagged_Bounded_Vector is

   function Max_Index (This : T) return Int32_T is
      pragma Unreferenced (This);
   begin
      return Max_Last_Index;
   end Max_Index;

   function Default_Vector return T is
   begin
      return This : T do
         This.My_Items := (others => Default_Element);
         This.My_Last_Index := Extended_Index_T'First;
      end return;
   end Default_Vector;

   procedure Append (This     : in out T;
                     New_Item : Element_T) is
   begin
      if This.My_Last_Index = Extended_Index_T'First then
         This.My_Last_Index := Index_T'First;
         This.My_Items (Index_T'First) := New_Item;
      else
         This.My_Last_Index := This.My_Last_Index + 1;
         This.My_Items (Index_T (This.My_Last_Index)) := New_Item;
      end if;
   end Append;

   function Contains (This    : T;
                      Element : Element_T) return Boolean
   is
      Result : Boolean := False;
   begin
      for I in Extended_Index_T range Index_T'First..This.Last_Index loop
         if This.My_Items (I) = Element then
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
      return This.My_Items (Index);
   end Element;

   function First_Index (This : T) return Index_T is
      pragma Unreferenced (This);
   begin
      return Index_T'First;
   end First_Index;

   function Last_Element (This : T) return Element_T is
   begin
      return This.My_Items (Index_T (This.Last_Index));
   end Last_Element;

   procedure Delete_Last (This : in out T) is
   begin
      This.My_Last_Index := This.My_Last_Index - 1;
   end Delete_Last;
--
--     procedure Act_On_Immutable_Elements (This : T) is
--     begin
--        Do_Something (This.Items (Index_T'First..Index_T(This.Last_Index)));
--     end Act_On_Immutable_Elements;
--
--     procedure Act_On_Mutable_Elements (This : in out T) is
--     begin
--        Do_Something (This.Items (Index_T'First..Index_T(This.Last_Index)));
--     end Act_On_Mutable_Elements;
--
   procedure Clear (This : in out T) is
   begin
      This.My_Last_Index := Extended_Index_T'First;
   end Clear;

   procedure Replace_Element (This        : in out T;
                              Index       : Index_T;
                              New_Element : Element_T) is
   begin
      This.My_Items (Index) := New_Element;
   end Replace_Element;

   procedure Replace_Last_Element (This        : in out T;
                                   New_Element : Element_T) is
   begin
      This.My_Items (Last_Index (This)) := New_Element;
   end Replace_Last_Element;

end Aida.Tagged_Bounded_Vector;
