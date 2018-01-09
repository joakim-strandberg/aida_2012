with Aida.Deepend_XML_SAX_Parser;

pragma Elaborate_All (Aida.Deepend_XML_SAX_Parser);

package body Aida.Deepend_XML_DOM_Parser is

   procedure Start_Tag (This        : in out SAX_Parser_T;
                        Tag_Name    : Aida.String_T;
                        Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case This.State is
         when Expecting_Object_Start =>
            if
              Tag_Name'Length > 0 and
              This.Current_Nodes.Is_Empty
            then
               declare
                  Current_Node : not null Node_Ptr := new (This.Subpool) Node_T;
               begin
                  Current_Node.Inner.My_Tag.My_Name := new (This.Subpool) Aida.String_T'(Tag_Name);
                  This.Current_Nodes.Append (Current_Node);
                  This.Root_Node := Current_Node;
               end;
               This.State := Expecting_Default;
            else
               Call_Result.Initialize (-1835682926, -1646923797);
            end if;
         when Expecting_Default =>
            if
              Tag_Name'Length > 0
            then
               declare
                  Current_Node : not null Node_Ptr := new (This.Subpool) Node_T;
               begin
                  Current_Node.Inner.My_Tag.My_Name := new (This.Subpool) Aida.String_T'(Tag_Name);

                  if This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Id = XML_Tag then
                     This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Inner.My_Tag.My_Child_Nodes.Append (Current_Node);

                     This.Current_Nodes.Append (Current_Node);
                  else
                     Call_Result.Initialize (0638744504, -1415872799);
                  end if;
               end;
            else
               Call_Result.Initialize (1882474635, -0124544835);
            end if;
         when End_State =>
            Call_Result.Initialize (-1916263923, -1891215598);
      end case;
   end Start_Tag;

   procedure End_Tag (This        : in out SAX_Parser_T;
                      Tag_Name    : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case This.State is
         when Expecting_Default =>
            if not This.Current_Nodes.Is_Empty and then
              (This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Id = XML_Tag)
            then
               if This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Tag.Name = Tag_Name then
                  This.Current_Nodes.Delete_Last;
                  if This.Current_Nodes.Is_Empty then
                     This.State := End_State;
                  end if;
               else
                  Call_Result.Initialize (-1262093307, -2026465349);
               end if;
            else
               Call_Result.Initialize (1242872628, -0551706534);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1504928428, -0102009206);
      end case;
   end End_Tag;

   procedure Text (This        : in out SAX_Parser_T;
                   Value       : Aida.String_T;
                   Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case This.State is
         when Expecting_Default =>
            if Value'Length = 0 or (Value'Length > 0 and then
                                      (for all I in Value'Range => Value (I) = ' ' or Value (I) = Character'Val (10) or Value (I) = Character'Val (13)))
            then
               null;
            elsif
              not This.Current_Nodes.Is_Empty
            then
               declare
                  Current_Node : not null Node_Ptr := new (This.Subpool) Node_T;
               begin
                  Current_Node.Inner := (My_Id   => XML_Text,
                                         My_Text => new (This.Subpool) String_T'(Value));

                  if This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Id = XML_Tag then
                     This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Inner.My_Tag.My_Child_Nodes.Append (Current_Node);
                  else
                     Call_Result.Initialize (0638744504, -1415872799);
                  end if;
               end;
            else
               Call_Result.Initialize (0621778668, -1095646032);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-0440932756, 1176624933);
      end case;
   end Text;

   procedure Attribute (This            : in out SAX_Parser_T;
                        Attribute_Name  : Aida.String_T;
                        Attribute_Value : Aida.String_T;
                        Call_Result     : in out Aida.Subprogram_Call_Result.T) is
   begin
      case This.State is
         when Expecting_Default =>
            if not This.Current_Nodes.Is_Empty then
               if
                 Attribute_Name'Length > 0 and Attribute_Value'Length > 0
               then
                  declare
                     Attribute : not null Attribute_Ptr := new (This.Subpool) Attribute_T;
                  begin
                     Attribute.My_Name := new (This.Subpool) String_T'(Attribute_Name);

                     Attribute.My_Value := new (This.Subpool) String_T'(Attribute_Value);

                     if This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Id = XML_Tag then
                        This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Inner.My_Tag.My_Attributes.Append (Attribute);
                     else
                        Call_Result.Initialize (1957291889, -1734827804);
                     end if;
                  end;
               else
                  Call_Result.Initialize (1780391481, 0404382112);
               end if;
            else
               Call_Result.Initialize (0161942634, 2123499335);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-0353530820, -0093804016);
      end case;
   end Attribute;

   procedure Comment (This        : in out SAX_Parser_T;
                      Value       : Aida.String_T;
                      Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case This.State is
         when Expecting_Default =>
            if
              not This.Current_Nodes.Is_Empty
            then
               if
                 Value'Length > 0
               then
                  declare
                     Node : Node_Ptr := new (This.Subpool) Node_T;
                  begin
                     Node.Inner := (My_Id   => XML_Comment,
                                    My_Text => new (This.Subpool) String_T'(Value));

                     if This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Id = XML_Tag then
                        This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Inner.My_Tag.My_Child_Nodes.Append (Node);
                     else
                        Call_Result.Initialize (0638744504, -1415872799);
                     end if;
                  end;
               else
                  Call_Result.Initialize (1435504396, 0652030300);
               end if;
            else
               Call_Result.Initialize (1397833757, -1569655120);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1935786430, 1870268822);
      end case;
   end Comment;

   procedure CDATA (This        : in out SAX_Parser_T;
                    Value       : Aida.String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T) is
   begin
      case This.State is
         when Expecting_Default =>
            if
              not This.Current_Nodes.Is_Empty
            then
               if
                 Value'Length > 0
               then
                  declare
                     Node : Node_Ptr := new (This.Subpool) Node_T;
                  begin
                     Node.Inner := (My_Id   => XML_CDATA,
                                    My_Text => new (This.Subpool) String_T'(Value));

                     if This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Id = XML_Tag then
                        This.Current_Nodes.Constant_Reference (This.Current_Nodes.Last_Index).all.Inner.My_Tag.My_Child_Nodes.Append (Node);
                     else
                        Call_Result.Initialize (-0012453674, 1793720627);
                     end if;
                  end;
               else
                  Call_Result.Initialize (0725532100, 1302769854);
               end if;
            else
               Call_Result.Initialize (0616203826, 1974203512);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1478093578, 1816232498);
      end case;
   end CDATA;

   procedure Parse (This        : in out DOM_Parser_T;
                    Subpool     : in out Dynamic_Pools.Subpool_Handle;
                    XML_Message : String_T;
                    Call_Result : in out Aida.Subprogram_Call_Result.T;
                    Root_Node   :    out Node_Ptr)
   is
      SAX_Parser : SAX_Parser_T;
   begin
      SAX_Parser.Subpool := Subpool;
      SAX_Parser.Parse (XML_Message,
                        Call_Result);

      Root_Node := SAX_Parser.Root_Node;
   end Parse;

end Aida.Deepend_XML_DOM_Parser;
