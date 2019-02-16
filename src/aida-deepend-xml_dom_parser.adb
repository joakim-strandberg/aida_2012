package body Aida.Deepend.XML_DOM_Parser is

   type State_T is
     (
      Expecting_Object_Start, -- seems to only apply to the root start tag
      --  Expecting_Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End,
      Expecting_Default, -- Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End
      End_State
     );

   function Name (This : Attribute) return String is
     (This.My_Name.all);

   function Value (This : Attribute) return String is
     (This.My_Value.all);

   function Name (This : XML_Tag) return String is
     (This.My_Name.all);

   function Child_Nodes (This : aliased XML_Tag) return Child_Nodes_Ref is
     ((Element => This.My_Child_Nodes'Access));

   function Attributes (This : aliased XML_Tag) return Attributes_Ref is
     ((Element => This.My_Attributes'Access));

   type Argument_Type
     (
      Subpool : Dynamic_Pools.Subpool_Handle
     )
   is limited record
      Current_Nodes : Node_Vectors.Vector;
      -- The current node is the last Node pointed to in the container

      Root_Node : Node_Ptr := null;
      State     : State_T := Expecting_Object_Start;
   end record;

   procedure Handle_Start_Tag
     (Argument    : in out Argument_Type;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case Argument.State is
         when Expecting_Object_Start =>
            if
              Tag_Name'Length > 0 and
              Argument.Current_Nodes.Is_Empty
            then
               declare
                  Current_Node : not null Node_Ptr
                    := new (Argument.Subpool) Node;
               begin
                  Current_Node.Tag.My_Name
                    := new (Argument.Subpool) String'(Tag_Name);
                  Argument.Current_Nodes.Append (Current_Node);
                  Argument.Root_Node := Current_Node;
               end;
               Argument.State := Expecting_Default;
            else
               Call_Result.Initialize (-2132671123, 1966624808);
            end if;
         when Expecting_Default =>
            if
              Tag_Name'Length > 0
            then
               declare
                  Current_Node : not null Node_Ptr
                    := new (Argument.Subpool) Node;
               begin
                  Current_Node.Tag.My_Name
                    := new (Argument.Subpool) String'(Tag_Name);

                  if
                    Argument.Current_Nodes.Constant_Reference
                      (Argument.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                  then
                     Argument.Current_Nodes.Constant_Reference
                       (Argument.Current_Nodes.Last_Index).all.Tag.
                       My_Child_Nodes.Append (Current_Node);

                     Argument.Current_Nodes.Append (Current_Node);
                  else
                     Call_Result.Initialize (1695756105, 1714042669);
                  end if;
               end;
            else
               Call_Result.Initialize (-0416079960, -1464855808);
            end if;
         when End_State =>
            Call_Result.Initialize (0561631589, 0761077416);
      end case;
   end Handle_Start_Tag;

   procedure Handle_End_Tag
     (Argument    : in out Argument_Type;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case Argument.State is
         when Expecting_Default =>
            if not Argument.Current_Nodes.Is_Empty and then
              (Argument.Current_Nodes.Constant_Reference
                 (Argument.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag)
            then
               if Argument.Current_Nodes.Constant_Reference
                 (Argument.Current_Nodes.Last_Index).all.Tag.My_Name.all = Tag_Name
               then
                  Argument.Current_Nodes.Delete_Last;
                  if Argument.Current_Nodes.Is_Empty then
                     Argument.State := End_State;
                  end if;
               else
                  Call_Result.Initialize (-0316487383, -2063296151);
               end if;
            else
               Call_Result.Initialize (-1355522791, 1675536860);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-0728861922, -0299445966);
      end case;
   end Handle_End_Tag;

   procedure Handle_Text
     (Argument    : in out Argument_Type;
      Value       : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case Argument.State is
         when Expecting_Default =>
            if
              Value'Length = 0 or
              (Value'Length > 0 and then
                 (for all I in Value'Range =>
                      Value (I) = ' ' or
                      Value (I) = Character'Val (10) or
                      Value (I) = Character'Val (13)))
            then
               null;
            elsif
              not Argument.Current_Nodes.Is_Empty
            then
               declare
                  Current_Node : not null Node_Ptr
                    := new (Argument.Subpool) Node (Node_Kind_Text);
               begin
                  Current_Node.all
                    := (Id   => Node_Kind_Text,
                        Text => new (Argument.Subpool) String'(Value));

                  if
                    Argument.Current_Nodes.Constant_Reference
                      (Argument.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                  then
                     Argument.Current_Nodes
                       (Argument.Current_Nodes.Last_Index).all.Tag.
                       My_Child_Nodes.Append (Current_Node);
                  else
                     Call_Result.Initialize (-0944309962, -0212130363);
                  end if;
               end;
            else
               Call_Result.Initialize (0536156601, 0921613311);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (0240750889, 1723362921);
      end case;
   end Handle_Text;

   procedure Handle_Attribute
     (Argument        : in out Argument_Type;
      Attribute_Name  : String;
      Attribute_Value : String;
      Call_Result     : in out Aida.Call_Result) is
   begin
      case Argument.State is
         when Expecting_Default =>
            if not Argument.Current_Nodes.Is_Empty then
               if
                 Attribute_Name'Length > 0 and Attribute_Value'Length > 0
               then
                  declare
                     Attr : not null Attribute_Ptr
                       := new (Argument.Subpool) Attribute;
                  begin
                     Attr.My_Name
                       := new (Argument.Subpool) String'(Attribute_Name);
                     Attr.My_Value
                       := new (Argument.Subpool) String'(Attribute_Value);

                     if
                       Argument.Current_Nodes.Constant_Reference
                       (Argument.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                     then
                        Argument.Current_Nodes
                          (Argument.Current_Nodes.Last_Index).all.Tag.
                          My_Attributes.Append (Attr);
                     else
                        Call_Result.Initialize (0612916249, -0250963769);
                     end if;
                  end;
               else
                  Call_Result.Initialize (-1091502024, -1483543078);
               end if;
            else
               Call_Result.Initialize (-0372407662, -1139199208);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1103012185, 0319457400);
      end case;
   end Handle_Attribute;

   procedure Handle_Comment
     (Argument    : in out Argument_Type;
      Value       : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case Argument.State is
         when Expecting_Default =>
            if
              not Argument.Current_Nodes.Is_Empty
            then
               if
                 Value'Length > 0
               then
                  declare
                     Current_Node : Node_Ptr
                       := new (Argument.Subpool) Node (Node_Kind_Comment);
                  begin
                     Current_Node.all
                       := (Id   => Node_Kind_Comment,
                           Text => new (Argument.Subpool) String'(Value));

                     if
                       Argument.Current_Nodes.Constant_Reference
                         (Argument.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                     then
                        Argument.Current_Nodes.Constant_Reference
                          (Argument.Current_Nodes.Last_Index).all.Tag.
                          My_Child_Nodes.Append (Current_Node);
                     else
                        Call_Result.Initialize (2066772500, 1193932906);
                     end if;
                  end;
               else
                  Call_Result.Initialize (1366102371, 1421674126);
               end if;
            else
               Call_Result.Initialize (0845969060, 0639006566);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-1373186804, -0874315849);
      end case;
   end Handle_Comment;

   procedure Handle_CDATA
     (Argument    : in out Argument_Type;
      Value       : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case Argument.State is
         when Expecting_Default =>
            if
              not Argument.Current_Nodes.Is_Empty
            then
               if
                 Value'Length > 0
               then
                  declare
                     Current_Node : Node_Ptr
                       := new (Argument.Subpool) Node (Node_Kind_CDATA);
                  begin
                     Current_Node.all
                       := (Id   => Node_Kind_CDATA,
                           Text => new (Argument.Subpool) String'(Value));

                     if
                       Argument.Current_Nodes.Constant_Reference
                         (Argument.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                     then
                        Argument.Current_Nodes.Constant_Reference
                          (Argument.Current_Nodes.Last_Index).all.Tag.
                          My_Child_Nodes.Append (Current_Node);
                     else
                        Call_Result.Initialize (-2021174626, -1403249390);
                     end if;
                  end;
               else
                  Call_Result.Initialize (1915730777, 1973598725);
               end if;
            else
               Call_Result.Initialize (-0076965217, 0193355440);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (0698504230, -0963685542);
      end case;
   end Handle_CDATA;

   procedure SAX_Parse is new Aida.XML_SAX_Parse
     (Argument_Type,
      Handle_Start_Tag,
      Handle_End_Tag,
      Handle_Text,
      Handle_Attribute,
      Handle_Comment,
      Handle_CDATA);

   procedure Parse
     (Subpool     : in out Dynamic_Pools.Subpool_Handle;
      XML_Message : String;
      Call_Result : in out Aida.Call_Result;
      Root_Node   :    out Node_Ptr)
   is
      Argument : Argument_Type (Subpool);
   begin
      SAX_Parse (Argument, XML_Message, Call_Result);

      Root_Node := Argument.Root_Node;
   end Parse;

end Aida.Deepend.XML_DOM_Parser;
