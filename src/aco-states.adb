package body ACO.States is

   function Is_Full (This : Node_States_List) return Boolean
   is
      (This.Next_Index > This.Node_States'Last);

   procedure Clear
      (This : in out Node_States_List)
   is
   begin
      This := (Node_States =>
                  (others => (Is_Used    => False,
                              Node_Id    => 0,
                              Node_State => Unknown_State)),
               Next_Index  => This.Node_States'First);
   end Clear;

   procedure Add_Node
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr;
       Node_State : in     State)
   is
      use type ACO.Messages.Node_Nr;
   begin
      for N of This.Node_States loop
         if not N.Is_Used then
            N := (Is_Used    => True,
                  Node_Id    => Node_Id,
                  Node_State => Node_State);
            This.Next_Index := This.Next_Index + 1;
            return;
         end if;
      end loop;
   end Add_Node;

   procedure Set_Node_State
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr;
       Node_State : in     State)
   is
      use type ACO.Messages.Node_Nr;
   begin
      for N of This.Node_States loop
         if N.Node_Id = Node_Id then
            N := (Is_Used    => True,
                  Node_Id    => Node_Id,
                  Node_State => Node_State);
            return;
         end if;
      end loop;

      if not This.Is_Full then
         This.Add_Node (Node_Id, Node_State);
      end if;
   end Set_Node_State;

   function Get_Node_State
      (This    : Node_States_List;
       Node_Id : ACO.Messages.Slave_Node_Nr)
       return State
   is
      use type ACO.Messages.Node_Nr;
   begin
      for N of This.Node_States loop
         if N.Is_Used and then N.Node_Id = Node_Id then
            return N.Node_State;
         end if;
      end loop;
      return Unknown_State;
   end Get_Node_State;

end ACO.States;
