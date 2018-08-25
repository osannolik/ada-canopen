package body ACO.States is

   No_Node : constant State_Record :=
      (Is_Used    => False,
       Node_Id    => 0,
       Node_State => Unknown_State);

   use type ACO.Messages.Node_Nr;

   function Is_Full (This : Node_States_List) return Boolean is
      (for all N of This.Node_States => N.Is_Used);

   function In_List
      (This    : Node_States_List;
       Node_Id : ACO.Messages.Node_Nr)
       return Boolean
   is
      (for some N of This.Node_States => N.Is_Used and then N.Node_Id = Node_Id);

   procedure Clear
      (This : in out Node_States_List)
   is
   begin
      This.Node_States := (others => No_Node);
   end Clear;

   procedure Add_Node
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr;
       Node_State : in     State)
   is

   begin
      if This.In_List (Node_Id) then
         This.Set_Node_State (Node_Id, Node_State);
      else
         for N of This.Node_States loop
            if not N.Is_Used then
               N := (Is_Used    => True,
                     Node_Id    => Node_Id,
                     Node_State => Node_State);
               return;
            end if;
         end loop;
      end if;
   end Add_Node;

   procedure Remove_Node
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr)
   is
      pragma Warnings (Off, This);
   begin
      for N of This.Node_States loop
         if N.Is_Used and then N.Node_Id = Node_Id then
            N := No_Node;
         end if;
      end loop;
   end Remove_Node;

   procedure Set_Node_State
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr;
       Node_State : in     State)
   is
   begin
      for N of This.Node_States loop
         if N.Is_Used and then N.Node_Id = Node_Id then
            N.Node_State := Node_State;
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
   begin
      for N of This.Node_States loop
         if N.Is_Used and then N.Node_Id = Node_Id then
            return N.Node_State;
         end if;
      end loop;
      return Unknown_State;
   end Get_Node_State;

end ACO.States;
