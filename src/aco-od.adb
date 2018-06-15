package body ACO.OD is

   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State)
   is
      Tmp : constant State_Transition :=
         (Previous => This.Node_State,
          Current  => Node_State);
   begin
      This.Node_State := Node_State;
      Node_State_Change_Indication.Update (Tmp);
   end Set_Node_State;

   function Get_Node_State (This : Object_Dict) return ACO.States.State is
     (This.Node_State);

end ACO.OD;
