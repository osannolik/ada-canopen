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

   function Get_Heartbeat_Producer_Period (This : Object_Dict) return Natural is
     (Heartbeat_Producer_Period);

   function Get_Heartbeat_Consumer_Period
     (This    : Object_Dict;
      Node_Id : ACO.Messages.Node_Nr)
      return Natural
   is
      pragma Unreferenced (This);

      use ACO.Messages;
   begin
      --  Temporary for test
      if Node_Id = 2 then
         return 0;
      else
         return 600;
      end if;
   end Get_Heartbeat_Consumer_Period;

end ACO.OD;
