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
      (This.Heartbeat_Producer_Period);

   function Get_Communication_Cycle_Period (This : Object_Dict) return Natural is
      (This.Communication_Cycle_Period);

   function Get_Sync_Counter_Overflow (This : Object_Dict) return Sync_Counter is
      (This.Sync_Counter_Overflow_Value);

   procedure Set_Heartbeat_Producer_Period
     (This   : in out Object_Dict;
      Period : in     Natural)
   is
   begin
      This.Heartbeat_Producer_Period := Period;
   end Set_Heartbeat_Producer_Period;

   procedure Set_Communication_Cycle_Period
     (This   : in out Object_Dict;
      Period : in     Natural)
   is
   begin
      This.Communication_Cycle_Period := Period;
      Sync_Producer_Change_Indication.Update (Period);
   end Set_Communication_Cycle_Period;

   procedure Set_Sync_Counter_Overflow
     (This           : in out Object_Dict;
      Overflow_Value : in     Sync_Counter)
   is
   begin
      This.Sync_Counter_Overflow_Value := Overflow_Value;
      Sync_Producer_Change_Indication.Update (Overflow_Value);
   end Set_Sync_Counter_Overflow;

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
