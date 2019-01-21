package body ACO.Events is

   procedure Process
      (This : in out Node_Event_Manager)
   is
   begin
      This.Heartbeat_Received.Process;
      This.Heartbeat_Timed_Out.Process;
      This.Entry_Updated.Process;
      This.Node_State_Modified.Process;
      This.Slave_State_Change.Process;
   end Process;

end ACO.Events;
