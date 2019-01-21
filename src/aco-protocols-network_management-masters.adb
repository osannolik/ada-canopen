package body ACO.Protocols.Network_Management.Masters is

   procedure Request_State
      (This  : in out Master;
       State : in     ACO.States.State)
   is
      Cmd : NMT_Commands.NMT_Command;
   begin
      This.Set (State);

      Cmd := (As_Raw            => False,
              Command_Specifier => NMT_Commands.To_CMD_Spec (State),
              Node_Id           => This.Id);

      This.Handler.Put (NMT_Commands.To_Msg (Cmd));
   end Request_State;

   procedure Set_Heartbeat_Timeout
      (This    : in out Master;
       Timeout : in     Natural)
   is
   begin
      This.Timeout_Ms := Timeout;
      This.T_Heartbeat_Update := Ada.Real_Time.Time_Last;
   end Set_Heartbeat_Timeout;

   procedure Periodic_Actions
      (This  : in out Master;
       T_Now : in     Ada.Real_Time.Time)
   is
      use Ada.Real_Time;
      use ACO.States;
   begin
      case This.Get is
         when Initializing | Pre_Operational | Operational | Stopped =>
            if This.Timeout_Ms > 0 and then
               T_Now >= This.T_Heartbeat_Update + Milliseconds (This.Timeout_Ms)
            then
               This.Set (Unknown_State);
               This.Od.Events.Heartbeat_Timed_Out.Put (This.Id);
            end if;

         when Unknown_State =>
            null;
      end case;
   end Periodic_Actions;

   overriding
   procedure Update
      (This : access Heartbeat_Subscriber;
       Data : in     ACO.Events.Heartbeat_Data)
   is
   begin
      --  TODO: Should really use timestamp of CAN message instead since this
      --        event probably is delayed from the time of reception.
      This.Ref.T_Heartbeat_Update := Ada.Real_Time.Clock;
      This.Ref.Set (Data.State);
   end Update;

   overriding
   procedure Initialize
      (This : in out Master)
   is
   begin
      NMT (This).Initialize;

      This.Od.Events.Heartbeat_Received.Attach
         (This.Heartbeat_Update'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize
      (This : in out Master)
   is
   begin
      NMT (This).Finalize;

      This.Od.Events.Heartbeat_Received.Detach
         (This.Heartbeat_Update'Unchecked_Access);
   end Finalize;

end ACO.Protocols.Network_Management.Masters;
