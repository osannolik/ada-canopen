with ACO.CANopen;
with Ada.Real_Time;

private with ACO.Events;

package ACO.Protocols.Network_Management.Masters is

   type Master
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new NMT with private;

   procedure Request_State
      (This  : in out Master;
       State : in     ACO.States.State);

   procedure Set_Heartbeat_Timeout
      (This    : in out Master;
       Timeout : in     Natural);

   procedure Periodic_Actions
      (This  : in out Master;
       T_Now : in     Ada.Real_Time.Time);

private

   type Heartbeat_Subscriber
      (Ref : not null access Master)
   is new ACO.Events.Event_Listener (ACO.Events.Heartbeat_Received)
   with null record;

   overriding
   procedure On_Event
      (This : in out Heartbeat_Subscriber;
       Data : in     ACO.Events.Event_Data);

   type Master
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new NMT (Id, Od) with record
      Heartbeat_Update : aliased Heartbeat_Subscriber (Master'Access);
      T_Heartbeat_Update : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      Timeout_Ms : Natural := 0;
   end record;

   overriding
   procedure Initialize
      (This : in out Master);

   overriding
   procedure Finalize
      (This : in out Master);

end ACO.Protocols.Network_Management.Masters;
