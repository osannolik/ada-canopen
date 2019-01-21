with Ada.Real_Time;
with ACO.CANopen;

private with ACO.Utils.Generic_Alarms;
private with ACO.Slave_Monitors;
private with ACO.OD_Types;

package ACO.Protocols.Error_Control.Masters is

   type Master
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new EC with private;

   procedure Periodic_Actions
      (This  : in out Master;
       T_Now : in     Ada.Real_Time.Time);

private

   package Alarms is new ACO.Utils.Generic_Alarms (1);

   type Heartbeat_Producer_Alarm
      (Ref : not null access Master)
   is new Alarms.Alarm_Type with null record;

   overriding
   procedure Signal
      (This  : access Heartbeat_Producer_Alarm;
       T_Now : in     Ada.Real_Time.Time);

   type Entry_Update_Subscriber
      (Ref : not null access Master)
   is new ACO.Events.Entry_Update.Subscriber with null record;

   overriding
   procedure Update
      (This : access Entry_Update_Subscriber;
       Data : in     ACO.OD_Types.Entry_Index);

   type Master
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new EC (Id, Od) with record
      Entry_Update : aliased Entry_Update_Subscriber (Master'Access);
      Timers : Alarms.Alarm_Manager;
      Producer_Alarm : aliased Heartbeat_Producer_Alarm (Master'Access);
      Monitor : ACO.Slave_Monitors.Slave_Monitor (Od);
   end record;

   overriding
   procedure Initialize
      (This : in out Master);

   overriding
   procedure Finalize
      (This : in out Master);

   procedure On_State_Change
      (This     : in out Master;
       Previous : in     ACO.States.State;
       Current  : in     ACO.States.State);

   procedure On_Heartbeat
      (This      : in out Master;
       Id        : in     ACO.Messages.Node_Nr;
       Hbt_State : in     EC_Commands.EC_State);

   function Create_Heartbeat
      (State   : ACO.States.State;
       Node_Id : ACO.Messages.Node_Nr)
       return ACO.Messages.Message;

   procedure Send_Bootup
      (This : in out Master);

   procedure Heartbeat_Producer_Start
      (This : in out Master);

   procedure Heartbeat_Producer_Stop
      (This : in out Master);

end ACO.Protocols.Error_Control.Masters;
