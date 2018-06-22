with ACO.Messages;
with ACO.Drivers;
with ACO.OD;

private with ACO.States;
private with ACO.Log;
private with ACO.Utils.Generic_Alarms;

package ACO.Protocols.Error_Control is

   use ACO.Messages;
   use ACO.OD;

   EC_Id : constant Function_Code := 16#E#;

   type EC
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   overriding
   procedure Setup_Internal_Callbacks (This : in out EC);

   procedure Message_Received
     (This : in out EC;
      Msg  : in     Message);

   procedure Update_Alarms
     (This : in out EC);

private

   package Alarms is new ACO.Utils.Generic_Alarms (Max_Nof_Heartbeat_Slaves + 1);

   type Heartbeat_Producer_Alarm (EC_Ref : not null access EC) is
      new Alarms.Alarm_Type with null record;

   overriding
   procedure Signal (This : access Heartbeat_Producer_Alarm);

   type Heartbeat_Consumer_Alarm is new Alarms.Alarm_Type with record
      Slave_Id : Node_Nr := 0;
   end record;

   type Consumer_Alarm_Access is access all Heartbeat_Consumer_Alarm;

   overriding
   procedure Signal (This : access Heartbeat_Consumer_Alarm);

   type Heartbeat_Consumer_Alarms is
      array (Positive range <>) of aliased Heartbeat_Consumer_Alarm;

   type Heartbeat_Consumer_Change_Subscriber (EC_Ref : not null access EC'Class) is
      new ACO.OD.Natural_Pubsub.Sub with null record;

   overriding
   procedure Update
      (This : access Heartbeat_Consumer_Change_Subscriber;
       Data : in     Natural);

   type Heartbeat_Producer_Change_Subscriber (EC_Ref : not null access EC'Class) is
      new ACO.OD.Natural_Pubsub.Sub with null record;

   overriding
   procedure Update
      (This : access Heartbeat_Producer_Change_Subscriber;
       Data : in     Natural);

   type EC
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol with
   record
      Event_Manager : Alarms.Alarm_Manager;
      Producer_Alarm : aliased Heartbeat_Producer_Alarm (EC'Access);
      Consumer_Alarms : Heartbeat_Consumer_Alarms (1 .. Max_Nof_Heartbeat_Slaves);
      Heartbeat_Consumer_Change_Indication :
         aliased Heartbeat_Consumer_Change_Subscriber (EC'Access);
      Heartbeat_Producer_Change_Indication :
         aliased Heartbeat_Producer_Change_Subscriber (EC'Access);
   end record;

   procedure Send_Bootup (This : in out EC);

   procedure Heartbeat_Producer_Start (This : in out EC);

   procedure Heartbeat_Producer_Stop (This : in out EC);

   procedure Heartbeat_Consumer_Reset
     (This     : in out EC;
      Slave_Id : in     Node_Nr);

   procedure Heartbeat_Consumer_Start
     (This     : in out EC;
      Slave_Id : in     Node_Nr);

   function Is_Heartbeat_Monitored_Slave
     (This     : in out EC;
      Slave_Id : in     Node_Nr)
      return Boolean;

   overriding
   procedure On_State_Change
     (This     : in out EC;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure EC_Log
     (This    : in out EC;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

end ACO.Protocols.Error_Control;
