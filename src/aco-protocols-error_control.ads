with ACO.Messages;
with ACO.Drivers;
with ACO.OD;

private with ACO.States;
private with ACO.Log;
private with ACO.Utils.Alarms;

package ACO.Protocols.Error_Control is

   use ACO.Messages;

   EC_Id : constant Function_Code := 16#E#;

   type EC
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   procedure Message_Received
     (This : in out EC;
      Msg  : in     Message);

   procedure Update_Alarms
     (This : in out EC);

private

   type Heartbeat_Producer_Alarm (EC_Ref : not null access EC) is
      new ACO.Utils.Alarms.Alarm_Type with null record;

   overriding
   procedure Signal (This : access Heartbeat_Producer_Alarm);

   type EC
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol with
   record
      Event_Manager : ACO.Utils.Alarms.Alarm_Manager;
      Producer_Alarm : aliased Heartbeat_Producer_Alarm (EC'Access);
   end record;

   procedure Send_Bootup (This : in out EC);

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
