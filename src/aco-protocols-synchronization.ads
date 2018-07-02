with ACO.Messages;
with ACO.OD;
with ACO.Drivers;
with ACO.States;

private with Interfaces;
private with ACO.Log;
private with ACO.Utils.Generic_Alarms;

package ACO.Protocols.Synchronization is

   use ACO.Messages;

   SYNC_CAN_Id : constant Id_Type := 16#80#;

   type SYNC
      (Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   procedure Message_Received
     (This    : in out SYNC;
      Msg     : in     Message;
      Node_Id : in     Node_Nr);

   procedure Update_Alarms
     (This : in out SYNC);

private

   package Alarms is new ACO.Utils.Generic_Alarms (1);

   type Sync_Producer_Alarm (SYNC_Ref : not null access SYNC) is
      new Alarms.Alarm_Type with null record;

   overriding
   procedure Signal (This : access Sync_Producer_Alarm);

   subtype Counter_Type is
      Interfaces.Unsigned_8 range 1 .. Interfaces.Unsigned_8'Last;

   type SYNC
      (Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol with
      record
      Event_Manager : Alarms.Alarm_Manager;
      Producer_Alarm : aliased Sync_Producer_Alarm (SYNC'Access);
      Counter : Counter_Type := Counter_Type'First;
   end record;

   overriding
   procedure On_State_Change
     (This     : in out SYNC;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure SYNC_Log
     (This    : in out SYNC;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

end ACO.Protocols.Synchronization;
