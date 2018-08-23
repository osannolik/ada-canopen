with ACO.Messages;
with ACO.OD;
with ACO.Drivers;
with ACO.States;

private with Interfaces;
private with ACO.Log;
private with ACO.Utils.Generic_Alarms;
private with ACO.OD_Types;

package ACO.Protocols.Synchronization is

   use ACO.Messages;

   SYNC_CAN_Id : constant Id_Type := 16#80#;

   type SYNC
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   procedure Message_Received
     (This : in out SYNC;
      Msg  : in     Message);

   procedure Update_Alarms
     (This : in out SYNC);

private

   subtype Sync_Counter is Natural range 0 .. 240;

   overriding
   procedure Initialize (This : in out SYNC);

   overriding
   procedure Finalize (This : in out SYNC);

   package Alarms is new ACO.Utils.Generic_Alarms (1);

   type Sync_Producer_Alarm (SYNC_Ref : not null access SYNC) is
      new Alarms.Alarm_Type with null record;

   overriding
   procedure Signal (This : access Sync_Producer_Alarm);

   subtype Counter_Type is
      Interfaces.Unsigned_8 range 1 .. Interfaces.Unsigned_8'Last;

   type Entry_Update_Subscriber (Sync_Ref : not null access SYNC) is
      new ACO.Events.Entry_Update_Pack.Sub with null record;

   overriding
   procedure Update
      (This : access Entry_Update_Subscriber;
       Data : in     ACO.OD_Types.Entry_Index);

   type SYNC
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol (Od) with
   record
      Event_Manager : Alarms.Alarm_Manager;
      Producer_Alarm : aliased Sync_Producer_Alarm (SYNC'Access);
      Counter : Counter_Type := Counter_Type'First;
      Entry_Update : aliased Entry_Update_Subscriber (SYNC'Access);
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
