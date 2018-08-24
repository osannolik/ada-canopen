with ACO.Messages;
with ACO.OD;
with ACO.Drivers;
with ACO.States;

private with ACO.Log;
private with ACO.Utils.Generic_Alarms;

package ACO.Protocols.Service_Data is

   use ACO.Messages;

   SDO_Tx_Id : constant Function_Code := 16#B#;
   SDO_Rx_Id : constant Function_Code := 16#C#;

   type SDO
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   procedure Message_Received
     (This : in out SDO;
      Msg  : in     Message);

   procedure Update_Alarms
     (This : in out SDO);

private

   overriding
   procedure Initialize (This : in out SDO);

   overriding
   procedure Finalize (This : in out SDO);

   package Alarms is new ACO.Utils.Generic_Alarms (1);

   type SDO
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol (Od) with
   record
      Event_Manager : Alarms.Alarm_Manager;
   end record;

   overriding
   procedure On_State_Change
     (This     : in out SDO;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure SDO_Log
     (This    : in out SDO;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

end ACO.Protocols.Service_Data;
