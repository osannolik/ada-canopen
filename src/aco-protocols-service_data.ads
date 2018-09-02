with ACO.Messages;
with ACO.OD;
with ACO.Drivers;
with ACO.States;

private with ACO.Log;
private with ACO.Utils.Generic_Alarms;
private with ACO.SDO_Sessions;

package ACO.Protocols.Service_Data is

   use ACO.Messages;

   SDO_S2C_Id : constant Function_Code := 16#B#;
   SDO_C2S_Id : constant Function_Code := 16#C#;

   type SDO
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class)
   is new Protocol with private;

   procedure Message_Received
     (This : in out SDO;
      Msg  : in     Message);

   procedure Periodic_Actions
     (This : in out SDO);

private
   use ACO.SDO_Sessions;

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

   function Get_Endpoint
      (Id         : Id_Type;
       Rx_CAN_Ids : Id_Array)
       return Endpoint_Nr;

   procedure Message_Received_For_Server
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Nr);

   procedure Message_Received_For_Client
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Nr);

end ACO.Protocols.Service_Data;
