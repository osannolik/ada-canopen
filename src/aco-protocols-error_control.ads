with Ada.Real_Time;
with ACO.Messages;
with ACO.Drivers;
with ACO.OD;

private with ACO.States;
private with ACO.Log;
private with ACO.Utils.Generic_Alarms;
private with Interfaces;
private with ACO.OD_Types;
private with ACO.Slave_Monitors;

package ACO.Protocols.Error_Control is

   use ACO.Messages;
   use ACO.OD;

   EC_Id : constant Function_Code := 16#E#;

   type EC
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   procedure Message_Received
     (This : in out EC;
      Msg  : in     Message);

   procedure Periodic_Actions
      (This  : in out EC;
       T_Now : in     Ada.Real_Time.Time);

private

   package Commands is
      use ACO.States;
      use Interfaces;

      type EC_State is new Interfaces.Unsigned_8;

      Bootup : constant := 0;
      Stop   : constant := 4;
      Op     : constant := 5;
      Pre_Op : constant := 127;

      To_EC_State : constant array (ACO.States.State) of EC_State :=
         (Unknown_State | Initializing => Bootup,
          Pre_Operational              => Pre_Op,
          Operational                  => Op,
          Stopped                      => Stop);

      function Get_EC_State (Msg : Message) return EC_State is
         (EC_State (Msg.Data (0)));

      function Get_State (Msg : Message) return ACO.States.State is
         (case Msg.Data(0) is
             when Bootup => Initializing,
             when Pre_Op => Pre_Operational,
             when Op     => Operational,
             when Stop   => Stopped,
             when others => Unknown_State);

      function Is_Valid_Command (Msg : Message) return Boolean is
         (Msg.Length = EC_State'Size / 8 and then Node_Id (Msg) /= 0);

      function Is_Bootup (Msg : Message) return Boolean is
         (Get_EC_State (Msg) = Bootup);

   end Commands;

   overriding
   procedure Initialize (This : in out EC);

   overriding
   procedure Finalize (This : in out EC);

   package Alarms is new ACO.Utils.Generic_Alarms (1);

   type Heartbeat_Producer_Alarm (EC_Ref : not null access EC) is
      new Alarms.Alarm_Type with null record;

   overriding
   procedure Signal
      (This  : access Heartbeat_Producer_Alarm;
       T_Now : in     Ada.Real_Time.Time);

   type Entry_Update_Subscriber (EC_Ref : not null access EC) is
      new ACO.Events.Entry_Update.Subscriber with null record;

   overriding
   procedure Update
      (This : access Entry_Update_Subscriber;
       Data : in     ACO.OD_Types.Entry_Index);

   type EC
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol (Od) with
   record
      Event_Manager : Alarms.Alarm_Manager;
      Producer_Alarm : aliased Heartbeat_Producer_Alarm (EC'Access);
      Monitor : ACO.Slave_Monitors.Slave_Monitor (Od);
      Entry_Update : aliased Entry_Update_Subscriber (EC'Access);
   end record;

   procedure Send_Bootup (This : in out EC);

   procedure Heartbeat_Producer_Start (This : in out EC);

   procedure Heartbeat_Producer_Stop (This : in out EC);

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
