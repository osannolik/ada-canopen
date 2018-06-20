with Interfaces;
with Ada.Real_Time;

package body ACO.Protocols.Error_Control is

   package Commands is
      use ACO.States;

      subtype EC_State is Interfaces.Unsigned_8;

      Bootup : constant := 0;
      Stop   : constant := 4;
      Op     : constant := 5;
      Pre_Op : constant := 127;

      To_EC_State : constant array (ACO.States.State) of EC_State :=
         (Unknown_State | Initializing => Bootup,
          Pre_Operational              => Pre_Op,
          Operational                  => Op,
          Stopped                      => Stop);

      function Is_Valid_Command (Msg : Message) return Boolean is
        (Msg.Length = EC_State'Size / 8);

      function Get_EC_State (Msg : Message) return EC_State is
        (EC_State (Msg.Data (0)));

      pragma Unreferenced (Get_EC_State);
   end Commands;

   function Create_Heartbeat
     (Node_State : ACO.States.State;
      Node_Id    : Node_Nr)
      return Message
   is
      (Create (Code => EC_Id,
               Node => Node_Id,
               RTR  => False,
               Data => (Msg_Data'First => Commands.To_EC_State (Node_State))));

   procedure Send_Bootup
     (This : in out EC)
   is
      Msg : constant Message :=
         Create_Heartbeat (ACO.States.Initializing, This.Id);
   begin
      This.EC_Log (ACO.Log.Debug, "Sending bootup for node" & This.Id'Img);
      This.Driver.Send_Message (Msg);
   end Send_Bootup;

   overriding
   procedure Signal (This : access Heartbeat_Producer_Alarm)
   is
      use Ada.Real_Time;
      use ACO.Utils.Alarms;

      EC_Ref : access EC renames This.EC_Ref;

      Signal_Time : constant Time :=
         Clock + Milliseconds (EC_Ref.Od.Get_Heartbeat_Producer_Period);

      Msg : constant Message :=
         Create_Heartbeat (EC_Ref.Od.Get_Node_State, EC_Ref.Id);
   begin
      EC_Ref.Event_Manager.Set (Alarm_Access (This), Signal_Time);

      EC_Ref.Driver.Send_Message (Msg);
   end Signal;

   procedure Heartbeat_Producer_Start (This : in out EC)
   is
      use Ada.Real_Time;

      Signal_Time : constant Time :=
         Clock + Milliseconds (This.Od.Get_Heartbeat_Producer_Period);
   begin
      This.Send_Bootup;
      This.Event_Manager.Set
         (Alarm       => This.Producer_Alarm'Unchecked_Access,
          Signal_Time => Signal_Time);
   end Heartbeat_Producer_Start;

   procedure Heartbeat_Producer_Stop (This : in out EC)
   is
   begin
      This.Event_Manager.Cancel (This.Producer_Alarm'Unchecked_Access);
   end Heartbeat_Producer_Stop;

   overriding
   procedure On_State_Change
     (This     : in out EC;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State)
   is
      use ACO.States;
      use Ada.Real_Time;
   begin
      case Current is
         when Initializing | Unknown_State =>
            This.Heartbeat_Producer_Stop;

         when Pre_Operational =>
            if Previous = Initializing then
               This.Heartbeat_Producer_Start;
            end if;

         when Operational | Stopped =>
            null;
      end case;
   end On_State_Change;

   procedure Message_Received
     (This : in out EC;
      Msg  : in     Message)
   is
      pragma Unreferenced (This);
      use Commands;
   begin
      if not Is_Valid_Command (Msg) then
         return;
      end if;


   end Message_Received;

   procedure Update_Alarms
     (This : in out EC)
   is
   begin
      This.Event_Manager.Process;
   end Update_Alarms;

   procedure EC_Log
     (This    : in out EC;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
      pragma Unreferenced (This);
   begin
      ACO.Log.Put_Line (Level, "(EC) " & Message);
   end EC_Log;

end ACO.Protocols.Error_Control;
