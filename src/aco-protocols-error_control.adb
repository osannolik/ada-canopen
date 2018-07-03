with Ada.Real_Time;

package body ACO.Protocols.Error_Control is

   function Create_Heartbeat
     (Node_State : ACO.States.State;
      Node_Id    : Node_Nr)
      return Message
   is
      (Create (Code => EC_Id,
               Node => Node_Id,
               RTR  => False,
               Data => (Msg_Data'First =>
                           Data_Type (Commands.To_EC_State (Node_State)))));

   procedure Send_Heartbeat
      (This       : in out EC;
       Node_State : in     ACO.States.State)
   is
   begin
      This.Driver.Send_Message (Create_Heartbeat (Node_State, This.Id));
   end Send_Heartbeat;

   procedure Send_Bootup
     (This : in out EC)
   is
   begin
      This.EC_Log (ACO.Log.Debug, "Sending bootup for node" & This.Id'Img);
      This.Send_Heartbeat (ACO.States.Initializing);
   end Send_Bootup;

   overriding
   procedure Signal (This : access Heartbeat_Producer_Alarm)
   is
      use Ada.Real_Time;
      use Alarms;

      EC_Ref : access EC renames This.EC_Ref;

      Period : constant Natural := EC_Ref.Od.Get_Heartbeat_Producer_Period;
   begin
      if Period > 0 then
         EC_Ref.Event_Manager.Set (Alarm_Access (This), Clock + Milliseconds (Period));
         EC_Ref.Send_Heartbeat (EC_Ref.Od.Get_Node_State);
      end if;
   end Signal;

   procedure Heartbeat_Producer_Start (This : in out EC)
   is
      use Ada.Real_Time;

      Period : constant Natural := This.Od.Get_Heartbeat_Producer_Period;
      Immediately : constant Time := Clock;
   begin
      if Period > 0 then
         This.Event_Manager.Set
            (Alarm       => This.Producer_Alarm'Unchecked_Access,
             Signal_Time => Immediately);
      end if;
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
   begin
      case Current is
         when Initializing | Unknown_State =>
            This.Heartbeat_Producer_Stop;

         when Pre_Operational =>
            if Previous = Initializing then
               This.Send_Bootup;
               This.Heartbeat_Producer_Start;
            end if;

         when Operational | Stopped =>
            null;
      end case;
   end On_State_Change;

   overriding
   procedure Signal (This : access Heartbeat_Consumer_Alarm)
   is
      use ACO.Log;
   begin
      --  Ada.Text_IO.Put_Line ("Signal CONSUMER" & This.Slave_Id'Img);

      This.Slave_Id := 0;

      --  TODO: Set node state disconnected
   end Signal;

   procedure Consumer_Alarm_Reset
     (Event_Manager : in out Alarms.Alarm_Manager;
      Alarm         : in out Heartbeat_Consumer_Alarm;
      Period        : in     Natural)
   is
      use Ada.Real_Time;
   begin
      Event_Manager.Cancel (Alarm'Unchecked_Access);

      if Period = 0 then
         Alarm.Slave_Id := 0;
      elsif Alarm.Slave_Id /= 0 then
         Event_Manager.Set
            (Alarm'Unchecked_Access, Clock + Milliseconds (Period));
      end if;
   end Consumer_Alarm_Reset;

   procedure Heartbeat_Consumer_Reset
     (This     : in out EC;
      Slave_Id : in     Node_Nr)
   is
      use Ada.Real_Time;

      Period : constant Natural :=
         This.Od.Get_Heartbeat_Consumer_Period (Slave_Id);
   begin
      This.EC_Log (ACO.Log.Debug, "Heartbeat consumer reset for slave" & Slave_Id'Img);
      for Alarm of This.Consumer_Alarms loop
         if Alarm.Slave_Id = Slave_Id then
            Consumer_Alarm_Reset
               (Event_Manager => This.Event_Manager,
                Alarm         => Alarm,
                Period        => Period);
            exit;
         end if;
      end loop;
   end Heartbeat_Consumer_Reset;

   procedure Heartbeat_Consumer_Start
     (This     : in out EC;
      Slave_Id : in     Node_Nr)
   is
      use Ada.Real_Time;

      Period : constant Natural :=
         This.Od.Get_Heartbeat_Consumer_Period (Slave_Id);
   begin
      if Period = 0 then
         return;
      end if;

      This.EC_Log (ACO.Log.Debug, "Heartbeat consumer start for slave" & Slave_Id'Img);
      for Alarm of This.Consumer_Alarms loop
         if Alarm.Slave_Id = 0 then
            Alarm.Slave_Id := Slave_Id;
            This.Event_Manager.Set
               (Alarm'Unchecked_Access, Clock + Milliseconds (Period));

            exit;
         end if;
      end loop;
   end Heartbeat_Consumer_Start;

   function Is_Heartbeat_Monitored_Slave
     (This     : in out EC;
      Slave_Id : in     Node_Nr)
      return Boolean
   is
   begin
      for Alarm of This.Consumer_Alarms loop
         if Alarm.Slave_Id = Slave_Id then
            return True;
         end if;
      end loop;

      return False;
   end Is_Heartbeat_Monitored_Slave;

   procedure Message_Received
     (This : in out EC;
      Msg  : in     Message)
   is
      use Commands;
      use ACO.States;

      Id : Node_Nr;
   begin
      if not Is_Valid_Command (Msg) then
         return;
      end if;

      case This.Od.Get_Node_State is
         when Initializing | Unknown_State =>
            return;

         when Pre_Operational | Operational | Stopped =>
            null;
      end case;

      Id := Node_Id (Msg);

      --  TODO: Update slave node state in OD

      if This.Is_Heartbeat_Monitored_Slave (Id) then
         This.Heartbeat_Consumer_Reset (Id);
      elsif Is_Bootup (Msg) then
         This.Heartbeat_Consumer_Start (Id);
      end if;
   end Message_Received;

   overriding
   procedure Update
     (This : access Heartbeat_Producer_Change_Subscriber;
      Data : in     Natural)
   is
      pragma Unreferenced (Data);
      EC_Ref : access EC renames This.EC_Ref;
   begin
      if EC_Ref.Event_Manager.Is_Pending (EC_Ref.Producer_Alarm'Access) then
         EC_Ref.Heartbeat_Producer_Stop;
         EC_Ref.Heartbeat_Producer_Start;
      end if;
   end Update;

   overriding
   procedure Update
     (This : access Heartbeat_Consumer_Change_Subscriber;
      Data : in     Natural)
   is
      pragma Unreferenced (Data);
   begin
      for Alarm of This.EC_Ref.Consumer_Alarms loop
         if Alarm.Slave_Id /= 0 then
            declare
               Period : constant Natural :=
                  This.EC_Ref.Od.Get_Heartbeat_Consumer_Period (Alarm.Slave_Id);
            begin
               Consumer_Alarm_Reset
                  (Event_Manager => This.EC_Ref.Event_Manager,
                   Alarm         => Alarm,
                   Period        => Period);
            end;
         end if;
      end loop;
   end Update;

   procedure Update_Alarms
     (This : in out EC)
   is
   begin
      This.Event_Manager.Process;
   end Update_Alarms;

   overriding
   procedure Setup_Internal_Callbacks (This : in out EC)
   is
   begin
      Protocols.Setup_Internal_Callbacks (Protocol (This));

      ACO.OD.Heartbeat_Producer_Change_Indication.Attach
         (Subscriber => This.Heartbeat_Producer_Change_Indication'Unchecked_Access);
      ACO.OD.Heartbeat_Consumer_Change_Indication.Attach
         (Subscriber => This.Heartbeat_Consumer_Change_Indication'Unchecked_Access);
   end Setup_Internal_Callbacks;

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
