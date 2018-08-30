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

      if Id /= Not_A_Slave then
         declare
            Slave_State : constant ACO.States.State := Commands.Get_State (Msg);
         begin
            if This.Monitor.Is_Monitored (Id) then
               This.Monitor.Update_State (Id, Slave_State);
            elsif Is_Bootup (Msg) then
               This.Monitor.Start (Id, Slave_State);
            end if;
         end;
      end if;
   end Message_Received;

   overriding
   procedure Update
      (This : access Entry_Update_Subscriber;
       Data : in     ACO.OD_Types.Entry_Index)
   is
      use type ACO.OD_Types.Object_Index;
      EC_Ref : access EC renames This.EC_Ref;
   begin
      case Data.Index is
         when Heartbeat_Consumer_Index =>
            EC_Ref.Monitor.Restart;

         when Heartbeat_Producer_Index =>
            if EC_Ref.Event_Manager.Is_Pending (EC_Ref.Producer_Alarm'Access) then
               EC_Ref.Heartbeat_Producer_Stop;
               EC_Ref.Heartbeat_Producer_Start;
            end if;

         when others => null;
      end case;
   end Update;

   procedure Periodic_Actions
     (This : in out EC)
   is
   begin
      This.Event_Manager.Process;
      This.Monitor.Update_Alarms;
   end Periodic_Actions;

   overriding
   procedure Initialize (This : in out EC)
   is
   begin
      Protocol (This).Initialize;

      This.Od.Events.Entry_Updated.Attach (This.Entry_Update'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (This : in out EC)
   is
   begin
      Protocol (This).Finalize;

      This.Od.Events.Entry_Updated.Detach (This.Entry_Update'Unchecked_Access);
   end Finalize;

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
