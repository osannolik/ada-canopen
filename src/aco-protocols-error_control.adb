with Interfaces;
with Ada.Real_Time;

package body ACO.Protocols.Error_Control is

   package Commands is
      type EC_State is new Interfaces.Unsigned_8;

      Bootup  : constant := 0;
      Stopped : constant := 4;
      Op      : constant := 5;
      Pre_Op  : constant := 127;

      function Is_Valid_Command (Msg : Message) return Boolean is
        (Msg.Length = EC_State'Size / 8);

      function To_EC_State (Msg : Message) return EC_State is
        (EC_State (Msg.Data (0)));

      pragma Unreferenced (Stopped, Op, Pre_Op, To_EC_State);
   end Commands;

   procedure Send_Bootup
     (This : in out EC)
   is
      Msg : constant Message :=
         Create (Code => EC_Id,
                 Node => This.Id,
                 RTR  => False,
                 Data => (Msg_Data'First => Commands.Bootup));
   begin
      This.EC_Log (ACO.Log.Debug, "Sending bootup for node" & This.Id'Img);
      This.Driver.Send_Message (Msg);
   end Send_Bootup;

   overriding
   procedure Signal (This : access Heartbeat_Producer_Alarm)
   is
      use Ada.Real_Time;
   begin
      This.EC_Ref.EC_Log (ACO.Log.Debug, "HBT producer signal");
      This.EC_Ref.Event_Manager.Set
         (ACO.Utils.Alarms.Alarm_Access (This), Clock + Milliseconds (500));
   end Signal;

   overriding
   procedure On_State_Change
     (This     : in out EC;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State)
   is
      use ACO.States;
      use Ada.Real_Time;
   begin
      if Previous = Initializing and Current = Pre_Operational then
         This.Send_Bootup;
         This.Event_Manager.Set
            (Alarm       => This.Producer_Alarm'Unchecked_Access,
             Signal_Time => Clock + Milliseconds (500));
      end if;
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
