with Ada.Real_Time;

package body ACO.Protocols.Synchronization is

   function To_Ms_From_100us (T : Natural) return Natural is
      (T / 10);

   procedure Counter_Reset (This : in out SYNC)
   is
   begin
      This.Counter := Counter_Type'First;
      This.SYNC_Log (ACO.Log.Debug, "Counter reset");
   end Counter_Reset;

   procedure Counter_Increment
     (This           : in out SYNC;
      Overflow_Value : in     Sync_Counter)
   is
   begin
      if Natural (This.Counter) >= Overflow_Value then
         This.Counter_Reset;
      else
         This.Counter := Counter_Type'Succ (This.Counter);
      end if;
   end Counter_Increment;

   function Is_Counter_Expected (This : in out SYNC) return Boolean is
      (This.Od.Get_Sync_Counter_Overflow > 1);

   function Create_Sync
     (This           : in out SYNC;
      Overflow_Value : in     Sync_Counter)
      return Message
   is
      Data : constant Data_Array :=
         (if Overflow_Value > 1 then
             (Msg_Data'First => This.Counter)
          else
             Empty_Data);
   begin
      return Create (CAN_Id => SYNC_CAN_Id,
                     RTR    => False,
                     Data   => Data);
   end Create_Sync;

   procedure Send_Sync (This : in out SYNC)
   is
      Overflow_Value : constant Sync_Counter :=
         This.Od.Get_Sync_Counter_Overflow;
   begin
      This.Driver.Send_Message (This.Create_Sync (Overflow_Value));

      if Overflow_Value > 1 then
         This.Counter_Increment (Overflow_Value);
      end if;
   end Send_Sync;

   overriding
   procedure Signal (This : access Sync_Producer_Alarm)
   is
      use Ada.Real_Time;
      use Alarms;

      SYNC_Ref : access SYNC renames This.SYNC_Ref;

      Period : constant Natural :=
         To_Ms_From_100us (SYNC_Ref.Od.Get_Communication_Cycle_Period);
   begin
      if Period > 0 then
         SYNC_Ref.Event_Manager.Set (Alarm_Access (This), Clock + Milliseconds (Period));
         SYNC_Ref.Send_Sync;
      end if;
   end Signal;

   procedure Sync_Producer_Start (This : in out SYNC)
   is
      use Ada.Real_Time;

      Period : constant Natural :=
         To_Ms_From_100us (This.Od.Get_Communication_Cycle_Period);
   begin
      if Period > 0 then
         This.Event_Manager.Set
            (Alarm       => This.Producer_Alarm'Unchecked_Access,
             Signal_Time => Clock + Milliseconds (Period));
      end if;
   end Sync_Producer_Start;

   procedure Sync_Producer_Stop (This : in out SYNC)
   is
   begin
      This.Event_Manager.Cancel (This.Producer_Alarm'Unchecked_Access);
   end Sync_Producer_Stop;

   overriding
   procedure On_State_Change
     (This     : in out SYNC;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State)
   is
      use ACO.States;

      Active_In_State : constant array (State) of Boolean :=
         (Stopped | Initializing | Unknown_State => False,
          Pre_Operational | Operational          => True);

   begin
      if Previous = Initializing and Current = Pre_Operational then
         --  Bootup
         This.Counter_Reset;
      end if;

      if Active_In_State (Current) and not Active_In_State (Previous) then
         if Previous = Stopped then
            This.Counter_Reset;
         end if;
         This.Sync_Producer_Start;
      elsif not Active_In_State (Current) and Active_In_State (Previous) then
         This.Sync_Producer_Stop;
      end if;
   end On_State_Change;

   overriding
   procedure Update
      (This : access Entry_Update_Subscriber;
       Data : in     ACO.OD_Types.Entry_Index)
   is
      pragma Unreferenced (Data);

      SYNC_Ref : access SYNC renames This.SYNC_Ref;
   begin
      if SYNC_Ref.Event_Manager.Is_Pending (SYNC_Ref.Producer_Alarm'Access) then
         SYNC_Ref.Sync_Producer_Stop;
         SYNC_Ref.Counter_Reset;
         SYNC_Ref.Sync_Producer_Start;
      end if;
   end Update;

   procedure Message_Received
     (This : in out SYNC;
      Msg  : in     Message)
   is
   begin
      if This.Is_Counter_Expected and Msg.Length /= 1 then
         return;
      end if;

      --  TODO: Trigger TPDO
   end Message_Received;

   procedure Update_Alarms
     (This : in out SYNC)
   is
   begin
      This.Event_Manager.Process;
   end Update_Alarms;

   overriding
   procedure Initialize (This : in out SYNC)
   is
   begin
      Protocol (This).Initialize;

      This.Od.Events.Entry_Updated.Attach (This.Entry_Update'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (This : in out SYNC)
   is
   begin
      Protocol (This).Finalize;

      This.Od.Events.Entry_Updated.Detach (This.Entry_Update'Unchecked_Access);
   end Finalize;

   procedure SYNC_Log
     (This    : in out SYNC;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
      pragma Unreferenced (This);
   begin
      ACO.Log.Put_Line (Level, "(SYNC) " & Message);
   end SYNC_Log;

end ACO.Protocols.Synchronization;
