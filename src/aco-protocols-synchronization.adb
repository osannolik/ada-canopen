with ACO.States;
package body ACO.Protocols.Synchronization is

   function To_Ms_From_100us (T : Natural) return Natural is (T / 10);

   procedure Counter_Reset
      (This : in out SYNC)
   is
   begin
      This.Counter := Counter_Type'First;
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

   function Is_Counter_Expected
      (This : in out SYNC)
       return Boolean
   is
      (This.Od.Get_Sync_Counter_Overflow > 1);

   function Create_Sync
     (This           : in out SYNC;
      Overflow_Value : in     Sync_Counter)
      return ACO.Messages.Message
   is
      Data : constant ACO.Messages.Data_Array :=
         (if Overflow_Value > 1 then
             (ACO.Messages.Msg_Data'First => This.Counter)
          else
             ACO.Messages.Empty_Data);
   begin
      return ACO.Messages.Create (CAN_Id => SYNC_CAN_Id,
                                  RTR    => False,
                                  Data   => Data);
   end Create_Sync;

   procedure Send_Sync
      (This : in out SYNC)
   is
      Overflow_Value : constant Sync_Counter :=
         This.Od.Get_Sync_Counter_Overflow;
   begin
      This.Handler.Put (This.Create_Sync (Overflow_Value));

      if Overflow_Value > 1 then
         This.Counter_Increment (Overflow_Value);
      end if;
   end Send_Sync;

   overriding
   procedure Signal
      (This  : access Sync_Producer_Alarm;
       T_Now : in     Ada.Real_Time.Time)
   is
      use type Ada.Real_Time.Time;
      use Alarms;

      SYNC_Ref : access SYNC renames This.SYNC_Ref;

      Period : constant Natural :=
         To_Ms_From_100us (SYNC_Ref.Od.Get_Communication_Cycle_Period);
   begin
      if Period > 0 then
         SYNC_Ref.Timers.Set
            (Alarm_Access (This), T_Now + Ada.Real_Time.Milliseconds (Period));
         SYNC_Ref.Send_Sync;
      end if;
   end Signal;

   procedure Sync_Producer_Start
      (This : in out SYNC)
   is
      use Ada.Real_Time;

      Period : constant Natural :=
         To_Ms_From_100us (This.Od.Get_Communication_Cycle_Period);
   begin
      if Period > 0 then
         This.Timers.Set
            (Alarm       => This.Producer_Alarm'Unchecked_Access,
             Signal_Time =>
               This.Handler.Current_Time + Milliseconds (Period));
      end if;
   end Sync_Producer_Start;

   procedure Sync_Producer_Stop
      (This : in out SYNC)
   is
   begin
      This.Timers.Cancel (This.Producer_Alarm'Unchecked_Access);
   end Sync_Producer_Stop;

   overriding
   procedure On_Event
      (This : in out Node_State_Change_Subscriber;
       Data : in     ACO.Events.Event_Data)
   is
      use ACO.States;

      Active_In_State : constant array (State) of Boolean :=
         (Stopped | Initializing | Unknown_State => False,
          Pre_Operational | Operational          => True);

   begin
      if Data.State.Previous = Initializing and Data.State.Current = Pre_Operational then
         --  Bootup
         This.Sync_Ref.Counter_Reset;
      end if;

      if Active_In_State (Data.State.Current) and
         not Active_In_State (Data.State.Previous)
      then
         if Data.State.Previous = Stopped then
            This.Sync_Ref.Counter_Reset;
         end if;
         This.Sync_Ref.Sync_Producer_Start;
      elsif not Active_In_State (Data.State.Current) and
         Active_In_State (Data.State.Previous)
      then
         This.Sync_Ref.Sync_Producer_Stop;
      end if;
   end On_Event;

   overriding
   procedure On_Event
      (This : in out Entry_Update_Subscriber;
       Data : in     ACO.Events.Event_Data)
   is
      pragma Unreferenced (Data);

      SYNC_Ref : access SYNC renames This.SYNC_Ref;
   begin
      if SYNC_Ref.Timers.Is_Pending (SYNC_Ref.Producer_Alarm'Unchecked_Access)
      then
         SYNC_Ref.Sync_Producer_Stop;
         SYNC_Ref.Counter_Reset;
         SYNC_Ref.Sync_Producer_Start;
      end if;
   end On_Event;

   overriding
   function Is_Valid
      (This : in out SYNC;
       Msg  : in     ACO.Messages.Message)
       return Boolean
   is
      use type ACO.Messages.Id_Type;

      pragma Unreferenced (This);
   begin
      return ACO.Messages.CAN_Id (Msg) = SYNC_CAN_Id;
   end Is_Valid;

   procedure Message_Received
     (This : in out SYNC;
      Msg  : in     ACO.Messages.Message)
   is
   begin
      if This.Is_Counter_Expected and Msg.Length /= 1 then
         return;
      end if;

      --  TODO: Trigger TPDO
   end Message_Received;

   procedure Periodic_Actions
      (This  : in out SYNC;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.Timers.Process (T_Now);
   end Periodic_Actions;

   overriding
   procedure Initialize
      (This : in out SYNC)
   is
   begin
      Protocol (This).Initialize;

      This.Od.Events.Node_Events.Attach
         (This.Entry_Update'Unchecked_Access);
      This.Od.Events.Node_Events.Attach
         (This.State_Change'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize
      (This : in out SYNC)
   is
   begin
      Protocol (This).Finalize;

      This.Od.Events.Node_Events.Detach
         (This.Entry_Update'Unchecked_Access);
      This.Od.Events.Node_Events.Detach
         (This.State_Change'Unchecked_Access);
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
