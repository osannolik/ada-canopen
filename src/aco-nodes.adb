package body ACO.Nodes is

   procedure Init
     (This : in out Node)
   is
   begin
      Ada.Synchronous_Task_Control.Set_True (This.Start_Receiver_Task);
   end Init;

   overriding
   procedure Initialize (This : in out Node)
   is
   begin
      This.Od.Events.Node_State_Modified.Attach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (This : in out Node)
   is
   begin
      This.Od.Events.Node_State_Modified.Detach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);
   end Finalize;

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.States.State_Transition)
   is
      use ACO.States;
      use ACO.Log;
   begin
      case Data.Current is
         when Initializing =>
            Init (This.Node_Ref.all);

         when Pre_Operational | Operational | Stopped | Unknown_State =>
            --  Put protocol handlers into correct state?
            null;
      end case;
   end Update;

   procedure Set_State
     (This  : in out Node;
      State : in     ACO.States.State)
   is
   begin
      This.NMT.Set_State (State);
   end Set_State;

   procedure Dispatch
     (This : in out Node;
      Msg  : in     Message)
   is
      Func : constant Function_Code := Func_Code (Msg);
   begin
      if CAN_Id (Msg) = Network_Management.NMT_CAN_Id then
         This.NMT.Message_Received (Msg, This.Id);
      elsif CAN_Id (Msg) = Synchronization.SYNC_CAN_Id then
         This.SYNC.Message_Received (Msg);
      elsif Func = Error_Control.EC_Id then
         This.EC.Message_Received (Msg);
      elsif Func = Service_Data.SDO_C2S_Id or
            Func = Service_Data.SDO_S2C_Id then
         This.SDO.Message_Received (Msg);
      end if;
   end Dispatch;

   task body Receiver_Task
   is
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (This.Start_Receiver_Task);
      This.Node_Log (ACO.Log.Debug, "Starting receiver task...");

      loop
         This.Get_Received_Messages (Block => True);
      end loop;
   end Receiver_Task;

   procedure Get_Received_Messages
      (This  : in out Node;
       Block : in     Boolean := False)
   is
      Msg : Message;
   begin
      if Block or else This.Driver.Is_Message_Pending then
         This.Driver.Receive_Message_Blocking (Msg);
         This.Node_Log (ACO.Log.Debug, "Received message " & Image (Msg));

         if This.Received_Messages.Is_Full then
            This.Node_Log (ACO.Log.Warning, "Ignoring message: buffer full");
         else
            This.Received_Messages.Put_Blocking (Msg);
         end if;
      end if;
   end Get_Received_Messages;

   procedure Process_Received_Messages
      (This : in out Node)
   is
      Msg : Message;
   begin
      while not This.Received_Messages.Is_Empty loop
         This.Received_Messages.Get (Msg);
         This.Dispatch (Msg);
      end loop;
   end Process_Received_Messages;

   procedure Periodic_Actions
      (This  : in out Node;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.Od.Events.Process;

      This.EC.Periodic_Actions (T_Now);
      This.SDO.Periodic_Actions (T_Now);
      This.SYNC.Periodic_Actions (T_Now);

      This.Process_Received_Messages;
   end Periodic_Actions;

   task body Periodic_Task
   is
      use type Ada.Real_Time.Time;

      Next_Release : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
         Ada.Real_Time.Milliseconds (Configuration.Periodic_Task_Period_Ms);
   begin
      This.Node_Log (ACO.Log.Debug, "Starting periodic worker task...");

      Next_Release := Ada.Real_Time.Clock;

      loop
         This.Periodic_Actions (T_Now => Next_Release);

         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Periodic_Task;

   procedure Node_Log
     (This    : in out Node;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
   begin
      ACO.Log.Put_Line (Level, "(Node" & This.Id'Img & ") " & Message);
   end Node_Log;

end ACO.Nodes;
