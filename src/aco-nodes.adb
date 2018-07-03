with Ada.Real_Time;

package body ACO.Nodes is

   procedure Setup_Internal_Callbacks
     (This : in out Node)
   is
   begin
      ACO.OD.Node_State_Change_Indication.Attach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);

      This.NMT.Setup_Internal_Callbacks;
      This.EC.Setup_Internal_Callbacks;
      This.SYNC.Setup_Internal_Callbacks;
   end Setup_Internal_Callbacks;

   procedure Initialize
     (This : in out Node)
   is
      use ACO.States;
      use ACO.Log;
   begin
      Ada.Synchronous_Task_Control.Set_True (This.Start_Receiver_Task);
      Ada.Synchronous_Task_Control.Set_True (This.Start_Periodic_Task);
   end Initialize;

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.OD.State_Transition)
   is
      use ACO.States;
      use ACO.Log;
   begin
      case Data.Current is
         when Initializing =>
            Initialize (This.Node_Ref.all);

         when Pre_Operational | Operational | Stopped | Unknown_State =>
            --  Put protocol handlers into correct state?
            null;
      end case;
   end Update;

   procedure Set_State
     (This  : in out Node;
      State : in     ACO.States.State)
   is
      use ACO.States;
   begin
      case State is
         when Initializing =>
            Setup_Internal_Callbacks (This);

         when Pre_Operational | Operational | Stopped | Unknown_State =>
            null;
      end case;

      This.NMT.Set_State (State);
   end Set_State;

   procedure Dispatch
     (This : in out Node;
      Msg  : in     Message)
   is
      use ACO.Protocols;
   begin
      if CAN_Id (Msg) = Network_Management.NMT_CAN_Id then
         This.NMT.Message_Received (Msg, This.Id);
      elsif CAN_Id (Msg) = Synchronization.SYNC_CAN_Id then
         This.SYNC.Message_Received (Msg);
      elsif Func_Code (Msg) = Error_Control.EC_Id then
         This.EC.Message_Received (Msg);
      end if;
   end Dispatch;

   task body Receiver_Task
   is
      use ACO.Log;

      Msg : Message;
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (This.Start_Receiver_Task);
      This.Node_Log (Debug, "Starting receiver task...");

      loop
         This.Driver.Await_Message (Msg);

         This.Node_Log (Debug, "Received message " & Image (Msg));

         This.Dispatch (Msg);
      end loop;
   end Receiver_Task;

   task body Periodic_Task
   is
      use ACO.Log;
      use Ada.Real_Time;

      Next_Release : Time := Clock;
      Period : constant Time_Span := Milliseconds (1);  --  alarm resolution
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (This.Start_Periodic_Task);
      This.Node_Log (Debug, "Starting periodic worker task...");

      loop
         This.EC.Update_Alarms;
         This.SYNC.Update_Alarms;

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
