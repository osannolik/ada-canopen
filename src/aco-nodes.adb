with Ada.Real_Time;

package body ACO.Nodes is

   procedure Init
     (This : in out Node)
   is
   begin
      Ada.Synchronous_Task_Control.Set_True (This.Start_Receiver_Task);
      Ada.Synchronous_Task_Control.Set_True (This.Start_Periodic_Task);
   end Init;

   overriding
   procedure Initialize (This : in out Node)
   is
   begin
      This.Od.Events.Node_State_Change.Attach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (This : in out Node)
   is
   begin
      This.Od.Events.Node_State_Change.Detach
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
      elsif Func = Service_Data.SDO_Tx_Id or
            Func = Service_Data.SDO_Rx_Id then
         This.SDO.Message_Received (Msg);
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

         if not This.Received_Messages.Is_Full then
            This.Received_Messages.Put_Blocking (Msg);
         else
            This.Node_Log (Warning, "Ignoring message: buffer full");
         end if;
      end loop;
   end Receiver_Task;

   task body Periodic_Task
   is
      use ACO.Log;
      use Ada.Real_Time;
      use ACO.Messages.Buffer;

      Next_Release : Time := Clock;
      Period : constant Time_Span :=
         Milliseconds (Configuration.Periodic_Task_Period_Ms);
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (This.Start_Periodic_Task);
      This.Node_Log (Debug, "Starting periodic worker task...");

      loop
         This.EC.Periodic_Actions;
         This.SDO.Periodic_Actions;
         This.SYNC.Periodic_Actions;

         while not This.Received_Messages.Is_Empty loop
            declare
               Msg : Message;
            begin
               This.Received_Messages.Get (Msg);
               This.Dispatch (Msg);
            end;
         end loop;

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
