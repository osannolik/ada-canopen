package body ACO.Nodes is

   procedure Setup_Internal_Callbacks
     (This : in out Node)
   is
   begin
      ACO.OD.Node_State_Change_Indication.Attach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);
   end Setup_Internal_Callbacks;

   procedure Initialize
     (This : in out Node)
   is
      use ACO.States;
      use ACO.Log;
   begin
      This.Node_Log (Debug, "Initializing...");

      Ada.Synchronous_Task_Control.Set_True (This.Start_Receiver_Task);

      This.NMT.Set_State (This.Id, Pre_Operational);
   end Initialize;

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.States.State)
   is
      use ACO.States;
      use ACO.Log;

   begin
      This.Node_Ref.Node_Log (Info, Data'Img);

      case Data is
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

      This.NMT.Set_State (This.Id, State);
   end Set_State;

   procedure Dispatch
     (This : in out Node;
      Msg  : in     Message)
   is
--        Func : constant Function_Code := Func_Code (Msg);
      use ACO.Protocols;
   begin

      if CAN_Id (Msg) = Network_Management.NMT_CAN_Id then
         This.NMT.Message_Received (Msg, This.Id);
      end if;

--        if Func = This.NMT.Code then
--           This.NMT.Message_Received (Msg);
--
--        elsif Func = This.SYNC.Code or else
--              Func = This.EMCY.Code
--        then
--           if Node_Id (Msg) = Broadcast_Id then
--              This.SYNC.Message_Received (Msg);
--           else
--              This.NMT.Message_Received (Msg);
--           end if;
--        end if;

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

         if Node_Id (Msg) = This.Id or else
            Node_Id (Msg) = Broadcast_Id
         then
            This.Dispatch (Msg);
         end if;
      end loop;
   end Receiver_Task;

   procedure Node_Log
     (This    : in out Node;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
   begin
      ACO.Log.Put_Line (Level, "Node" & This.Id'Img & ": " & Message);
   end Node_Log;

end ACO.Nodes;
