with ACO.Log;

package body ACO.Nodes is

   procedure Initialize
     (This : in out Node)
   is
      use ACO.States;
      use ACO.Log;
   begin
      This.Set_State (Initializing);

      ACO.Log.Set_Level (Debug);

      This.Set_State (Pre_Operational);

      ACO.Log.Put_Line (Info, "Initialized CANopen");
   end Initialize;

   procedure Set_State
     (This  : in out Node;
      State : in     ACO.States.State)
   is
      use type ACO.States.State;
   begin
      if State /= This.Od.Get_Node_State then
--           case State is
--              when Initializing =>
--              when Pre_Operational =>
--              when Operational =>
--              when Stopped =>
--              when Unknown_State =>
--           end case;
         This.NMT.Set_State (This.Id, State);
      end if;
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
      loop
         This.Driver.Await_Message (Msg);

         ACO.Log.Put_Line (Debug, "Received message " & Image (Msg));

         if Node_Id (Msg) = This.Id or else
            Node_Id (Msg) = Broadcast_Id
         then
            This.Dispatch (Msg);
         end if;
      end loop;
   end Receiver_Task;

end ACO.Nodes;
