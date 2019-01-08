package body ACO.Nodes is

   procedure On_Message_Dispatch
      (This : in out Node_Base;
       Msg  : in     ACO.Messages.Message)
   is
      use type ACO.Messages.Id_Type;
      use type ACO.Messages.Function_Code;

      Func : constant ACO.Messages.Function_Code :=
         ACO.Messages.Func_Code (Msg);
   begin
      if ACO.Messages.CAN_Id (Msg) =
         ACO.Protocols.Network_Management.NMT_CAN_Id
      then
         This.NMT.Message_Received (Msg, This.Id);
      elsif ACO.Messages.CAN_Id (Msg) =
         ACO.Protocols.Synchronization.SYNC_CAN_Id
      then
         This.SYNC.Message_Received (Msg);
      elsif Func = ACO.Protocols.Error_Control.EC_Id then
         This.EC.Message_Received (Msg);
      elsif Func = ACO.Protocols.Service_Data.SDO_C2S_Id or
         Func = ACO.Protocols.Service_Data.SDO_S2C_Id
      then
         This.SDO.Message_Received (Msg);
      end if;
   end On_Message_Dispatch;

   procedure Periodic_Actions
      (This  : in out Node_Base;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.EC.Periodic_Actions (T_Now);
      This.SDO.Periodic_Actions (T_Now);
      This.SYNC.Periodic_Actions (T_Now);
   end Periodic_Actions;

   overriding
   procedure Update
      (This : access Tick_Subscriber;
       Data : in     Ada.Real_Time.Time)
   is
   begin
      This.Node_Ref.Periodic_Actions (T_Now => Data);

      This.Node_Ref.Od.Events.Process;
   end Update;

   overriding
   procedure Update
      (This : access Message_Subscriber;
       Data : in     ACO.Messages.Message)
   is
   begin
      This.Node_Ref.On_Message_Dispatch (Msg => Data);
   end Update;

   overriding
   procedure Initialize (This : in out Node_Base)
   is
   begin
      This.Handler.Events.Periodic_Action.Attach
         (Subscriber => This.Periodic_Action_Indication'Unchecked_Access);
      This.Handler.Events.Received_Message.Attach
         (Subscriber => This.New_Message_Indication'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (This : in out Node_Base)
   is
   begin
      This.Handler.Events.Periodic_Action.Detach
         (Subscriber => This.Periodic_Action_Indication'Unchecked_Access);
      This.Handler.Events.Received_Message.Detach
         (Subscriber => This.New_Message_Indication'Unchecked_Access);
   end Finalize;

end ACO.Nodes;
