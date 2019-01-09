package body ACO.Nodes is

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
