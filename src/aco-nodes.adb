package body ACO.Nodes is

   overriding
   procedure On_Event
      (This : in out Tick_Subscriber;
       Data : in     ACO.Events.Handler_Event_Data)
   is
   begin
      This.Node_Ref.Periodic_Actions (T_Now => Data.Current_Time);

      This.Node_Ref.Od.Events.Process;
   end On_Event;

   overriding
   procedure On_Event
      (This : in out Message_Subscriber;
       Data : in     ACO.Events.Handler_Event_Data)
   is
   begin
      This.Node_Ref.On_Message_Dispatch (Msg => Data.Msg);
   end On_Event;

   overriding
   procedure Initialize (This : in out Node_Base)
   is
   begin
      This.Handler.Events.Handler_Events.Attach
         (Subscriber => This.Periodic_Action_Indication'Unchecked_Access);
      This.Handler.Events.Handler_Events.Attach
         (Subscriber => This.New_Message_Indication'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (This : in out Node_Base)
   is
   begin
      This.Handler.Events.Handler_Events.Detach
         (Subscriber => This.Periodic_Action_Indication'Unchecked_Access);
      This.Handler.Events.Handler_Events.Detach
         (Subscriber => This.New_Message_Indication'Unchecked_Access);
   end Finalize;

end ACO.Nodes;
