package body ACO.Protocols is

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.States.State_Transition)
   is
   begin
      On_State_Change
         (This     => This.Protocol_Ref.all,
          Previous => Data.Previous,
          Current  => Data.Current);
   end Update;

   overriding
   procedure Initialize (This : in out Protocol)
   is
   begin
      This.Od.Events.Node_State_Modified.Attach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (This : in out Protocol)
   is
   begin
      This.Od.Events.Node_State_Modified.Detach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);
   end Finalize;

end ACO.Protocols;
