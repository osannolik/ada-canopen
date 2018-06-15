package body ACO.Protocols is

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.OD.State_Transition)
   is
   begin
      On_State_Change
         (This     => This.Protocol_Ref.all,
          Previous => Data.Previous,
          Current  => Data.Current);
   end Update;

   procedure Setup_Internal_Callbacks
     (This : in out Protocol)
   is
   begin
      ACO.OD.Node_State_Change_Indication.Attach
         (Subscriber => This.Node_State_Change_Indication'Unchecked_Access);
   end Setup_Internal_Callbacks;

end ACO.Protocols;
