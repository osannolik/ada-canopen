with ACO.States;
with ACO.Events;
with ACO.OD;

package ACO.Protocols is

   pragma Preelaborate;

   type Protocol (Od : not null access ACO.OD.Object_Dict'Class) is
      abstract tagged limited private;

   type Protocol_Access is access all Protocol'Class;

   procedure On_State_Change
     (This     : in out Protocol;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State) is abstract;

   procedure Setup_Internal_Callbacks
     (This : in out Protocol);

private

   type Node_State_Change_Subscriber (Protocol_Ref : not null access Protocol'Class) is
      new ACO.Events.Node_State_Pubsub.Sub with null record;

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.States.State_Transition);

   type Protocol (Od : not null access ACO.OD.Object_Dict'Class) is
      abstract tagged limited
   record
      Node_State_Change_Indication : aliased Node_State_Change_Subscriber (Protocol'Access);
   end record;

end ACO.Protocols;
