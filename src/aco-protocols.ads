with ACO.States;
private with ACO.OD;

package ACO.Protocols is

   pragma Preelaborate;

   type Protocol is abstract tagged limited private;

   type Protocol_Access is access all Protocol'Class;

   procedure On_State_Change
     (This     : in out Protocol;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State) is abstract;

   procedure Setup_Internal_Callbacks
     (This : in out Protocol);

private

   type Node_State_Change_Subscriber (Protocol_Ref : not null access Protocol'Class) is
      new ACO.OD.Node_State_Pubsub.Sub with null record;

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.OD.State_Transition);

   type Protocol is abstract tagged limited record
      Node_State_Change_Indication : aliased Node_State_Change_Subscriber (Protocol'Access);
   end record;

end ACO.Protocols;
