with ACO.States;
with ACO.Utils.Generic_Pubsub;

package ACO.OD is

   pragma Preelaborate;

   type Object_Dict is tagged limited private;

   type Object_Dict_Access is access all Object_Dict'Class;

   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State);

   function Get_Node_State (This : Object_Dict) return ACO.States.State;

   package Node_State_Pubsub is new ACO.Utils.Generic_Pubsub
     (Item_Type           => ACO.States.State,
      Max_Nof_Subscribers => 1);

   type Node_State_Change_Publisher is new Node_State_Pubsub.Pub with null record;

   Node_State_Change_Indication : Node_State_Change_Publisher;

private

   type Object_Dict is tagged limited record
      Node_State : ACO.States.State := ACO.States.Unknown_State;
   end record;

end ACO.OD;
