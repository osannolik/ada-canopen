with ACO.States;
with ACO.Utils.Generic_Pubsub;

package ACO.OD is
   --  This package (or a child package of it) will eventually be generated

   pragma Preelaborate;

   type Object_Dict is tagged limited private;

   type Object_Dict_Access is access all Object_Dict'Class;

   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State);

   function Get_Node_State (This : Object_Dict) return ACO.States.State;

   function Get_Heartbeat_Producer_Period (This : Object_Dict) return Natural;

   type State_Transition is record
      Previous : ACO.States.State := ACO.States.Unknown_State;
      Current  : ACO.STates.State := ACO.States.Unknown_State;
   end record;

   package Node_State_Pubsub is new ACO.Utils.Generic_Pubsub
     (Item_Type           => State_Transition,
      Max_Nof_Subscribers => 5);

   type Node_State_Change_Publisher is new Node_State_Pubsub.Pub with null record;

   Node_State_Change_Indication : Node_State_Change_Publisher;

private

   type Object_Dict is tagged limited record
      Node_State : ACO.States.State := ACO.States.Unknown_State;
   end record;

   Heartbeat_Producer_Period : constant := 500;

end ACO.OD;
