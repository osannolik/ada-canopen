with ACO.States;

package ACO.OD is

   pragma Preelaborate;

   type Object_Dict is tagged limited private;

   type Object_Dict_Access is access all Object_Dict'Class;

   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State);

   function Get_Node_State (This : Object_Dict) return ACO.States.State;

private

   type Object_Dict is tagged limited record
      Node_State : ACO.States.State := ACO.States.Initializing;
   end record;

end ACO.OD;
