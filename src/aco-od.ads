with ACO.States;
with ACO.Messages;
with ACO.Utils.Generic_Pubsub;

package ACO.OD is
   --  This package (or a child package of it) will eventually be generated
   --  TODO: Generalize such that reading and writing to it is possible using
   --        e.g. SDO and application. Also, generalize indication to stack.

   pragma Preelaborate;

   type Object_Dict is tagged limited private;

   type Object_Dict_Access is access all Object_Dict'Class;

   Max_Nof_Heartbeat_Slaves : constant := 8;

   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State);

   function Get_Node_State (This : Object_Dict) return ACO.States.State;

   function Get_Heartbeat_Producer_Period (This : Object_Dict) return Natural;

   function Get_Communication_Cycle_Period (This : Object_Dict) return Natural;

   subtype Sync_Counter is Natural range 0 .. 240;

   function Get_Sync_Counter_Overflow (This : Object_Dict) return Sync_Counter;

   procedure Set_Heartbeat_Producer_Period
     (This   : in out Object_Dict;
      Period : in     Natural);

   function Get_Heartbeat_Consumer_Period
     (This    : Object_Dict;
      Node_Id : ACO.Messages.Node_Nr)
      return Natural;

   type State_Transition is record
      Previous : ACO.States.State := ACO.States.Unknown_State;
      Current  : ACO.STates.State := ACO.States.Unknown_State;
   end record;

   package Node_State_Pubsub is new ACO.Utils.Generic_Pubsub
     (Item_Type           => State_Transition,
      Max_Nof_Subscribers => 5);

   type Node_State_Change_Publisher is new Node_State_Pubsub.Pub with null record;

   Node_State_Change_Indication : Node_State_Change_Publisher;

   package Natural_Pubsub is new ACO.Utils.Generic_Pubsub
      (Item_Type           => Natural,
       Max_Nof_Subscribers => 5);

   type Heartbeat_Consumer_Change_Publisher is
      new Natural_Pubsub.Pub with null record;

   Heartbeat_Consumer_Change_Indication : Heartbeat_Consumer_Change_Publisher;

   type Heartbeat_Producer_Change_Publisher is
      new Natural_Pubsub.Pub with null record;

   Heartbeat_Producer_Change_Indication : Heartbeat_Producer_Change_Publisher;

private

   type State_Array is array (Positive range <>) of ACO.States.State;

   type Object_Dict is tagged limited record
      Node_State : ACO.States.State := ACO.States.Unknown_State;
      Communication_Cycle_Period : Natural := 10_000; --  Multiples of 100us
      Sync_Counter_Overflow_Value : Sync_Counter := 16;
      Heartbeat_Producer_Period : Natural := 500;
      Slave_States : State_Array (1 .. Max_Nof_Heartbeat_Slaves) :=
         (others => ACO.States.Unknown_State);
   end record;

end ACO.OD;
