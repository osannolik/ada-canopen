with ACO.Drivers;
with ACO.Messages;
with ACO.States;
with ACO.OD;

private with Ada.Synchronous_Task_Control;
private with ACO.Protocols.Network_Management;

package ACO.Nodes is
   use ACO.Messages;

   type Node
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class)
   is tagged limited private;

   procedure Initialize (This : in out Node);

   procedure Set_State
     (This  : in out Node;
      State : in     ACO.States.State);

   procedure Dispatch
     (This : in out Node;
      Msg  : in     Message);

   task type Receiver_Task (This : not null access Node'Class);

private

   type Node_State_Change_Subscriber (Node_Ref : not null access Node'Class) is
      new ACO.OD.Node_State_Pubsub.Sub with null record;

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.States.State);

   type Node
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class)
   is tagged limited record
      NMT : ACO.Protocols.Network_Management.NMT (Od, Driver);
      Node_State_Change_Indication : aliased Node_State_Change_Subscriber (Node'Access);
      Start_Receiver_Task : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end ACO.Nodes;
