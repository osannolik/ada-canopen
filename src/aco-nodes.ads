with ACO.Drivers;
with ACO.Messages;
with ACO.States;
with ACO.OD;

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

   type Node
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class)
   is tagged limited record
      NMT : ACO.Protocols.Network_Management.NMT (Od, Driver);
   end record;

end ACO.Nodes;
