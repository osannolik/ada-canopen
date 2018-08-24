with Ada.Finalization;
with ACO.Drivers;
with ACO.Messages;
with ACO.States;
with ACO.OD;

private with Ada.Synchronous_Task_Control;
private with ACO.Events;
private with ACO.Protocols.Network_Management;
private with ACO.Protocols.Error_Control;
private with ACO.Protocols.Synchronization;
private with ACO.Protocols.Service_Data;
private with ACO.Log;

package ACO.Nodes is
   use ACO.Messages;

   type Node
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class)
   is new Ada.Finalization.Limited_Controlled with private;

   procedure Set_State
     (This  : in out Node;
      State : in     ACO.States.State);

   procedure Dispatch
     (This : in out Node;
      Msg  : in     Message);

   task type Receiver_Task (This : not null access Node'Class);

   task type Periodic_Task (This : not null access Node'Class);

private
   use ACO.Protocols;

   type Node_State_Change_Subscriber (Node_Ref : not null access Node'Class) is
      new ACO.Events.Node_State_Pubsub.Sub with null record;

   overriding
   procedure Update
     (This : access Node_State_Change_Subscriber;
      Data : in     ACO.States.State_Transition);

   type Node
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class)
   is new Ada.Finalization.Limited_Controlled with record
      NMT  : Network_Management.NMT (Od);
      EC   : Error_Control.EC (Id, Od, Driver);
      SYNC : Synchronization.SYNC (Od, Driver);
      SDO  : Service_Data.SDO (Od, Driver);
      Node_State_Change_Indication : aliased Node_State_Change_Subscriber (Node'Access);
      Start_Receiver_Task : Ada.Synchronous_Task_Control.Suspension_Object;
      Start_Periodic_Task : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

   procedure Node_Log
     (This    : in out Node;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

   overriding
   procedure Initialize (This : in out Node);

   overriding
   procedure Finalize (This : in out Node);

   procedure Init
      (This : in out Node);

end ACO.Nodes;
