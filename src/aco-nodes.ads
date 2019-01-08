with Ada.Finalization;
with ACO.CANopen;
with ACO.States;
with ACO.Messages;
with ACO.OD;
with ACO.OD_Types;
with Ada.Real_Time;

private with ACO.Events;
private with ACO.Protocols.Network_Management;
private with ACO.Protocols.Error_Control;
private with ACO.Protocols.Synchronization;
private with ACO.Protocols.Service_Data;

package ACO.Nodes is

   type Node_Base
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler'Class;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Ada.Finalization.Limited_Controlled with private;

   procedure Set_State
      (This  : in out Node_Base;
       State : in     ACO.States.State) is abstract;

   procedure Write
      (This     : in out Node_Base;
       Node     : in     ACO.Messages.Node_Nr;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class) is abstract;

private

   procedure On_Message_Dispatch
      (This : in out Node_Base;
       Msg  : in     ACO.Messages.Message);

   procedure Periodic_Actions
      (This  : in out Node_Base;
       T_Now : in     Ada.Real_Time.Time);


   type Tick_Subscriber
      (Node_Ref : not null access Node_Base)
   is new ACO.Events.Periodic_Tick.Subscriber with null record;

   overriding
   procedure Update
      (This : access Tick_Subscriber;
       Data : in     Ada.Real_Time.Time);

   type Message_Subscriber
      (Node_Ref : not null access Node_Base)
   is new ACO.Events.New_Message.Subscriber with null record;

   overriding
   procedure Update
      (This : access Message_Subscriber;
       Data : in     ACO.Messages.Message);

   type Node_Base
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler'Class;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Ada.Finalization.Limited_Controlled with record
      NMT  : ACO.Protocols.Network_Management.NMT (Od);
      EC   : ACO.Protocols.Error_Control.EC (Id, Handler, Od);
      SYNC : ACO.Protocols.Synchronization.SYNC (Handler, Od);
      SDO  : ACO.Protocols.Service_Data.SDO (Handler, Od);
      Periodic_Action_Indication : aliased Tick_Subscriber (Node_Base'Access);
      New_Message_Indication : aliased Message_Subscriber (Node_Base'Access);
   end record;

   overriding
   procedure Initialize (This : in out Node_Base);

   overriding
   procedure Finalize (This : in out Node_Base);

end ACO.Nodes;
