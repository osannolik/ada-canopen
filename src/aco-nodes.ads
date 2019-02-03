with Ada.Finalization;
with ACO.CANopen;
with ACO.States;
with ACO.Messages;
with ACO.OD;
with ACO.OD_Types;

private with Ada.Real_Time;
private with ACO.Events;

package ACO.Nodes is

   type Node_Base
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Ada.Finalization.Limited_Controlled with private;

   procedure Set_State
      (This  : in out Node_Base;
       State : in     ACO.States.State) is abstract;
   --  Local: Set state in OD, maybe send boot
   --  Remote: Set state in OD, send nmt command to node id of remote

   function Get_State
      (This  : Node_Base)
       return ACO.States.State is abstract;
   --  Local: Get state from OD
   --  Remote: Get state from OD

   procedure Start
      (This : in out Node_Base) is abstract;

   procedure Write
      (This     : in out Node_Base;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class) is abstract;

   procedure Read
      (This     : in out Node_Base;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       To_Entry :    out ACO.OD_Types.Entry_Base'Class) is abstract;

private

   procedure On_Message_Dispatch
      (This : in out Node_Base;
       Msg  : in     ACO.Messages.Message) is null;

   procedure Periodic_Actions
      (This  : in out Node_Base;
       T_Now : in     Ada.Real_Time.Time) is null;


   type Tick_Subscriber
      (Node_Ref : not null access Node_Base'Class)
   is new ACO.Events.Periodic_Tick.Subscriber with null record;

   overriding
   procedure Update
      (This : access Tick_Subscriber;
       Data : in     Ada.Real_Time.Time);

   type Message_Subscriber
      (Node_Ref : not null access Node_Base'Class)
   is new ACO.Events.New_Message.Subscriber with null record;

   overriding
   procedure Update
      (This : access Message_Subscriber;
       Data : in     ACO.Messages.Message);

   type Node_Base
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Ada.Finalization.Limited_Controlled with record
      Periodic_Action_Indication : aliased Tick_Subscriber (Node_Base'Access);
      New_Message_Indication : aliased Message_Subscriber (Node_Base'Access);
   end record;

   overriding
   procedure Initialize (This : in out Node_Base);

   overriding
   procedure Finalize (This : in out Node_Base);

end ACO.Nodes;
