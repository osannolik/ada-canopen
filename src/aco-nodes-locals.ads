with ACO.Protocols.Service_Data.Servers;

private with ACO.Protocols.Network_Management;
private with ACO.Protocols.Error_Control;
private with ACO.Protocols.Synchronization;
private with ACO.Protocols.Service_Data;

package ACO.Nodes.Locals is

   type Local
      (Id     : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler'Class;
       Od     : not null access ACO.OD.Object_Dictionary'Class)
   is new Node_Base with private;

   overriding
   procedure Set_State
      (This  : in out Local;
       State : in     ACO.States.State);

   overriding
   procedure Write
      (This     : in out Local;
       Node     : in     ACO.Messages.Node_Nr;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class);

private

   procedure On_Message_Dispatch
      (This : in out Local;
       Msg  : in     ACO.Messages.Message);

   procedure Periodic_Actions
      (This  : in out Local;
       T_Now : in     Ada.Real_Time.Time);

   type Local
      (Id     : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler'Class;
       Od     : not null access ACO.OD.Object_Dictionary'Class)
   is new Node_Base (Id, Handler, Od) with record
      NMT  : ACO.Protocols.Network_Management.NMT (Od);
      EC   : ACO.Protocols.Error_Control.EC (Id, Handler, Od);
      SYNC : ACO.Protocols.Synchronization.SYNC (Handler, Od);
      SDO  : ACO.Protocols.Service_Data.Servers.Server (Handler, Od);
   end record;

end ACO.Nodes.Locals;
