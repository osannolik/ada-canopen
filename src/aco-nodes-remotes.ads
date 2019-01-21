private with ACO.Protocols.Network_Management.Masters;
private with ACO.Protocols.Error_Control.Slaves;
private with ACO.Protocols.Service_Data.Clients;
private with ACO.SDO_Sessions;

package ACO.Nodes.Remotes is

   type Remote
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new Node_Base with private;

   overriding
   procedure Set_State
      (This  : in out Remote;
       State : in     ACO.States.State);

   overriding
   function Get_State
      (This  : Remote)
       return ACO.States.State;

   overriding
   procedure Start
      (This : in out Remote) is null;

   overriding
   procedure Write
      (This     : in out Remote;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class);

   procedure Set_Heartbeat_Timeout
      (This    : in out Remote;
       Timeout : in     Natural);

private

   procedure On_Message_Dispatch
      (This : in out Remote;
       Msg  : in     ACO.Messages.Message);

   procedure Periodic_Actions
      (This  : in out Remote;
       T_Now : in     Ada.Real_Time.Time);

   type Remote_Client
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new ACO.Protocols.Service_Data.Clients.Client (Handler, Od)
   with null record;

   overriding
   function Tx_CAN_Id
      (This      : Remote_Client;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type;

   overriding
   function Rx_CAN_Id
      (This      : Remote_Client;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type;

   overriding
   function Get_Endpoint
      (This      : Remote_Client;
       Rx_CAN_Id : ACO.Messages.Id_Type)
       return ACO.SDO_Sessions.Endpoint_Type;

   overriding
   function Get_Endpoint
      (This        : Remote_Client;
       Server_Node : ACO.Messages.Node_Nr)
       return ACO.SDO_Sessions.Endpoint_Type;

   type Remote
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new Node_Base (Id, Handler, Od) with record
      NMT : ACO.Protocols.Network_Management.Masters.Master (Id, Handler, Od);
      EC  : ACO.Protocols.Error_Control.Slaves.Slave (Id, Od);
      SDO : Remote_Client (Id, Handler, Od);
   end record;

end ACO.Nodes.Remotes;
