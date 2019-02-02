with ACO.SDO_Sessions;

private with Ada.Synchronous_Task_Control;
private with ACO.Protocols.Network_Management.Masters;
private with ACO.Protocols.Error_Control.Slaves;
private with ACO.Protocols.Service_Data.Clients;

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
      (This : in out Remote);

   type SDO_Request
     (Node : not null access Remote)
   is tagged private;

   subtype SDO_Status is ACO.SDO_Sessions.SDO_Status;
   subtype SDO_Result is ACO.SDO_Sessions.SDO_Result;

   procedure Suspend_Until_Result
     (This   : in out SDO_Request;
      Result :    out SDO_Result);

   function Request_Status
     (This : SDO_Request)
      return SDO_Status;

   overriding
   procedure Write
      (This     : in out Remote;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class)
      with Pre => This.Od.Entry_Exist (Index, Subindex) and then
                  This.Od.Is_Entry_Compatible (An_Entry, Index, Subindex);

   procedure Write
      (This     : in out Remote;
       Request  : in out SDO_Request'Class;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class);

   procedure Set_Heartbeat_Timeout
      (This    : in out Remote;
       Timeout : in     Natural);

private

   type Request_Type is (Write, Read);

   type SDO_Request
     (Node : not null access Remote)
   is tagged record
      Id : ACO.SDO_Sessions.Endpoint_Nr := ACO.SDO_Sessions.No_Endpoint_Id;
   end record;

   type Request_Data is record
      Suspension  : Ada.Synchronous_Task_Control.Suspension_Object;
      Status : ACO.SDO_Sessions.SDO_Status := ACO.SDO_Sessions.Pending;
      Operation : Request_Type;
   end record;

   type SDO_Request_Array is array (ACO.SDO_Sessions.Valid_Endpoint_Nr)
      of Request_Data;

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
   with record
      Requests : SDO_Request_Array;
   end record;

   overriding
   procedure Result_Callback
     (This    : in out Remote_Client;
      Session : in     ACO.SDO_Sessions.SDO_Session;
      Result  : in     ACO.SDO_Sessions.SDO_Result);

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
