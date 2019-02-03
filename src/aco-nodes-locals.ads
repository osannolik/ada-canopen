private with ACO.Protocols.Network_Management.Slaves;
private with ACO.Protocols.Error_Control.Masters;
private with ACO.Protocols.Synchronization;
private with ACO.Protocols.Service_Data.Servers;
private with ACO.SDO_Sessions;

package ACO.Nodes.Locals is

   type Local
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new Node_Base with private;

   overriding
   procedure Set_State
      (This  : in out Local;
       State : in     ACO.States.State);

   overriding
   function Get_State
      (This  : Local)
       return ACO.States.State;

   overriding
   procedure Start
      (This : in out Local);

   overriding
   procedure Write
      (This     : in out Local;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class)
      with Pre => This.Od.Entry_Exist (Index, Subindex) and then
                  This.Od.Is_Entry_Compatible (An_Entry, Index, Subindex);

   overriding
   procedure Read
      (This     : in out Local;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       To_Entry :    out ACO.OD_Types.Entry_Base'Class)
      with Pre => This.Od.Entry_Exist (Index, Subindex) and then
                  This.Od.Is_Entry_Compatible (To_Entry, Index, Subindex);

   function Read
      (This     : Local;
       Index    : ACO.OD_Types.Object_Index;
       Subindex : ACO.OD_Types.Object_Subindex)
       return ACO.OD_Types.Entry_Base'Class
      with Pre => This.Od.Entry_Exist (Index, Subindex);

private

   procedure On_Message_Dispatch
      (This : in out Local;
       Msg  : in     ACO.Messages.Message);

   procedure Periodic_Actions
      (This  : in out Local;
       T_Now : in     Ada.Real_Time.Time);

   type Server_Only
      (Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new ACO.Protocols.Service_Data.Servers.Server (Handler, Od)
   with null record;

   overriding
   procedure Result_Callback
     (This    : in out Server_Only;
      Session : in     ACO.SDO_Sessions.SDO_Session;
      Result  : in     ACO.SDO_Sessions.SDO_Result);

   overriding
   function Tx_CAN_Id
      (This      : Server_Only;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type;

   overriding
   function Rx_CAN_Id
      (This      : Server_Only;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type;

   overriding
   function Get_Endpoint
      (This      : Server_Only;
       Rx_CAN_Id : ACO.Messages.Id_Type)
       return ACO.SDO_Sessions.Endpoint_Type;

   type Local
      (Id      : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new Node_Base (Id, Handler, Od) with record
      NMT  : ACO.Protocols.Network_Management.Slaves.Slave (Id, Od);
      EC   : ACO.Protocols.Error_Control.Masters.Master (Id, Handler, Od);
      SDO  : Server_Only (Handler, Od);
      SYNC : ACO.Protocols.Synchronization.SYNC (Handler, Od);
   end record;

end ACO.Nodes.Locals;
