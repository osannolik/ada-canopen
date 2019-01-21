with ACO.Protocols.Service_Data;

package body ACO.Nodes.Remotes is

   overriding
   procedure Set_State
      (This  : in out Remote;
       State : in     ACO.States.State)
   is
   begin
      This.NMT.Request_State (State);

      --  If there is no heartbeat or node guarding, just assume the requested
      --  state is correct...
      if This.Od.Get_Heartbeat_Producer_Period = 0 then
         This.NMT.Set (State);
      end if;
   end Set_State;

   overriding
   function Get_State
      (This  : Remote)
       return ACO.States.State
   is
   begin
      return This.NMT.Get;
   end Get_State;

   overriding
   procedure Write
      (This     : in out Remote;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class)
   is
      pragma Unreferenced (This, Index, Subindex, An_Entry);
   begin
      null;
   end Write;

   procedure Set_Heartbeat_Timeout
      (This    : in out Remote;
       Timeout : in     Natural)
   is
   begin
      This.NMT.Set_Heartbeat_Timeout (Timeout);
   end Set_Heartbeat_Timeout;

   procedure On_Message_Dispatch
      (This : in out Remote;
       Msg  : in     ACO.Messages.Message)
   is
   begin
      if This.NMT.Is_Valid (Msg) then
         This.NMT.Message_Received (Msg);
      elsif This.EC.Is_Valid (Msg) then
         This.EC.Message_Received (Msg);
      elsif This.SDO.Is_Valid (Msg) then
         This.SDO.Message_Received (Msg);
      end if;
   end On_Message_Dispatch;

   procedure Periodic_Actions
      (This  : in out Remote;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.NMT.Periodic_Actions (T_Now);
      This.SDO.Periodic_Actions (T_Now);
   end Periodic_Actions;

   overriding
   function Tx_CAN_Id
      (This      : Remote_Client;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type
   is
      (Parameter.CAN_Id_C2S);

   overriding
   function Rx_CAN_Id
      (This      : Remote_Client;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type
   is
      (Parameter.CAN_Id_S2C);

   overriding
   function Get_Endpoint
      (This      : Remote_Client;
       Rx_CAN_Id : ACO.Messages.Id_Type)
       return ACO.SDO_Sessions.Endpoint_Type
   is
      use type ACO.Messages.Id_Type;
      --  Clients always initiate a session and a remote client always use the
      --  default mandatory Id's, meaning we should make sure we use the same
      --  endpoint as when we sent the initial request...
      Endpoint : constant ACO.SDO_Sessions.Endpoint_Type :=
         This.Get_Endpoint (Server_Node => This.Id);
   begin
      if This.Rx_CAN_Id (Endpoint.Parameters) = Rx_CAN_Id then
         return Endpoint;
      else
         return ACO.SDO_Sessions.No_Endpoint;
      end if;
   end Get_Endpoint;

   overriding
   function Get_Endpoint
      (This        : Remote_Client;
       Server_Node : ACO.Messages.Node_Nr)
       return ACO.SDO_Sessions.Endpoint_Type
   is
      use type ACO.Messages.Id_Type;
      --  As a remote client we only know the OD of the server, therefore we can
      --  use any of the C2S-Id's of the server.
      --  But the mandatory Server-Rx-Id is a good choice...
      Tx_CAN_Id : constant ACO.Messages.CAN_Id_Type :=
         (As_Id => False,
          Code  => ACO.Protocols.Service_Data.SDO_C2S_Id,
          Node  => Server_Node);
      I : ACO.SDO_Sessions.Endpoint_Nr :=
         ACO.SDO_Sessions.Valid_Endpoint_Nr'First;
   begin
      for P of This.Od.Get_SDO_Server_Parameters loop
         if This.Tx_CAN_Id (P) = Tx_CAN_Id.Id then
            return (Id => I, Role => ACO.SDO_Sessions.Client, Parameters => P);
         end if;
         I := ACO.SDO_Sessions.Endpoint_Nr'Succ (I);
      end loop;

      return ACO.SDO_Sessions.No_Endpoint;
   end Get_Endpoint;

end ACO.Nodes.Remotes;
