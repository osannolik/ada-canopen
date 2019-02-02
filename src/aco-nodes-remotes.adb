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
   procedure Start
      (This : in out Remote)
   is
   begin
      This.Handler.Start;
   end Start;

   procedure Suspend_Until_Result
     (This   : in out SDO_Request;
      Result :    out SDO_Result)
   is
   begin
      if This.Id in ACO.SDO_Sessions.Valid_Endpoint_Nr then
         declare
            Request : Request_Data renames This.Node.SDO.Requests (This.Id);
         begin
            Ada.Synchronous_Task_Control.Suspend_Until_True (Request.Suspension);
            Result := Request.Status;
         end;
      else
         Result := ACO.SDO_Sessions.Error;
      end if;
   end Suspend_Until_Result;

   function Request_Status
     (This : SDO_Request)
      return SDO_Status
   is
   begin
      if This.Id in ACO.SDO_Sessions.Valid_Endpoint_Nr then
         return This.Node.SDO.Requests (This.Id).Status;
      else
         return ACO.SDO_Sessions.Error;
      end if;
   end Request_Status;

   procedure Write
      (This     : in out Remote;
       Request  : in out SDO_Request'Class;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class)
   is
   begin
      This.SDO.Write_Remote_Entry
         (Node        => This.Id,
          Index       => Index,
          Subindex    => Subindex,
          An_Entry    => An_Entry,
          Endpoint_Id => Request.Id);

      if Request.Id in ACO.SDO_Sessions.Valid_Endpoint_Nr'Range then
         declare
            Req_Data : Request_Data renames This.SDO.Requests (Request.Id);
         begin
            Req_Data.Status := ACO.SDO_Sessions.Pending;
            Req_Data.Operation := Write;
            Ada.Synchronous_Task_Control.Set_False (Req_Data.Suspension);
         end;
      end if;
   end Write;

   overriding
   procedure Write
      (This       : in out Remote;
       Index      : in     ACO.OD_Types.Object_Index;
       Subindex   : in     ACO.OD_Types.Object_Subindex;
       An_Entry   : in     ACO.OD_Types.Entry_Base'Class)
   is
      pragma Unreferenced (This, Index, Subindex, An_Entry);
   begin
      null;
   end Write;

--     procedure Write
--        (This       : in out Remote;
--         Index      : in     ACO.OD_Types.Object_Index;
--         Subindex   : in     ACO.OD_Types.Object_Subindex;
--         An_Entry   : in     ACO.OD_Types.Entry_Base'Class)
--     is
--        use type ACO.SDO_Sessions.SDO_Status;
--        use type Ada.Real_Time.Time;
--
--        Status : ACO.SDO_Sessions.SDO_Status;
--        Endpoint_Id : ACO.SDO_Sessions.Endpoint_Nr;
--     begin
--        This.SDO.Write_Remote_Entry
--           (Node        => This.Id,
--            Index       => Index,
--            Subindex    => Subindex,
--            An_Entry    => An_Entry,
--            Endpoint_Id => Endpoint_Id);
--
--
--        if Timeout_Ms > 0 then
--           delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (Timeout_Ms);
--        end if;
--
--
--
--
--
--        if Endpoint_Id in ACO.SDO_Sessions.Valid_Endpoint_Nr then
--           loop
--              This.Handler.Periodic_Actions (T_Now => Ada.Real_Time.Clock);
--
--              Status := This.SDO.Get_Status (Endpoint_Id);
--
--              exit when Status /= ACO.SDO_Sessions.Pending;
--           end loop;
--           This.SDO.Clear (Endpoint_Id);
--
--           Success := (Status = ACO.SDO_Sessions.Complete);
--        else
--           Success := False;
--        end if;
--     end Write;

--     overriding
--     function Read
--        (This     : Remote;
--         Index    : ACO.OD_Types.Object_Index;
--         Subindex : ACO.OD_Types.Object_Subindex)
--         return ACO.OD_Types.Entry_Base'Class
--     is
--        pragma Unreferenced (This, Index, Subindex);
--     begin
--        null;
--     end Read;


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
   procedure Result_Callback
     (This    : in out Remote_Client;
      Session : in     ACO.SDO_Sessions.SDO_Session;
      Result  : in     ACO.SDO_Sessions.SDO_Result)
   is
      Request : Request_Data renames This.Requests (Session.Endpoint.Id);
   begin
      This.Od.Events.SDO_Status_Update.Put
        ((Endpoint_Id => Session.Endpoint.Id,
          Result      => Result));

      Request.Status := Result;

      case Request.Operation is
         when Write =>
            This.Clear (Session.Endpoint.Id);
            Ada.Synchronous_Task_Control.Set_True (Request.Suspension);

         when Read =>
            null;

      end case;
   end Result_Callback;

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
            return (Id => I, Parameters => P);
         end if;
         I := ACO.SDO_Sessions.Endpoint_Nr'Succ (I);
      end loop;

      return ACO.SDO_Sessions.No_Endpoint;
   end Get_Endpoint;

end ACO.Nodes.Remotes;
