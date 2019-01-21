package body ACO.Nodes.Locals is

   overriding
   procedure Set_State
      (This  : in out Local;
       State : in     ACO.States.State)
   is
   begin
      This.NMT.Set (State);
   end Set_State;

   overriding
   function Get_State
      (This  : Local)
       return ACO.States.State
   is
   begin
      return This.NMT.Get;
   end Get_State;

   overriding
   procedure Start
      (This : in out Local)
   is
   begin
      This.Set_State (ACO.States.Initializing);
   end Start;

   overriding
   procedure Write
      (This     : in out Local;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class)
   is
   begin
      This.Od.Set_Entry
         (New_Entry => An_Entry,
          Index     => Index,
          Subindex  => Subindex);
   end Write;

   procedure On_Message_Dispatch
      (This : in out Local;
       Msg  : in     ACO.Messages.Message)
   is
   begin
      if This.NMT.Is_Valid (Msg) then
         This.NMT.Message_Received (Msg);
      elsif This.EC.Is_Valid (Msg) then
         This.EC.Message_Received (Msg);
      elsif This.SDO.Is_Valid (Msg) then
         This.SDO.Message_Received (Msg);
      elsif This.SYNC.Is_Valid (Msg) then
         This.SYNC.Message_Received (Msg);
      end if;
   end On_Message_Dispatch;

   procedure Periodic_Actions
      (This  : in out Local;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.EC.Periodic_Actions (T_Now);
      This.SDO.Periodic_Actions (T_Now);
      This.SYNC.Periodic_Actions (T_Now);
   end Periodic_Actions;

   overriding
   function Tx_CAN_Id
      (This      : Server_Only;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type
   is
      (Parameter.CAN_Id_S2C);

   overriding
   function Rx_CAN_Id
      (This      : Server_Only;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type
   is
      (Parameter.CAN_Id_C2S);

   overriding
   function Get_Endpoint
      (This      : Server_Only;
       Rx_CAN_Id : ACO.Messages.Id_Type)
       return ACO.SDO_Sessions.Endpoint_Type
   is
      use type ACO.Messages.Id_Type;

      I : ACO.SDO_Sessions.Endpoint_Nr :=
         ACO.SDO_Sessions.Valid_Endpoint_Nr'First;
      Parameters : constant ACO.SDO_Sessions.SDO_Parameter_Array :=
         This.Od.Get_SDO_Server_Parameters;
   begin
      for P of Parameters loop
         if This.Rx_CAN_Id (P) = Rx_CAN_Id then
            return (Id => I, Role => ACO.SDO_Sessions.Server, Parameters => P);
         end if;
         I := ACO.SDO_Sessions.Endpoint_Nr'Succ (I);
      end loop;

      return ACO.SDO_Sessions.No_Endpoint;
   end Get_Endpoint;


--     procedure Write_Blocking
--        (This     : in out Node;
--         Node     : in     ACO.Messages.Node_Nr;
--         Index    : in     ACO.OD_Types.Object_Index;
--         Subindex : in     ACO.OD_Types.Object_Subindex;
--         An_Entry : in     ACO.OD_Types.Entry_Base'Class;
--         Success  :    out Boolean)
--     is
--        use type ACO.SDO_Sessions.SDO_Status;
--
--        E_Id : ACO.SDO_Sessions.Endpoint_Nr;
--        Status : ACO.SDO_Sessions.SDO_Status;
--     begin
--        This.SDO.Write_Remote_Entry
--           (Node        => Node,
--            Index       => Index,
--            Subindex    => Subindex,
--            An_Entry    => An_Entry,
--            Endpoint_Id => E_Id);
--
--        if E_Id in ACO.SDO_Sessions.Valid_Endpoint_Nr then
--           loop
--              This.Get_Received_Messages (Block => False);
--              This.Periodic_Actions (T_Now => Ada.Real_Time.Clock);
--
--              Status := This.SDO.Get_Status (E_Id);
--
--              exit when Status /= ACO.SDO_Sessions.Pending;
--           end loop;
--           This.SDO.Clear (E_Id);
--
--           Success := (Status = ACO.SDO_Sessions.Complete);
--        else
--           Success := False;
--        end if;
--     end Write_Blocking;
end ACO.Nodes.Locals;
