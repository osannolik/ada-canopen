package body ACO.Nodes.Locals is

   overriding
   procedure Set_State
      (This  : in out Local;
       State : in     ACO.States.State)
   is
   begin
      This.NMT.Set_State (State);
   end Set_State;

   overriding
   procedure Write
      (This     : in out Local;
       Node     : in     ACO.Messages.Node_Nr;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class)
   is
      pragma Unreferenced (This, Node, Index, Subindex, An_Entry);
   begin
      --  Write to local Od
      null;
   end Write;

   procedure On_Message_Dispatch
      (This : in out Local;
       Msg  : in     ACO.Messages.Message)
   is
      use type ACO.Messages.Id_Type;
      use type ACO.Messages.Function_Code;

      Func : constant ACO.Messages.Function_Code :=
         ACO.Messages.Func_Code (Msg);
   begin
      if ACO.Messages.CAN_Id (Msg) =
         ACO.Protocols.Network_Management.NMT_CAN_Id
      then
         This.NMT.Message_Received (Msg, This.Id);
      elsif ACO.Messages.CAN_Id (Msg) =
         ACO.Protocols.Synchronization.SYNC_CAN_Id
      then
         This.SYNC.Message_Received (Msg);
      elsif Func = ACO.Protocols.Error_Control.EC_Id then
         This.EC.Message_Received (Msg);
      elsif Func = ACO.Protocols.Service_Data.SDO_C2S_Id or
         Func = ACO.Protocols.Service_Data.SDO_S2C_Id
      then
         This.SDO.Message_Received (Msg);
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
