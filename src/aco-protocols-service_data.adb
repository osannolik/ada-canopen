package body ACO.Protocols.Service_Data is

   overriding
   procedure On_State_Change
     (This     : in out SDO;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State)
   is
      pragma Unreferenced (This, Previous, Current);
   begin
      null;
   end On_State_Change;

   procedure Server_Download_Init
      (This : in out SDO;
       Msg  : in     Message)
   is
      use Commands, Interfaces;
      Cmd : constant Download_Initiate_Cmd := To_Download_Initiate_Cmd (Msg);
      X : constant Interfaces.Unsigned_16 :=
         ACO.Utils.Byte_Order.Swap_Bus (Cmd.Index);
      Y : constant ACO.OD_Types.Entry_Index := Commands.Get_Index (Msg);
      pragma Unreferenced (This, Cmd, X, Y);
   begin
      null;
   end Server_Download_Init;

   procedure Message_Received_For_Server
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Nr)
   is
      use Commands;
      pragma Unreferenced (Endpoint);
   begin
      case Get_CS (Msg) is
         when Download_Initiate_Req =>
            This.Server_Download_Init (Msg);

         when others =>
            null;
      end case;
   end Message_Received_For_Server;

   procedure Message_Received_For_Client
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Nr)
   is
      pragma Unreferenced (This, Msg, Endpoint);
   begin
      null;
   end Message_Received_For_Client;

   function Get_Endpoint
      (Id         : Id_Type;
       Rx_CAN_Ids : Id_Array)
       return Endpoint_Nr
   is
   begin
      for Nr in Rx_CAN_Ids'Range loop
         if Rx_CAN_Ids (Nr) = Id then
            return Nr;
         end if;
      end loop;
      return No_Endpoint;
   end Get_Endpoint;

   procedure Message_Received
     (This : in out SDO;
      Msg  : in     Message)
   is
      use ACO.States;
   begin
      case This.Od.Get_Node_State is
         when Initializing | Unknown_State | Stopped =>
            return;

         when Pre_Operational | Operational =>
            null;
      end case;

      declare
         Server_Endpoint : constant Endpoint_Nr :=
            Get_Endpoint (CAN_Id (Msg), This.Od.Get_SDO_Server_Rx_CAN_Ids);
      begin
         if Server_Endpoint /= No_Endpoint then
            This.Message_Received_For_Server (Msg, Server_Endpoint);

            return;
         end if;
      end;

      declare
         Client_Endpoint : constant Endpoint_Nr :=
            Get_Endpoint (CAN_Id (Msg), This.Od.Get_SDO_Client_Rx_CAN_Ids);
      begin
         if Client_Endpoint /= No_Endpoint then
            This.Message_Received_For_Client (Msg, Client_Endpoint);

            return;
         end if;
      end;
   end Message_Received;

   procedure Periodic_Actions
     (This : in out SDO)
   is
   begin
      This.Event_Manager.Process;
   end Periodic_Actions;

   overriding
   procedure Initialize (This : in out SDO)
   is
   begin
      Protocol (This).Initialize;
   end Initialize;

   overriding
   procedure Finalize (This : in out SDO)
   is
   begin
      Protocol (This).Finalize;
   end Finalize;

   procedure SDO_Log
     (This    : in out SDO;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
      pragma Unreferenced (This);
   begin
      ACO.Log.Put_Line (Level, "(SDO) " & Message);
   end SDO_Log;

end ACO.Protocols.Service_Data;
