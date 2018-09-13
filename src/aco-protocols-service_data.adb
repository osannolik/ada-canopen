with Ada.Exceptions;

package body ACO.Protocols.Service_Data is

   use ACO.OD_Types;

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

   procedure Send_Abort
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type;
       Reason   : in     Abort_Type;
       Index    : in     Entry_Index := (0,0))
   is
      use Commands;
      Cmd : constant Abort_Cmd := Create_Abort (Index, Abort_Code (Reason));
   begin
      This.SDO_Log (ACO.Log.Warning, "Aborting: " & Reason'Img);
      This.Send_SDO_Response (Endpoint, Cmd.Raw);
   end Send_Abort;

   procedure Write
      (This    : in out SDO;
       Index   : in     Entry_Index;
       Data    : in     Data_Array;
       Success :    out Boolean)
   is
      --  TODO:
      --  Need a more efficient way to write large amount of data (Domain type)
      Ety : Entry_Base'Class := This.Od.Get_Entry (Index.Object, Index.Sub);
   begin
      Ety.Write (Byte_Array (Data));
      This.Od.Set_Entry (Ety, Index.Object, Index.Sub);
      Success := True;
   exception
      when E : others =>
         Success := False;
         This.SDO_Log (ACO.Log.Debug, Ada.Exceptions.Exception_Name (E));
   end Write;

   procedure Send_SDO_Response
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type;
       Raw_Data : in     Msg_Data)
   is
      Msg : constant Message :=
         Create (CAN_Id => Tx_CAN_Id (Endpoint),
                 RTR    => False,
                 DLC    => 8,
                 Data   => Raw_Data);
   begin
      This.SDO_Log (ACO.Log.Debug, "Sending " & Image (Msg));
      This.Driver.Send_Message (Msg);
   end Send_SDO_Response;

   procedure Server_Download_Init
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use Commands, Interfaces;

      Cmd     : constant Download_Initiate_Cmd := To_Download_Initiate_Cmd (Msg);
      Index   : constant Entry_Index := Get_Index (Msg);
      Success : Boolean := True;
   begin
      if not This.Od.Is_Entry_Writable (Index) then
         This.Send_Abort (Endpoint => Endpoint,
                          Reason   => Attempt_To_Write_A_Read_Only_Object,
                          Index    => Index);
         return;
      end if;

      if not Cmd.Is_Size_Indicated then
         This.Send_Abort (Endpoint => Endpoint,
                          Reason   => Command_Specifier_Not_Valid_Or_Unknown,
                          Index    => Index);
         return;
      end if;

      if Cmd.Is_Expedited then
         This.Write
            (Index   => Index,
             Data    => Cmd.Data (0 .. 3 - Natural (Cmd.Nof_No_Data)),
             Success => Success);
      else
         This.Sessions.Put ((Endpoint  => Endpoint,
                             State     => Downloading,
                             Nof_Bytes => Get_Data_Size (Cmd),
                             Count     => 0));
      end if;

      if Success then
         declare
            Resp : constant Download_Initiate_Resp := Create_Response (Index);
         begin
            This.Send_SDO_Response (Endpoint, Resp.Raw);
         end;
      else
         This.Send_Abort (Endpoint => Endpoint,
                          Reason   => Failed_To_Transfer_Or_Store_Data,
                          Index    => Index);
      end if;
   end Server_Download_Init;

   procedure Message_Received_For_Server
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use Commands;
   begin
      case Get_CS (Msg) is
         when Download_Initiate_Req =>
            if not This.Sessions.In_List (Endpoint) and then
               not This.Sessions.Is_Full
            then
               This.Server_Download_Init (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Reason   => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when others =>
            null;
      end case;
   end Message_Received_For_Server;

   procedure Message_Received_For_Client
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      pragma Unreferenced (This, Msg, Endpoint);
   begin
      null;
   end Message_Received_For_Client;

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
         Endpoint : constant Endpoint_Type := Get_Endpoint
            (CAN_Id            => CAN_Id (Msg),
             Client_CAN_Ids => This.Od.Get_SDO_Client_CAN_Ids,
             Server_CAN_Ids => This.Od.Get_SDO_Server_CAN_Ids);
      begin
         if Endpoint.Id /= No_Endpoint_Id then
            if Endpoint.Role = Server then
               This.Message_Received_For_Server (Msg, Endpoint);
            else
               This.Message_Received_For_Client (Msg, Endpoint);
            end if;
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
