with Ada.Exceptions;
with Ada.Real_Time;

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

   overriding
   procedure Signal (This : access Alarm)
   is
      Session : constant SDO_Session := This.SDO_Ref.Sessions.Get (This.Id);
   begin
      This.SDO_Ref.SDO_Log
         (ACO.Log.Info, "Session timed out for service " & Session.Service'Img);
      This.SDO_Ref.Send_Abort
         (Endpoint => Session.Endpoint,
          Error    => SDO_Protocol_Timed_Out,
          Index    => Session.Index);
      This.SDO_Ref.Sessions.Clear (This.Id);
   end Signal;

   procedure Start_Alarm
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type)
   is
      use Ada.Real_Time, ACO.Configuration;
      Timeout_Alarm : Alarm renames This.Alarms (Endpoint.Id);
   begin
      if This.Event_Manager.Is_Pending (Timeout_Alarm'Unchecked_Access) then
         This.Event_Manager.Cancel (Timeout_Alarm'Unchecked_Access);
      end if;

      Timeout_Alarm.Id := Endpoint.Id;
      This.Event_Manager.Set
         (Alarm       => Timeout_Alarm'Unchecked_Access,
          Signal_Time => Clock + Milliseconds (SDO_Session_Timeout_Ms));
   end Start_Alarm;

   procedure Stop_Alarm
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type)
   is
   begin
      This.Event_Manager.Cancel (This.Alarms (Endpoint.Id)'Unchecked_Access);
   end Stop_Alarm;

   procedure Send_Abort
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type;
       Error    : in     Error_Type;
       Index    : in     Entry_Index := (0,0))
   is
      use Commands;
      Cmd : constant Abort_Cmd := Create_Abort (Index, Abort_Code (Error));
   begin
      This.SDO_Log (ACO.Log.Warning, "Aborting: " & Error'Img);
      This.Send_SDO_Response (Endpoint, Cmd.Raw);
   end Send_Abort;

   procedure Write
      (This  : in out SDO;
       Index : in     Entry_Index;
       Data  : in     Data_Array;
       Error :    out Error_Type)
   is
      --  TODO:
      --  Need a more efficient way to write large amount of data (Domain type)
      Ety : Entry_Base'Class := This.Od.Get_Entry (Index.Object, Index.Sub);
   begin
      Ety.Write (Byte_Array (Data));
      This.Od.Set_Entry (Ety, Index.Object, Index.Sub);
      Error := Nothing;
   exception
      when E : others =>
         Error := Failed_To_Transfer_Or_Store_Data;
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
      use Commands;

      Cmd   : constant Download_Initiate_Cmd := To_Download_Initiate_Cmd (Msg);
      Index : constant Entry_Index := Get_Index (Msg);
      Error : Error_Type := Nothing;
   begin
      if not This.Od.Entry_Exist (Index.Object, Index.Sub) then
         Error := Object_Does_Not_Exist_In_The_Object_Dictionary;
      elsif not This.Od.Is_Entry_Writable (Index) then
         Error := Attempt_To_Write_A_Read_Only_Object;
      elsif not Cmd.Is_Size_Indicated then
         Error := Command_Specifier_Not_Valid_Or_Unknown;
      end if;

      if Error /= Nothing then
         This.Send_Abort (Endpoint, Error, Index);
         return;
      end if;

      if Cmd.Is_Expedited then
         This.Write
            (Index => Index,
             Data  => Cmd.Data (0 .. 3 - Natural (Cmd.Nof_No_Data)),
             Error => Error);
      else
         declare
            Session : constant SDO_Session :=
               Create_Download (Endpoint, Index, Get_Data_Size (Cmd));
         begin
            This.Sessions.Put (Session);
            This.Start_Alarm (Endpoint);
         end;
      end if;

      if Error = Nothing then
         declare
            Resp : constant Download_Initiate_Resp := Create_Response (Index);
         begin
            This.Send_SDO_Response (Endpoint, Resp.Raw);
         end;
      else
         This.Send_Abort (Endpoint, Error, Index);
      end if;
   end Server_Download_Init;

   procedure Server_Download_Segment
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use Commands;

      Cmd     : constant Download_Segment_Cmd := To_Download_Segment_Cmd (Msg);
      Session : SDO_Session := This.Sessions.Get (Endpoint.Id);
      Error   : Error_Type := Nothing;
   begin
      if Cmd.Toggle /= Session.Toggle then
         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Toggle_Bit_Not_Altered,
                          Index    => Session.Index);
         This.Sessions.Clear (Endpoint.Id);
         return;
      end if;

      This.Sessions.Buffer
         (Id   => Endpoint.Id,
          Data => Cmd.Data (0 .. 6 - Natural (Cmd.Nof_No_Data)));

      Session.Toggle := not Session.Toggle;

      if Cmd.Is_Complete then
         This.Write
            (Index => Session.Index,
             Data  => This.Sessions.Get_Buffer_Data (Endpoint.Id),
             Error => Error);

         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);
      else
         This.Sessions.Put (Session);
         This.Start_Alarm (Endpoint);
      end if;

      if Error = Nothing then
         declare
            Resp : constant Download_Segment_Resp := Create_Response (Cmd.Toggle);
         begin
            This.Send_SDO_Response (Endpoint, Resp.Raw);
         end;
      else
         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Failed_To_Transfer_Or_Store_Data,
                          Index    => Session.Index);
      end if;
   end Server_Download_Segment;

   procedure Message_Received_For_Server
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use Commands;
      Service : constant Services := This.Sessions.Service (Endpoint.Id);
   begin
      case Get_CS (Msg) is
         when Download_Initiate_Req =>
            if Service = None then
               This.SDO_Log (ACO.Log.Debug, "Server: Handling Download Initiate");
               This.Server_Download_Init (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Download_Segment_Req =>
            if Service = Download then
               This.SDO_Log (ACO.Log.Debug, "Server: Handling Download Segment");
               This.Server_Download_Segment (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
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
