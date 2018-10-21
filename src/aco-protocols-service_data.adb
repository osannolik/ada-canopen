with Ada.Exceptions;
with Interfaces;

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

   overriding
   procedure Signal
      (This  : access Alarm;
       T_Now : in     Ada.Real_Time.Time)
   is
      pragma Unreferenced (T_Now);

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
      if This.Timeouts.Is_Pending (Timeout_Alarm'Unchecked_Access) then
         This.Timeouts.Cancel (Timeout_Alarm'Unchecked_Access);
      end if;

      Timeout_Alarm.Id := Endpoint.Id;
      This.Timeouts.Set
         (Alarm       => Timeout_Alarm'Unchecked_Access,
          Signal_Time => Clock + Milliseconds (SDO_Session_Timeout_Ms));
   end Start_Alarm;

   procedure Stop_Alarm
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type)
   is
   begin
      This.Timeouts.Cancel (This.Alarms (Endpoint.Id)'Unchecked_Access);
   end Stop_Alarm;

   procedure Send_Abort
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type;
       Error    : in     Error_Type;
       Index    : in     Entry_Index := (0,0))
   is
      use ACO.SDO_Commands;

      Cmd : constant Abort_Cmd := Create (Index, Abort_Code (Error));
   begin
      This.SDO_Log (ACO.Log.Warning, "Aborting: " & Error'Img);
      This.Send_SDO (Endpoint, Cmd.Raw);
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

   procedure Send_SDO
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
   end Send_SDO;

   procedure Server_Download_Init
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Cmd   : constant Download_Initiate_Cmd := Convert (Msg);
      Index : constant Entry_Index := Get_Index (Msg);
      Error : Error_Type := Nothing;
   begin
      if not This.Od.Entry_Exist (Index.Object, Index.Sub) then
         Error := Object_Does_Not_Exist_In_The_Object_Dictionary;
      elsif not This.Od.Is_Entry_Writable (Index) then
         Error := Attempt_To_Write_A_Read_Only_Object;
      elsif not Cmd.Is_Size_Indicated then
         Error := Command_Specifier_Not_Valid_Or_Unknown;
      elsif Get_Data_Size (Cmd) > ACO.Configuration.Max_Data_SDO_Transfer_Size then
         Error := General_Error;
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
               Create_Download (Endpoint, Index);
         begin
            This.Sessions.Put (Session);
            This.Start_Alarm (Endpoint);
         end;
      end if;

      if Error = Nothing then
         declare
            Resp : constant Download_Initiate_Resp := Create (Index);
         begin
            This.Send_SDO (Endpoint, Resp.Raw);
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
      use ACO.SDO_Commands;

      Cmd     : constant Download_Segment_Cmd := Convert (Msg);
      Session : SDO_Session := This.Sessions.Get (Endpoint.Id);
      Error   : Error_Type := Nothing;
   begin
      if Cmd.Toggle /= Session.Toggle then
         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Toggle_Bit_Not_Altered,
                          Index    => Session.Index);
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);
         return;
      end if;

      This.Sessions.Put_Buffer
         (Id   => Endpoint.Id,
          Data => Cmd.Data (0 .. 6 - Natural (Cmd.Nof_No_Data)));

      Session.Toggle := not Session.Toggle;

      if Cmd.Is_Complete then
         This.Write
            (Index => Session.Index,
             Data  => This.Sessions.Peek_Buffer (Endpoint.Id),
             Error => Error);

         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);
      else
         This.Sessions.Put (Session);
         This.Start_Alarm (Endpoint);
      end if;

      if Error = Nothing then
         declare
            Resp : constant Download_Segment_Resp := Create (Cmd.Toggle);
         begin
            This.Send_SDO (Endpoint, Resp.Raw);
         end;
      else
         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Failed_To_Transfer_Or_Store_Data,
                          Index    => Session.Index);
      end if;
   end Server_Download_Segment;

   procedure Server_Upload_Init
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Index : constant Entry_Index := Get_Index (Msg);
      Error : Error_Type := Nothing;
   begin
      if not This.Od.Entry_Exist (Index.Object, Index.Sub) then
         Error := Object_Does_Not_Exist_In_The_Object_Dictionary;
      elsif not This.Od.Is_Entry_Readable (Index) then
         Error := Attempt_To_Read_A_Write_Only_Object;
      end if;

      if Error /= Nothing then
         This.Send_Abort (Endpoint, Error, Index);
         return;
      end if;

      declare
         Ety  : constant Entry_Base'Class := This.Od.Get_Entry (Index.Object, Index.Sub);
         Size : Natural;
         Resp : Upload_Initiate_Resp;
      begin
         Size := Ety.Data_Length;

         if Size > ACO.Configuration.Max_Data_SDO_Transfer_Size then
            This.Send_Abort (Endpoint, General_Error, Index);
            return;
         end if;

         if Size <= Expedited_Data'Length then
            Resp := Create (Index, Data_Array (Ety.Read));
         else
            Resp := Create (Index, Size);

            This.Sessions.Clear_Buffer (Endpoint.Id);

            This.Sessions.Put_Buffer (Endpoint.Id, Data_Array (Ety.Read));

            This.Sessions.Put (Create_Upload (Endpoint, Index));

            This.Start_Alarm (Endpoint);
         end if;

         This.Send_SDO (Endpoint, Resp.Raw);
      end;
   end Server_Upload_Init;

   procedure Server_Upload_Segment
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Cmd          : constant Upload_Segment_Cmd := Convert (Msg);
      Bytes_Remain : constant Natural := This.Sessions.Length_Buffer (Endpoint.Id);
      Session      : SDO_Session;
      Error        : Error_Type := Nothing;
   begin
      Session := This.Sessions.Get (Endpoint.Id);

      if Cmd.Toggle /= Session.Toggle then
         Error := Toggle_Bit_Not_Altered;
      elsif Bytes_Remain = 0 then
         Error := General_Error;
      end if;

      if Error /= Nothing then
         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Error,
                          Index    => Session.Index);
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);

         return;
      end if;

      declare
         Bytes_To_Send : constant Positive :=
            Natural'Min (Bytes_Remain, Segment_Data'Length);
         Data : Data_Array (0 .. Bytes_To_Send - 1);
         Resp : Upload_Segment_Resp;
      begin
         Session.Is_Complete := (Bytes_To_Send = Bytes_Remain);

         This.Sessions.Get_Buffer (Endpoint.Id, Data);
         Resp := Create (Toggle      => Session.Toggle,
                         Is_Complete => Session.Is_Complete,
                         Data        => Data);
         This.Send_SDO (Endpoint => Endpoint,
                        Raw_Data => Resp.Raw);
         This.SDO_Log
            (ACO.Log.Debug, "Server: Sent data segment of length" & Bytes_To_Send'Img);
      end;

      if Session.Is_Complete then
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);
      else
         This.Start_Alarm (Endpoint);

         Session.Toggle := not Session.Toggle;

         This.Sessions.Put (Session);
      end if;
   end Server_Upload_Segment;

   procedure Message_Received_For_Server
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Service : constant Services := This.Sessions.Service (Endpoint.Id);
   begin
      case Get_CS (Msg) is
         when Download_Initiate_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Download Initiate");
            if Service = None then
               This.Server_Download_Init (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Download_Segment_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Download Segment");
            if Service = Download then
               This.Server_Download_Segment (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Upload_Initiate_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Upload Initiate");
            if Service = None then
               This.Server_Upload_Init (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Upload_Segment_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Upload Segment");
            if Service = Upload then
               This.Server_Upload_Segment (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Abort_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Abort");
            This.Abort_All (Msg, Endpoint);

         when others =>
            null;
      end case;
   end Message_Received_For_Server;

   procedure Client_Send_Data_Segment
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type;
       Toggle   : in     Boolean)
   is
      use ACO.SDO_Commands;

      Bytes_Remain : constant Natural := This.Sessions.Length_Buffer (Endpoint.Id);
   begin
      if Bytes_Remain = 0 then
         return;
      end if;

      declare
         Bytes_To_Send : constant Positive :=
            Natural'Min (Bytes_Remain, Segment_Data'Length);
         Data : Data_Array (0 .. Bytes_To_Send - 1);
         Cmd : Download_Segment_Cmd;
      begin
         This.Sessions.Get_Buffer (Endpoint.Id, Data);
         Cmd := Create (Toggle      => Toggle,
                        Is_Complete => (Bytes_To_Send = Bytes_Remain),
                        Data        => Data);
         This.Send_SDO (Endpoint => Endpoint,
                        Raw_Data => Cmd.Raw);
         This.SDO_Log
            (ACO.Log.Debug, "Client: Sent data segment of length" & Bytes_To_Send'Img);
      end;
   end Client_Send_Data_Segment;

   procedure Client_Download_Init
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      pragma Unreferenced (Msg);
      use ACO.SDO_Commands;

      Session      : SDO_Session := This.Sessions.Get (Endpoint.Id);
      Bytes_Remain : constant Natural := This.Sessions.Length_Buffer (Endpoint.Id);
   begin
      if Bytes_Remain = 0 then
         This.SDO_Log (ACO.Log.Debug, "Client: Expedited download completed");
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);

         return;
      end if;

      This.Start_Alarm (Endpoint);

      Session.Toggle := False;

      This.Client_Send_Data_Segment (Endpoint, Session.Toggle);

      This.Sessions.Put (Session);
   end Client_Download_Init;

   procedure Client_Upload_Init
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Resp  : constant Upload_Initiate_Resp := Convert (Msg);
      Index : constant Entry_Index := Get_Index (Msg);
      Error : Error_Type := Nothing;
   begin
      if not Resp.Is_Size_Indicated then
         Error := Command_Specifier_Not_Valid_Or_Unknown;
      elsif Get_Data_Size (Resp) > ACO.Configuration.Max_Data_SDO_Transfer_Size then
         Error := General_Error;
      end if;

      if Error /= Nothing then
         This.Send_Abort (Endpoint, Error, Index);
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);

         return;
      end if;

      declare
         Session : SDO_Session := This.Sessions.Get (Endpoint.Id);
         Cmd : Upload_Segment_Cmd;
      begin
         Session.Is_Complete := Resp.Is_Expedited;

         if Session.Is_Complete then
            This.Sessions.Put_Buffer
               (Id   => Endpoint.Id,
                Data => Resp.Data (0 .. 3 - Natural (Resp.Nof_No_Data)));
            This.SDO_Log (ACO.Log.Debug, "Client: Expedited upload completed");
            This.Stop_Alarm (Endpoint);
            --  NOTE: Do not end session here, let poller do that
         else
            --  TODO: Remember expected data size?
            This.Sessions.Clear_Buffer (Endpoint.Id);

            Session.Toggle := False;

            Cmd := Create (Session.Toggle);

            This.Send_SDO (Endpoint, Cmd.Raw);

            This.Start_Alarm (Endpoint);
         end if;

         This.Sessions.Put (Session);
      end;
   end Client_Upload_Init;

   procedure Client_Upload_Segment
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Resp : constant Upload_Segment_Resp := Convert (Msg);
      Session : SDO_Session;
      Cmd : Upload_Segment_Cmd;
   begin
      Session := This.Sessions.Get (Endpoint.Id);

      if Resp.Toggle /= Session.Toggle then
         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Toggle_Bit_Not_Altered,
                          Index    => Session.Index);
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);

         return;
      end if;

      This.Sessions.Put_Buffer
         (Id   => Endpoint.Id,
          Data => Resp.Data (0 .. 6 - Natural (Resp.Nof_No_Data)));

      Session.Is_Complete := Resp.Is_Complete;

      if Session.Is_Complete then
         This.Stop_Alarm (Endpoint);
         This.SDO_Log (ACO.Log.Debug, "Client: Segmented upload completed");
         --  NOTE: Do not end session here, let poller do that
      else
         Session.Toggle := not Session.Toggle;

         Cmd := Create (Session.Toggle);

         This.Send_SDO (Endpoint, Cmd.Raw);

         This.Start_Alarm (Endpoint);
      end if;

      This.Sessions.Put (Session);
   end Client_Upload_Segment;

   procedure Client_Download_Segment
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Resp         : constant Download_Segment_Resp := Convert (Msg);
      Session      : SDO_Session := This.Sessions.Get (Endpoint.Id);
      Bytes_Remain : constant Natural := This.Sessions.Length_Buffer (Endpoint.Id);
   begin
      if Resp.Toggle /= Session.Toggle then
         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Toggle_Bit_Not_Altered,
                          Index    => Session.Index);
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);

         return;
      end if;

      if Bytes_Remain = 0 then
         This.SDO_Log (ACO.Log.Debug, "Client: Segmented download completed");
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);

         return;
      end if;

      This.Start_Alarm (Endpoint);

      Session.Toggle := not Session.Toggle;

      This.Client_Send_Data_Segment (Endpoint, Session.Toggle);

      This.Sessions.Put (Session);
   end Client_Download_Segment;

   function Hex_Str (X : Interfaces.Unsigned_32) return String
   is
      use type Interfaces.Unsigned_32;

      Chars : constant String := "0123456789abcdef";
      N     : Interfaces.Unsigned_32 := X;
      Res   : String (1 .. 10) := "0x00000000";
      I     : Natural := Res'Last;
   begin
      loop
         Res (I) := Chars (Natural (N mod 16) + 1);
         N := N / 16;
         exit when N = 0;
         I := I - 1;
      end loop;
      return Res;
   end Hex_Str;

   procedure Abort_All
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;
      use type ACO.SDO_Commands.Abort_Code_Type;

      Resp : constant Abort_Cmd := Convert (Msg);
      Error : Error_Type := Unknown;
   begin
      for E in Error_Type'Range loop
         if Abort_Code (E) = Code (Resp) then
            Error := E;
            exit;
         end if;
      end loop;

      This.SDO_Log
         (ACO.Log.Error,
          Error'Img & " (" & Hex_Str (Code (Resp)) & ") on " & Image (Endpoint));

      This.Sessions.Clear (Endpoint.Id);
      This.Stop_Alarm (Endpoint);
   end Abort_All;

   procedure Message_Received_For_Client
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Service : constant Services := This.Sessions.Service (Endpoint.Id);
   begin
      case Get_CS (Msg) is
         when Download_Initiate_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Download Initiate");
            if Service = Download then
               This.Client_Download_Init (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Download_Segment_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Download Segment");
            if Service = Download then
               This.Client_Download_Segment (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Upload_Initiate_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Upload Initiate");
            if Service = Upload then
               This.Client_Upload_Init (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Upload_Segment_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Upload Segment");
            if Service = Upload then
               This.Client_Upload_Segment (Msg, Endpoint);
            else
               This.Send_Abort
                  (Endpoint => Endpoint,
                   Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
            end if;

         when Abort_Req =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Abort");
            This.Abort_All (Msg, Endpoint);

         when others =>
            null;
      end case;
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
            (Rx_CAN_Id         => CAN_Id (Msg),
             Client_Parameters => This.Od.Get_SDO_Client_Parameters,
             Server_Parameters => This.Od.Get_SDO_Server_Parameters);
      begin
         if Endpoint.Id /= No_Endpoint_Id then
            case Endpoint.Role is
               when Server =>
                  This.Message_Received_For_Server (Msg, Endpoint);
               when Client =>
                  This.Message_Received_For_Client (Msg, Endpoint);
            end case;
         end if;
      end;
   end Message_Received;

   procedure Write_Remote_Entry
      (This     : in out SDO;
       Node     : in     Node_Nr;
       Index    : in     Object_Index;
       Subindex : in     Object_Subindex;
       An_Entry : in     Entry_Base'Class)
   is
      use ACO.Configuration;

      Endpoint : constant Endpoint_Type := Get_Endpoint
         (Server_Node       => Node,
          Client_Parameters => This.Od.Get_SDO_Client_Parameters,
          Server_Parameters => This.Od.Get_SDO_Server_Parameters);
      Size : constant Natural := An_Entry.Data_Length;
   begin
      if Endpoint.Id = No_Endpoint_Id then
         This.SDO_Log (ACO.Log.Warning,
                       "Node" & Node'Img & " is not a server for any Client");
         return;
      elsif This.Sessions.Service (Endpoint.Id) /= None then
         This.SDO_Log (ACO.Log.Warning,
                       "Client endpoint" & Endpoint.Id'Img & " already in use");
         return;
      elsif not (Size in 1 .. Max_Data_SDO_Transfer_Size) then
         This.SDO_Log (ACO.Log.Warning,
                       "Size" & Size'Img & " bytes of entry is too large or 0");
         return;
      end if;

      This.Sessions.Clear_Buffer (Endpoint.Id);

      declare
         use ACO.SDO_Commands;
         Cmd : Download_Initiate_Cmd;
      begin
         if Size <= Expedited_Data'Length then
            Cmd := Create (Index => (Index, Subindex),
                           Data  => Data_Array (An_Entry.Read));
         else
            This.Sessions.Put_Buffer (Endpoint.Id, Data_Array (An_Entry.Read));
            Cmd := Create (Index => (Index, Subindex),
                           Size  => Size);
         end if;

         This.Send_SDO (Endpoint, Cmd.Raw);
      end;

      This.Sessions.Put (Create_Download (Endpoint, (Index, Subindex)));
      This.Start_Alarm (Endpoint);
   end Write_Remote_Entry;

   procedure Read_Remote_Entry
      (This     : in out SDO;
       Node     : in     Node_Nr;
       Index    : in     Object_Index;
       Subindex : in     Object_Subindex)
   is
      Endpoint : constant Endpoint_Type := Get_Endpoint
         (Server_Node       => Node,
          Client_Parameters => This.Od.Get_SDO_Client_Parameters,
          Server_Parameters => This.Od.Get_SDO_Server_Parameters);
      Cmd : ACO.SDO_Commands.Upload_Initiate_Cmd;
   begin
      if Endpoint.Id = No_Endpoint_Id then
         This.SDO_Log (ACO.Log.Warning,
                       "Node" & Node'Img & " is not a server for any Client");
         return;
      elsif This.Sessions.Service (Endpoint.Id) /= None then
         This.SDO_Log (ACO.Log.Warning,
                       "Client endpoint" & Endpoint.Id'Img & " already in use");
         return;
      end if;

      Cmd := ACO.SDO_Commands.Create ((Index, Subindex));

      This.Send_SDO (Endpoint, Cmd.Raw);

      This.Sessions.Put (Create_Upload (Endpoint, (Index, Subindex)));
      This.Start_Alarm (Endpoint);
   end Read_Remote_Entry;

   procedure Periodic_Actions
      (This  : in out SDO;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.Timeouts.Process (T_Now);
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
