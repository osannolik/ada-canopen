package body ACO.Protocols.Service_Data.Clients is

   overriding
   procedure Handle_Message
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type)
   is
      use ACO.SDO_Commands;
      use type ACO.SDO_Sessions.Services;

      Service : constant ACO.SDO_Sessions.Services :=
         This.Sessions.Service (Endpoint.Id);
      State_Error : Boolean := False;
   begin
      case Get_CS (Msg) is
         when Download_Initiate_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Download Initiate");
            if Service = ACO.SDO_Sessions.Download then
               This.Download_Init (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Download_Segment_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Download Segment");
            if Service = ACO.SDO_Sessions.Download then
               This.Download_Segment (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Upload_Initiate_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Upload Initiate");
            if Service = ACO.SDO_Sessions.Upload then
               This.Upload_Init (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Upload_Segment_Conf =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Upload Segment");
            if Service = ACO.SDO_Sessions.Upload then
               This.Upload_Segment (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Abort_Req =>
            This.SDO_Log (ACO.Log.Debug, "Client: Handling Abort");
            This.Abort_All (Msg, Endpoint);

         when others =>
            null;
      end case;

      if State_Error then
         This.Send_Abort
            (Endpoint => Endpoint,
             Error    => Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);
      end if;
   end Handle_Message;

   procedure Download_Init
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type)
   is
      pragma Unreferenced (Msg);
      use ACO.SDO_Commands;

      Id           : constant ACO.SDO_Sessions.Valid_Endpoint_Nr := Endpoint.Id;
      Session      : ACO.SDO_Sessions.SDO_Session := This.Sessions.Get (Id);
      Bytes_Remain : constant Natural :=
         This.Sessions.Length_Buffer (Endpoint.Id);
   begin
      if Bytes_Remain = 0 then
         This.SDO_Log (ACO.Log.Debug, "Client: Expedited download completed");
         This.Stop_Alarm (Id);
         Session.Status := ACO.SDO_Sessions.Complete;
      else
         This.Start_Alarm (Id);

         Session.Toggle := False;

         This.Send_Buffered (Endpoint, Session.Toggle);
      end if;

      This.Sessions.Put (Session);
   end Download_Init;

   procedure Download_Segment
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Resp         : constant Download_Segment_Resp := Convert (Msg);
      Id           : constant ACO.SDO_Sessions.Valid_Endpoint_Nr := Endpoint.Id;
      Session      : ACO.SDO_Sessions.SDO_Session := This.Sessions.Get (Id);
      Bytes_Remain : constant Natural :=
         This.Sessions.Length_Buffer (Endpoint.Id);
   begin
      if Resp.Toggle = Session.Toggle then
         if Bytes_Remain = 0 then
            Session.Status := ACO.SDO_Sessions.Complete;

            This.SDO_Log (ACO.Log.Debug, "Client: Segment download completed");
            This.Stop_Alarm (Id);
         else
            Session.Toggle := not Session.Toggle;

            This.Send_Buffered (Endpoint, Session.Toggle);

            This.Start_Alarm (Id);
         end if;
      else
         Session.Status := ACO.SDO_Sessions.Error;

         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Toggle_Bit_Not_Altered,
                          Index    => Session.Index);
         This.Stop_Alarm (Id);
      end if;

      This.Sessions.Put (Session);
   end Download_Segment;

   procedure Send_Buffered
      (This     : in out Client;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type;
       Toggle   : in     Boolean)
   is
      use ACO.SDO_Commands;

      Bytes_Remain : constant Natural :=
         This.Sessions.Length_Buffer (Endpoint.Id);
   begin
      if Bytes_Remain = 0 then
         return;
      end if;

      declare
         Bytes_To_Send : constant Positive :=
            Natural'Min (Bytes_Remain, Segment_Data'Length);
         Data : ACO.Messages.Data_Array (0 .. Bytes_To_Send - 1);
         Cmd : Download_Segment_Cmd;
      begin
         This.Sessions.Get_Buffer (Endpoint.Id, Data);
         Cmd := Create (Toggle      => Toggle,
                        Is_Complete => (Bytes_To_Send = Bytes_Remain),
                        Data        => Data);
         This.Send_SDO (Endpoint => Endpoint,
                        Raw_Data => Cmd.Raw);
         This.SDO_Log
            (ACO.Log.Debug, "Sent data of length" & Bytes_To_Send'Img);
      end;
   end Send_Buffered;

   procedure Upload_Init
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Resp    : constant Upload_Initiate_Resp := Convert (Msg);
      Index   : constant ACO.OD_Types.Entry_Index := Get_Index (Msg);
      Error   : Error_Type := Nothing;
      Session : ACO.SDO_Sessions.SDO_Session;
      Id      : constant ACO.SDO_Sessions.Valid_Endpoint_Nr := Endpoint.Id;
   begin
      if not Resp.Is_Size_Indicated then
         Error := Command_Specifier_Not_Valid_Or_Unknown;
      elsif Get_Data_Size (Resp) > ACO.Configuration.Max_SDO_Transfer_Size then
         Error := General_Error;
      end if;

      Session := This.Sessions.Get (Id);

      if Error = Nothing then
         if Resp.Is_Expedited then
            Session.Status := ACO.SDO_Sessions.Complete;

            This.Sessions.Put_Buffer
               (Id   => Id,
                Data => Resp.Data (0 .. 3 - Natural (Resp.Nof_No_Data)));
            This.SDO_Log (ACO.Log.Debug, "Client: Expedited upload completed");
            This.Stop_Alarm (Id);
            --  NOTE: Do not end session here, let poller do that
         else
            --  TODO: Remember expected data size?
            This.Sessions.Clear_Buffer (Endpoint.Id);

            Session.Toggle := False;

            declare
               Cmd : constant Upload_Segment_Cmd := Create (Session.Toggle);
            begin
               This.Send_SDO (Endpoint, Cmd.Raw);
            end;

            This.Start_Alarm (Id);
         end if;
      else
         Session.Status := ACO.SDO_Sessions.Error;

         This.Send_Abort (Endpoint, Error, Index);
         This.Stop_Alarm (Id);
      end if;

      This.Sessions.Put (Session);
   end Upload_Init;

   procedure Upload_Segment
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Resp    : constant Upload_Segment_Resp := Convert (Msg);
      Session : ACO.SDO_Sessions.SDO_Session;
      Id      : constant ACO.SDO_Sessions.Valid_Endpoint_Nr := Endpoint.Id;
   begin
      Session := This.Sessions.Get (Id);

      if Resp.Toggle = Session.Toggle then
         This.Sessions.Put_Buffer
            (Id   => Id,
             Data => Resp.Data (0 .. 6 - Natural (Resp.Nof_No_Data)));

         if Resp.Is_Complete then
            Session.Status := ACO.SDO_Sessions.Complete;

            This.Stop_Alarm (Id);
            This.SDO_Log (ACO.Log.Debug, "Client: Segmented upload completed");
            --  NOTE: Do not end session here, let poller do that
         else
            Session.Toggle := not Session.Toggle;

            declare
               Cmd : constant Upload_Segment_Cmd := Create (Session.Toggle);
            begin
               This.Send_SDO (Endpoint, Cmd.Raw);
            end;

            This.Start_Alarm (Id);
         end if;
      else
         Session.Status := ACO.SDO_Sessions.Error;

         This.Send_Abort (Endpoint => Endpoint,
                          Error    => Toggle_Bit_Not_Altered,
                          Index    => Session.Index);
         This.Stop_Alarm (Id);
      end if;

      This.Sessions.Put (Session);
   end Upload_Segment;

   procedure Write_Remote_Entry
      (This        : in out Client;
       Node        : in     ACO.Messages.Node_Nr;
       Index       : in     ACO.OD_Types.Object_Index;
       Subindex    : in     ACO.OD_Types.Object_Subindex;
       An_Entry    : in     ACO.OD_Types.Entry_Base'Class;
       Endpoint_Id :    out ACO.SDO_Sessions.Endpoint_Nr)
   is
      use ACO.Configuration;
      use type ACO.SDO_Sessions.Services;

      Endpoint : constant ACO.SDO_Sessions.Endpoint_Type :=
         ACO.SDO_Sessions.Get_Endpoint
            (Server_Node       => Node,
             Client_Parameters => This.Od.Get_SDO_Client_Parameters,
             Server_Parameters => This.Od.Get_SDO_Server_Parameters);
      Size : constant Natural := An_Entry.Data_Length;
   begin
      Endpoint_Id := ACO.SDO_Sessions.No_Endpoint_Id;

      if Endpoint.Id = ACO.SDO_Sessions.No_Endpoint_Id then
         This.SDO_Log (ACO.Log.Warning,
                       "Node" & Node'Img & " is not a server for any Client");
         return;
      elsif This.Sessions.Service (Endpoint.Id) /= ACO.SDO_Sessions.None then
         This.SDO_Log (ACO.Log.Warning,
                       "Client endpoint" & Endpoint.Id'Img & " already in use");
         return;
      elsif not (Size in 1 .. Max_SDO_Transfer_Size) then
         This.SDO_Log (ACO.Log.Warning,
                       "Size" & Size'Img & " bytes of entry is too large or 0");
         return;
      end if;

      Endpoint_Id := Endpoint.Id;

      This.Sessions.Clear_Buffer (Endpoint.Id);

      declare
         use ACO.SDO_Commands;
         Cmd : Download_Initiate_Cmd;
      begin
         if Size <= Expedited_Data'Length then
            Cmd := Create (Index => (Index, Subindex),
                           Data  => ACO.Messages.Data_Array (An_Entry.Read));
         else
            This.Sessions.Put_Buffer
               (Endpoint.Id, ACO.Messages.Data_Array (An_Entry.Read));
            Cmd := Create (Index => (Index, Subindex),
                           Size  => Size);
         end if;

         This.Send_SDO (Endpoint, Cmd.Raw);
      end;

      This.Sessions.Put
         (ACO.SDO_Sessions.Create_Download (Endpoint, (Index, Subindex)));
      This.Start_Alarm (Endpoint.Id);
   end Write_Remote_Entry;

   procedure Read_Remote_Entry
      (This        : in out Client;
       Node        : in     ACO.Messages.Node_Nr;
       Index       : in     ACO.OD_Types.Object_Index;
       Subindex    : in     ACO.OD_Types.Object_Subindex;
       Endpoint_Id :    out ACO.SDO_Sessions.Endpoint_Nr)
   is
      use type ACO.SDO_Sessions.Services;

      Endpoint : constant ACO.SDO_Sessions.Endpoint_Type :=
         ACO.SDO_Sessions.Get_Endpoint
            (Server_Node       => Node,
             Client_Parameters => This.Od.Get_SDO_Client_Parameters,
             Server_Parameters => This.Od.Get_SDO_Server_Parameters);
      Cmd : ACO.SDO_Commands.Upload_Initiate_Cmd;
   begin
      Endpoint_Id := Endpoint.Id;

      if Endpoint.Id = ACO.SDO_Sessions.No_Endpoint_Id then
         This.SDO_Log (ACO.Log.Warning,
                       "Node" & Node'Img & " is not a server for any Client");
         return;
      elsif This.Sessions.Service (Endpoint.Id) /= ACO.SDO_Sessions.None then
         Endpoint_Id := ACO.SDO_Sessions.No_Endpoint_Id;
         This.SDO_Log (ACO.Log.Warning,
                       "Client endpoint" & Endpoint.Id'Img & " already in use");
         return;
      end if;

      Cmd := ACO.SDO_Commands.Create ((Index, Subindex));

      This.Send_SDO (Endpoint, Cmd.Raw);

      This.Sessions.Put
         (ACO.SDO_Sessions.Create_Upload (Endpoint, (Index, Subindex)));
      This.Start_Alarm (Endpoint.Id);
   end Read_Remote_Entry;

   procedure Get_Read_Entry
      (This        : in out Client;
       Endpoint_Id : in     ACO.SDO_Sessions.Valid_Endpoint_Nr;
       Read_Entry  : in out ACO.OD_Types.Entry_Base'Class)
   is
      Data : ACO.Messages.Data_Array (0 .. Read_Entry.Data_Length - 1);
   begin
      This.Sessions.Get_Buffer (Endpoint_Id, Data);
      Read_Entry.Write (ACO.OD_Types.Byte_Array (Data));
      This.Sessions.Clear (Endpoint_Id);
   end Get_Read_Entry;

end ACO.Protocols.Service_Data.Clients;
