package body ACO.Protocols.Service_Data.Servers is

   overriding
   procedure Handle_Message
      (This     : in out Server;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Service : constant Services := This.Sessions.Service (Endpoint.Id);
      State_Error : Boolean := False;
   begin
      case Get_CS (Msg) is
         when Download_Initiate_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Download Initiate");
            if Service = None then
               This.Download_Init (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Download_Segment_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Download Segment");
            if Service = Download then
               This.Download_Segment (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Upload_Initiate_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Upload Initiate");
            if Service = None then
               This.Upload_Init (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Upload_Segment_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Upload Segment");
            if Service = Upload then
               This.Upload_Segment (Msg, Endpoint);
            else
               State_Error := True;
            end if;

         when Abort_Req =>
            This.SDO_Log (ACO.Log.Debug, "Server: Handling Abort");
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

   procedure Upload_Init
      (This     : in out Server;
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
         Ety  : constant Entry_Base'Class :=
            This.Od.Get_Entry (Index.Object, Index.Sub);
         Size : Natural;
         Resp : Upload_Initiate_Resp;
      begin
         Size := Ety.Data_Length;

         if Size > ACO.Configuration.Max_SDO_Transfer_Size then
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
   end Upload_Init;

   procedure Upload_Segment
      (This     : in out Server;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type)
   is
      use ACO.SDO_Commands;

      Cmd          : constant Upload_Segment_Cmd := Convert (Msg);
      Bytes_Remain : constant Natural :=
         This.Sessions.Length_Buffer (Endpoint.Id);
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
         if Bytes_To_Send = Bytes_Remain then
            Session.Status := Complete;
         end if;

         This.Sessions.Get_Buffer (Endpoint.Id, Data);
         Resp := Create (Toggle      => Session.Toggle,
                         Is_Complete => ACO.SDO_Sessions.Is_Complete (Session),
                         Data        => Data);
         This.Send_SDO (Endpoint => Endpoint,
                        Raw_Data => Resp.Raw);
         This.SDO_Log
            (ACO.Log.Debug, "Server: Sent data of length" & Bytes_To_Send'Img);
      end;

      if ACO.SDO_Sessions.Is_Complete (Session) then
         This.Stop_Alarm (Endpoint);
         This.Sessions.Clear (Endpoint.Id);
      else
         This.Start_Alarm (Endpoint);

         Session.Toggle := not Session.Toggle;

         This.Sessions.Put (Session);
      end if;
   end Upload_Segment;

   procedure Download_Init
      (This     : in out Server;
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
      elsif Get_Data_Size (Cmd) > ACO.Configuration.Max_SDO_Transfer_Size then
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
            Session : constant SDO_Session := Create_Download (Endpoint, Index);
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
   end Download_Init;

   procedure Download_Segment
      (This     : in out Server;
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
   end Download_Segment;

end ACO.Protocols.Service_Data.Servers;
