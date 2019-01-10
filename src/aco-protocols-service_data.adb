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

      Session : ACO.SDO_Sessions.SDO_Session :=
         This.SDO_Ref.Sessions.Get (This.Id);
   begin
      This.SDO_Ref.SDO_Log
         (ACO.Log.Info,
          "Session timed out for service " & Session.Service'Img & This.Id'Img);

      This.SDO_Ref.Send_Abort
         (Endpoint => Session.Endpoint,
          Error    => SDO_Protocol_Timed_Out,
          Index    => Session.Index);

      Session.Status := ACO.SDO_Sessions.Error;
      This.SDO_Ref.Sessions.Put (Session);
   end Signal;

   procedure Start_Alarm
      (This : in out SDO;
       Id   : in     ACO.SDO_Sessions.Valid_Endpoint_Nr)
   is
      use Ada.Real_Time, ACO.Configuration;

      Timeout_Alarm : Alarm renames This.Alarms (Id);
   begin
      if This.Timers.Is_Pending (Timeout_Alarm'Unchecked_Access) then
         This.Timers.Cancel (Timeout_Alarm'Unchecked_Access);
      end if;

      Timeout_Alarm.Id := Id;
      This.Timers.Set
         (Alarm       => Timeout_Alarm'Unchecked_Access,
          Signal_Time => Clock + Milliseconds (SDO_Session_Timeout_Ms));
   end Start_Alarm;

   procedure Stop_Alarm
      (This : in out SDO;
       Id   : in     ACO.SDO_Sessions.Valid_Endpoint_Nr)
   is
   begin
      This.Timers.Cancel (This.Alarms (Id)'Unchecked_Access);
   end Stop_Alarm;

   procedure Send_Abort
      (This     : in out SDO;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type;
       Error    : in     Error_Type;
       Index    : in     ACO.OD_Types.Entry_Index := (0,0))
   is
      use ACO.SDO_Commands;

      Cmd : constant Abort_Cmd := Create (Index, Abort_Code (Error));
   begin
      This.SDO_Log (ACO.Log.Warning, "Aborting: " & Error'Img);
      This.Send_SDO (Endpoint, Cmd.Raw);
   end Send_Abort;

   procedure Write
      (This  : in out SDO;
       Index : in     ACO.OD_Types.Entry_Index;
       Data  : in     ACO.Messages.Data_Array;
       Error :    out Error_Type)
   is
      --  TODO:
      --  Need a more efficient way to write large amount of data (Domain type)
      Ety : ACO.OD_Types.Entry_Base'Class :=
         This.Od.Get_Entry (Index.Object, Index.Sub);
   begin
      Ety.Write (ACO.OD_Types.Byte_Array (Data));
      This.Od.Set_Entry (Ety, Index.Object, Index.Sub);
      Error := Nothing;
   exception
      when E : others =>
         Error := Failed_To_Transfer_Or_Store_Data;
         This.SDO_Log (ACO.Log.Debug, Ada.Exceptions.Exception_Name (E));
   end Write;

   procedure Send_SDO
      (This     : in out SDO;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type;
       Raw_Data : in     ACO.Messages.Msg_Data)
   is
      Msg : constant ACO.Messages.Message :=
         ACO.Messages.Create (CAN_Id => ACO.SDO_Sessions.Tx_CAN_Id (Endpoint),
                              RTR    => False,
                              DLC    => 8,
                              Data   => Raw_Data);
   begin
      This.SDO_Log (ACO.Log.Debug, "Sending " & ACO.Messages.Image (Msg));
      This.Handler.Put (Msg);
   end Send_SDO;

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
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type)
   is
      use ACO.SDO_Commands;
      use type ACO.SDO_Commands.Abort_Code_Type;
      use type ACO.SDO_Sessions.Endpoint_Role;

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
          Error'Img & " (" & Hex_Str (Code (Resp)) & ") on " &
          ACO.SDO_Sessions.Image (Endpoint));

      case Endpoint.Role is
         when ACO.SDO_Sessions.Server =>
            This.Sessions.Clear (Endpoint.Id);

         when ACO.SDO_Sessions.Client =>
            declare
               Session : ACO.SDO_Sessions.SDO_Session :=
                  This.Sessions.Get (Endpoint.Id);
            begin
               Session.Status := ACO.SDO_Sessions.Error;
               This.Sessions.Put (Session);
            end;
      end case;

      This.Stop_Alarm (Endpoint.Id);
   end Abort_All;

   procedure Message_Received
     (This : in out SDO;
      Msg  : in     ACO.Messages.Message)
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
         Endpoint : constant ACO.SDO_Sessions.Endpoint_Type :=
            ACO.SDO_Sessions.Get_Endpoint
               (Rx_CAN_Id         => ACO.Messages.CAN_Id (Msg),
                Client_Parameters => This.Od.Get_SDO_Client_Parameters,
                Server_Parameters => This.Od.Get_SDO_Server_Parameters);
      begin
         if Endpoint.Id /= ACO.SDO_Sessions.No_Endpoint_Id then
            SDO'Class (This).Handle_Message (Msg, Endpoint);
         end if;
      end;
   end Message_Received;

   function Get_Status
      (This : SDO;
       Id   : ACO.SDO_Sessions.Valid_Endpoint_Nr)
       return ACO.SDO_Sessions.SDO_Status
   is
   begin
      return This.Sessions.Get (Id).Status;
   end Get_Status;

   function Is_Complete
      (This : SDO;
       Id   : ACO.SDO_Sessions.Valid_Endpoint_Nr)
       return Boolean
   is
   begin
      return ACO.SDO_Sessions.Is_Complete (This.Sessions.Get (Id));
   end Is_Complete;

   procedure Clear
      (This : in out SDO;
       Id   : in     ACO.SDO_Sessions.Valid_Endpoint_Nr)
   is
   begin
      This.Sessions.Clear (Id);
   end Clear;

   procedure Periodic_Actions
      (This  : in out SDO;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.Timers.Process (T_Now);
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
