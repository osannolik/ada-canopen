with Ada.Exceptions;
with ACO.Log;

package body ACO.Drivers.Socket is
   use ACO.Log;

   overriding
   procedure Receive_Message_Blocking
      (This : in out CAN_Driver;
       Msg  :    out ACO.Messages.Message)
   is
      Frame : SocketCAN.Can_Frame;
   begin
      SocketCAN.Receive_Socket_Blocking (This.Socket, Frame);

      Msg := (CAN_Id => (True, ACO.Messages.Id_Type (Frame.Can_Id)),
              RTR    => Frame.Rtr,
              Length => Frame.Dlc,
              Data   => ACO.Messages.Data_Array (Frame.Data));
   exception
      when E: others =>
         Put_Line (Warning, Ada.Exceptions.Exception_Information (E));

   end Receive_Message_Blocking;

   overriding
   procedure Send_Message
      (This : in out CAN_Driver;
       Msg  : in     ACO.Messages.Message)
   is
      Frame : SocketCAN.Can_Frame;
   begin
      Frame := (Can_Id => SocketCAN.Frame_Id_Type (ACO.Messages.CAN_Id (Msg)),
                Rtr    => Msg.RTR,
                Dlc    => Msg.Length,
                Data   => SocketCAN.Frame_Data (Msg.Data));

      SocketCAN.Send_Socket (This.Socket, Frame);
   exception
      when E: others =>
         Put_Line (Warning, Ada.Exceptions.Exception_Information (E));

   end Send_Message;

   overriding
   procedure Initialize
      (This : in out CAN_Driver)
   is
   begin
      This.Socket := SocketCAN.Create_Socket (SocketCAN.RAW);

      SocketCAN.Bind_Socket (This.Socket, CAN_If_Name);
   exception
      when E: others =>
         Put_Line (Error, Ada.Exceptions.Exception_Information (E));

   end Initialize;

   overriding
   procedure Finalize
      (This : in out CAN_Driver)
   is
   begin
      SocketCAN.Close_Socket (This.Socket);
   exception
      when E: others =>
         Put_Line (Warning, Ada.Exceptions.Exception_Information (E));

   end Finalize;

   overriding
   function Is_Message_Pending
      (This : CAN_Driver)
       return Boolean
   is
   begin
      return SocketCAN.Is_Frame_Pending (This.Socket);
   end Is_Message_Pending;

   overriding
   function Current_Time
      (This : CAN_Driver)
       return Ada.Real_Time.Time
   is
      pragma Unreferenced (This);
   begin
      return Ada.Real_Time.Clock;
   end Current_Time;

end ACO.Drivers.Socket;
