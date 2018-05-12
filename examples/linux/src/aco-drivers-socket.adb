package body ACO.Drivers.Socket is

   overriding
   procedure Await_Message
     (This : in out CAN_Driver;
      Msg  :    out Message)
   is
      use SocketCAN;

      Frame : Can_Frame;
   begin
      Receive_Socket (This.Socket, Frame);

      Msg := (CAN_Id => (True, Id_Type (Frame.Can_Id)),
              RTR    => Frame.Rtr,
              Length => Frame.Dlc,
              Data   => Data_Array (Frame.Data));
   end Await_Message;

   overriding
   procedure Send_Message
     (This : in out CAN_Driver;
      Msg  : in     Message)
   is
      use SocketCAN;

      Frame : constant Can_Frame :=
         (Can_Id => Frame_Id_Type (CAN_Id (Msg)),
          Rtr    => Msg.RTR,
          Dlc    => Msg.Length,
          Data   => Frame_Data (Msg.Data));
   begin
      Send_Socket (This.Socket, Frame);
   end Send_Message;

   overriding
   procedure Initialize (This : in out CAN_Driver)
   is
      use SocketCAN;
   begin
      This.Socket := Create_Socket (RAW);

      Bind_Socket (This.Socket, CAN_If_Name);
   end Initialize;

end ACO.Drivers.Socket;
