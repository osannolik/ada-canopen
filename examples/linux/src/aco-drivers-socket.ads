private with SocketCAN; --  Requires GNAT...

package ACO.Drivers.Socket is

   type CAN_Driver is new Driver with private;

   CAN_If_Name : constant String := "vcan0";

   overriding
   procedure Receive_Message_Blocking
      (This : in out CAN_Driver;
       Msg  :    out Message);
   overriding
   procedure Send_Message
     (This : in out CAN_Driver;
      Msg  : in     Message);

   overriding
   procedure Initialize (This : in out CAN_Driver);

   overriding
   function Is_Message_Pending (This : CAN_Driver) return Boolean;

private

   type CAN_Driver is new Driver with record
      Socket : SocketCAN.Socket_Type;
   end record;

end ACO.Drivers.Socket;
