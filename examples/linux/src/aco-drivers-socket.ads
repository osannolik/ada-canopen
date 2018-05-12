private with SocketCAN; --  Requires GNAT...

package ACO.Drivers.Socket is

   type CAN_Driver is new Driver with private;

   CAN_If_Name : constant String := "vcan0";

   overriding
   procedure Await_Message (This : in out CAN_Driver;
                            Msg  :    out Message);
   overriding
   procedure Send_Message
     (This : in out CAN_Driver;
      Msg  : in     Message);

   overriding
   procedure Initialize (This : in out CAN_Driver);

private

   type CAN_Driver is new Driver with record
      Socket : SocketCAN.Socket_Type;
   end record;

end ACO.Drivers.Socket;
