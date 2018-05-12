with Interfaces;
private with Interfaces.C;
private with System;

package SocketCAN is
   --  Ada binding for the SocketCAN driver included in Linux Kernel > 2.6.25.
   --  https://www.kernel.org/doc/Documentation/networking/can.txt
   --
   --  Note: This binding is incomplete!
   --  Interfaces CAN_RAW socket with CAN 2.0B standard frame format.
   --  The other protocols (and FD) should be easy to add though.
   --
   --  Adding e.g. a virtual CAN interface (vcan):
   --  $ sudo modprobe vcan
   --  $ sudo ip link add dev vcan0 type vcan
   --  $ sudo ip link set up vcan0

   type Socket_Type is private;

   type Protocol_Type is
      (RAW,    --  RAW sockets
       BCM,    --  Broadcast Manager
       TP16,   --  VAG Transport Protocol v1.6
       TP20,   --  VAG Transport Protocol v2.0
       MCNET,  --  Bosch MCNet
       ISOTP,  --  ISO 15765-2 Transport Protocol
       J1939,  --  SAE J1939
       NPROTO);

   Is_Implemented : constant array (Protocol_Type) of Boolean :=
      (RAW                                                => True,
       BCM | TP16 | TP20 | MCNET | ISOTP | J1939 | NPROTO => False);

   type Frame_Id_Type is mod 2**11;
   subtype Dlc_Type is Natural range 0 .. 8;

   subtype Frame_Data_Type is Interfaces.Unsigned_8;
   type Frame_Data is array (0 .. 7) of Frame_Data_Type;

   type Can_Frame is record
      Can_Id : Frame_Id_Type;
      Rtr    : Boolean;
      Dlc    : Dlc_Type;
      Data   : Frame_Data;
   end record;

   SocketCAN_Error : exception;

   function Create_Socket (Protocol : Protocol_Type := RAW) return Socket_Type
      with Pre => Is_Implemented (Protocol);

   procedure Bind_Socket
     (Socket : in Socket_Type;
      Name   : in String := "can0");

   procedure Send_Socket
     (Socket : in Socket_Type;
      Frame  : in Can_Frame);

   procedure Receive_Socket
     (Socket : in     Socket_Type;
      Frame  :    out Can_Frame);

private

   type Socket_Type is new Integer;

   package C renames Interfaces.C;

   function C_Name_To_Index (Name : C.char_array) return C.int;
   pragma Import (C, C_Name_To_Index, "if_nametoindex");

   function C_Write
      (S   : C.int;
       Msg : System.Address;
       Len : C.int) return C.int;
   pragma Import (C, C_Write, "write");

   function C_Read
      (S   : C.int;
       Msg : System.Address;
       Len : C.int) return C.int;
   pragma Import (C, C_Read, "read");

end SocketCAN;
