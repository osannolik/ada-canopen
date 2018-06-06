pragma Warnings (Off);
--  Internal GNAT unit, non-portable!
--  But SocketCAN is only available on Linux anyways...
with GNAT.Sockets.Thin;
with GNAT.Sockets.Thin_Common;
pragma Warnings (On);

with ACO.Log;
package body SocketCAN is

   use type C.int;

   package Socket_Defs is
      --  socket.h
      SOCK_RAW : constant := 3;
      AF_CAN   : constant := 29;
      PF_CAN   : constant := 29;
   end Socket_Defs;

   package Can_Defs is
      --  can.h
      CAN_RAW : constant := 1;
      pragma Unreferenced (CAN_RAW);

      type C_Raw_Sockaddr_In is record
         Family : C.unsigned_short;
         Index  : C.int;
         Fill   : C.char_array (0 .. 15);
      end record
         with Size => 24 * 8;
      pragma No_Component_Reordering (C_Raw_Sockaddr_In);
      pragma Convention (C, C_Raw_Sockaddr_In);

      for C_Raw_Sockaddr_In use record
         Family at 0 range 0 .. 31;
         Index  at 0 range 32 .. 63;
      end record;

      type C_Can_Data is array (0 .. 7) of Interfaces.Unsigned_8;
      for C_Can_Data'Alignment use 8;

      type C_Can_Frame is record
         Can_Id  : Interfaces.Unsigned_32;
         Can_Dlc : Interfaces.Unsigned_8;
         Pad     : Interfaces.Unsigned_8;
         Res0    : Interfaces.Unsigned_8;
         Res1    : Interfaces.Unsigned_8;
         Data    : C_Can_Data;
      end record;
      pragma No_Component_Reordering (C_Can_Frame);
      pragma Convention (C, C_Can_Frame);

      RTR_Flag : constant := 16#40000000#;
      EFF_Flag : constant := 16#80000000#;
      ERR_Flag : constant := 16#20000000#;

      SFF_Mask : constant := 16#000007FF#;
      EFF_Mask : constant := 16#1FFFFFFF#;
      ERR_Mask : constant := 16#1FFFFFFF#;

      pragma Unreferenced (EFF_Flag, ERR_Flag, EFF_Mask, ERR_Mask);

   end Can_Defs;

   function Convert (Frame : Can_Frame) return Can_Defs.C_Can_Frame;

   function Convert (Frame : Can_Frame) return Can_Defs.C_Can_Frame
   is
      use Interfaces;

      Id : constant Unsigned_32 := Unsigned_32 (Frame.Can_Id);
   begin
      return (Can_Id  => (if Frame.Rtr then Id or Can_Defs.RTR_Flag else Id),
              Can_Dlc => Unsigned_8 (Frame.Dlc),
              Pad     => 0,
              Res0    => 0,
              Res1    => 0,
              Data    => Can_Defs.C_Can_Data (Frame.Data));
   end Convert;

   function Convert (Frame : Can_Defs.C_Can_Frame) return Can_Frame;

   function Convert (Frame : Can_Defs.C_Can_Frame) return Can_Frame
   is
      use Interfaces;
   begin
      return (Can_Id => Frame_Id_Type (Frame.Can_Id and Can_Defs.SFF_Mask),
              Rtr    => (Frame.Can_Id and Can_Defs.RTR_Flag) /= 0,
              Dlc    => Dlc_Type (Frame.Can_Dlc),
              Data   => Frame_Data (Frame.Data));
   end Convert;

   procedure Send_Socket
     (Socket : in Socket_Type;
      Frame  : in Can_Frame)
   is
      Msg : aliased Can_Defs.C_Can_Frame := Convert (Frame);
      Mtu : constant C.int := Msg'Size / 8;
   begin
      if C_Write (C.int (Socket), Msg'Address, Mtu) /= Mtu then
         raise SocketCAN_Error with
            "Failed to write message with id =" & Msg.Can_Id'Img;
      end if;
   end Send_Socket;

   procedure Receive_Socket
     (Socket : in     Socket_Type;
      Frame  :    out Can_Frame)
   is
      use GNAT.Sockets.Thin_Common;

      Msg : aliased Can_Defs.C_Can_Frame;
      Mtu : constant C.int := Msg'Size / 8;
      Res : C.int;
   begin
      ACO.Log.Put_Line (ACO.Log.Debug, "Mtu=" & Mtu'Img);

      Res := C_Read (C.int (Socket), Msg'Address, Mtu);

      if Res = Failure then
         raise SocketCAN_Error with "Failed to read message";
      elsif Res /= Mtu then
         raise SocketCAN_Error with "Received frame size not supported";
      end if;

      Frame := Convert (Msg);
   end Receive_Socket;

   Protocols : constant array (Protocol_Type) of C.int :=
      (RAW    => 1,
       BCM    => 2,
       TP16   => 3,
       TP20   => 4,
       MCNET  => 5,
       ISOTP  => 6,
       J1939  => 7,
       NPROTO => 8);

   function Create_Socket (Protocol : Protocol_Type := RAW) return Socket_Type
   is
      use GNAT.Sockets.Thin;
      use GNAT.Sockets.Thin_Common;

      Res : C.int;

   begin
      ACO.Log.Put_Line (ACO.Log.Debug, "Create_Socket " & Protocol'Img);

      Res := C_Socket (Domain   => Socket_Defs.PF_CAN,
                       Typ      => Socket_Defs.SOCK_RAW,
                       Protocol => Protocols (Protocol));

      ACO.Log.Put_Line (ACO.Log.Debug, "Create_Socket Res=" & Res'Img);

      if Res = Failure then
         raise SocketCAN_Error with "Failed to create socket";
      end if;

      return Socket_Type (Res);
   end Create_Socket;

   procedure Bind_Socket
     (Socket  : in Socket_Type;
      Name    : in String := "can0")
   is
      use GNAT.Sockets.Thin;
      use GNAT.Sockets.Thin_Common;

      Index : constant C.int := C_Name_To_Index (C.To_C (Name));
   begin
      ACO.Log.Put_Line (ACO.Log.Debug, "Bind_Socket " & Name & " index" & Index'Img);

      if Index = 0 then
         ACO.Log.Put_Line (ACO.Log.Error, "Failed to bind " & Name & " index" & Index'Img);
         raise SocketCAN_Error with "Could not get interface index of " & Name;
      end if;

      declare
         Sin : aliased Can_Defs.C_Raw_Sockaddr_In :=
            (Family => Socket_Defs.AF_CAN,
             Index  => Index,
             Fill   => (others => C.char'First));
         Res : C.int;
      begin
         Res := C_Bind (C.int (Socket), Sin'Address, Sin'Size / 8);

         ACO.Log.Put_Line (ACO.Log.Debug, "Bind_Socket Res=" & Res'Img);

         if Res = Failure then
            raise SocketCAN_Error with "Failed to bind " & Name;
         end if;
      end;
   end Bind_Socket;

end SocketCAN;
