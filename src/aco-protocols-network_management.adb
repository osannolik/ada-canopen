package body ACO.Protocols.Network_Management is

   procedure Message_Received
     (This : in out NMT;
      Msg  : in     Message)
   is
      pragma Unreferenced (This, Msg);
   begin
      null;
   end Message_Received;

end ACO.Protocols.Network_Management;
