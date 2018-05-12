package body ACO.Protocols.Synchronization is

   procedure Message_Received
     (This : in out Sync;
      Msg  : in     Message)
   is
      pragma Unreferenced (This, Msg);
   begin
      null;
   end Message_Received;

end ACO.Protocols.Synchronization;
