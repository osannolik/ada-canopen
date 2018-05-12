package ACO.Protocols.Synchronization is

   pragma Preelaborate;

   Sync_Code : constant := 1;

   type Sync is new Protocol (Sync_Code) with private;

   procedure Message_Received
     (This : in out Sync;
      Msg  : in     Message);

private

   type Sync is new Protocol (Sync_Code) with
      record
         State : Boolean;
      end record;

end ACO.Protocols.Synchronization;
