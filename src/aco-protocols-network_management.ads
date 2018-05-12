package ACO.Protocols.Network_Management is

   pragma Preelaborate;

   NMT_Code : constant := 0;

   type NMT is new Protocol (NMT_Code) with private;

   procedure Message_Received
     (This : in out NMT;
      Msg  : in     Message);

private

   type NMT is new Protocol (NMT_Code) with
      record
         State : Boolean;
      end record;

end ACO.Protocols.Network_Management;
