package ACO.Protocols.Emergency is

   pragma Preelaborate;

   EMCY_Code : constant := 1;

   type Emcy is new Protocol (EMCY_Code) with private;

   procedure Message_Received
     (This : in out Emcy;
      Msg  : in     Message);

private

   type Emcy is new Protocol (EMCY_Code) with
      record
         State : Boolean;
      end record;

end ACO.Protocols.Emergency;
