package ACO.Protocols is

   pragma Preelaborate;

   type Protocol is abstract tagged limited null record;

   type Protocol_Access is access all Protocol'Class;

end ACO.Protocols;
