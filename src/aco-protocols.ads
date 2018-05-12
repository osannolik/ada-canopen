with ACO.Messages;

package ACO.Protocols is

   pragma Preelaborate;

   use ACO.Messages;

   type Protocol (Code : Function_Code) is abstract tagged limited null record;

   type Protocol_Access is access all Protocol'Class;

end ACO.Protocols;
