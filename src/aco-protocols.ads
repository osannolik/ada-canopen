with Ada.Finalization;
with ACO.Messages;
with ACO.OD;

package ACO.Protocols is

   type Protocol
      (Od : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Ada.Finalization.Limited_Controlled with private;

   type Protocol_Access is access all Protocol'Class;

   function Is_Valid
      (This : in out Protocol;
       Msg  : in     ACO.Messages.Message)
       return Boolean is abstract;

private

   type Protocol
      (Od : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Ada.Finalization.Limited_Controlled with null record;

end ACO.Protocols;
