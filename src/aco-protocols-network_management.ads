with ACO.Messages;
with ACO.Drivers;

private with ACO.States;

package ACO.Protocols.Network_Management is

   pragma Preelaborate;

   use ACO.Messages;

   NMT_CAN_Id : constant Id_Type := 0;

   type NMT (Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   procedure Message_Received
     (This    : in out NMT;
      Msg     : in     Message;
      Node_Id : in     Node_Nr);

private

   type NMT (Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with
      record
         State : ACO.States.State := ACO.States.Initializing;
      end record;

end ACO.Protocols.Network_Management;
