package ACO.Protocols.Error_Control.Slaves is

   type Slave
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is new EC with private;

private

   type Slave
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is new EC (Id, Od) with null record;

   procedure On_State_Change
      (This     : in out Slave;
       Previous : in     ACO.States.State;
       Current  : in     ACO.States.State) is null;

end ACO.Protocols.Error_Control.Slaves;
