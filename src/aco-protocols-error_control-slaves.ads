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

end ACO.Protocols.Error_Control.Slaves;
