package ACO.Protocols.Network_Management.Slaves is

   type Slave
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is new NMT with private;

private

   type Slave
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is new NMT (Id, Od) with null record;

end ACO.Protocols.Network_Management.Slaves;
