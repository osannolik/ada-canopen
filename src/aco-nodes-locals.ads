package ACO.Nodes.Locals is

   type Local
      (Id     : ACO.Messages.Node_Nr;
       Handler : not null access ACO.CANopen.Handler'Class;
       Od     : not null access ACO.OD.Object_Dictionary'Class)
   is new Node_Base (Id, Handler, Od) with null record;

   overriding
   procedure Set_State
      (This  : in out Local;
       State : in     ACO.States.State);

   overriding
   procedure Write
      (This     : in out Local;
       Node     : in     ACO.Messages.Node_Nr;
       Index    : in     ACO.OD_Types.Object_Index;
       Subindex : in     ACO.OD_Types.Object_Subindex;
       An_Entry : in     ACO.OD_Types.Entry_Base'Class);

end ACO.Nodes.Locals;
