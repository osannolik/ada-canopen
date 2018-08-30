package ACO.States is

   pragma Preelaborate;

   type State is
     (Initializing,
      Pre_Operational,
      Operational,
      Stopped,
      Unknown_State);

   type State_Transition is record
      Previous : State := Unknown_State;
      Current  : State := Unknown_State;
   end record;

end ACO.States;
