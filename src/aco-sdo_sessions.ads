package ACO.SDO_Sessions is

   type SDO_Session is record
      X : Boolean;
   end record;

   subtype Endpoint_Nr is Integer range -1 .. Integer'Last;

   No_Endpoint : constant Endpoint_Nr := Endpoint_Nr'First;

   procedure Dummy
      (Session : SDO_Session);

end ACO.SDO_Sessions;
