package body ACO.Events is

   procedure Process
      (This : in out Event_Manager)
   is
   begin
      This.Node_State_Modified.Process;
   end Process;

end ACO.Events;
