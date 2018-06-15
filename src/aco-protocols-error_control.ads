with ACO.Messages;
with ACO.Drivers;
with ACO.OD;

private with ACO.States;
private with ACO.Log;

package ACO.Protocols.Error_Control is

   pragma Preelaborate;

   use ACO.Messages;

   EC_Id : constant Function_Code := 16#E#;

   type EC
      (Id     : Node_Nr;
       Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with null record;

   procedure Message_Received
     (This : in out EC;
      Msg  : in     Message);

private

   procedure Send_Bootup (This : in out EC);

   overriding
   procedure On_State_Change
     (This     : in out EC;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure EC_Log
     (This    : in out EC;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

end ACO.Protocols.Error_Control;
