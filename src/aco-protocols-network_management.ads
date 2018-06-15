with ACO.Messages;
with ACO.Drivers;
with ACO.OD;
with ACO.States;

private with ACO.Log;

package ACO.Protocols.Network_Management is

   pragma Preelaborate;

   use ACO.Messages;

   NMT_CAN_Id : constant Id_Type := 0;

   type NMT
      (Od     : not null access ACO.OD.Object_Dict'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with null record;

   procedure Message_Received
     (This    : in out NMT;
      Msg     : in     Message;
      Node_Id : in     Node_Nr);

   procedure Set_State
     (This    : in out NMT;
      Node_Id : in     Node_Nr;
      State   : in     ACO.States.State);

   procedure Send_Bootup
     (This    : in out NMT;
      Node_Id : in     Node_Nr);

private

   NMT_Error_Code : constant Function_Code := 16#E#;

   overriding
   procedure On_State_Change
     (This     : in out NMT;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure NMT_Log
     (This    : in out NMT;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

end ACO.Protocols.Network_Management;
