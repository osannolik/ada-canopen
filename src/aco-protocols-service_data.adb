
package body ACO.Protocols.Service_Data is

   overriding
   procedure On_State_Change
     (This     : in out SDO;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State)
   is
      pragma Unreferenced (This, Previous, Current);
   begin
      null;
   end On_State_Change;

   procedure Message_Received
     (This : in out SDO;
      Msg  : in     Message)
   is
      pragma Unreferenced (Msg);
      use ACO.States;
   begin
      case This.Od.Get_Node_State is
         when Initializing | Unknown_State | Stopped =>
            return;

         when Pre_Operational | Operational =>
            null;
      end case;
   end Message_Received;

   procedure Periodic_Actions
     (This : in out SDO)
   is
   begin
      This.Event_Manager.Process;
   end Periodic_Actions;

   overriding
   procedure Initialize (This : in out SDO)
   is
   begin
      Protocol (This).Initialize;
   end Initialize;

   overriding
   procedure Finalize (This : in out SDO)
   is
   begin
      Protocol (This).Finalize;
   end Finalize;

   procedure SDO_Log
     (This    : in out SDO;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
      pragma Unreferenced (This);
   begin
      ACO.Log.Put_Line (Level, "(SDO) " & Message);
   end SDO_Log;

end ACO.Protocols.Service_Data;
