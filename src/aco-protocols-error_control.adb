with ACO.Events;

package body ACO.Protocols.Error_Control is

   overriding
   function Is_Valid
      (This : in out EC;
       Msg  : in     ACO.Messages.Message)
       return Boolean
   is
      pragma Unreferenced (This);

      use type ACO.Messages.Function_Code;
   begin
      return ACO.Messages.Func_Code (Msg) = EC_Id;
   end Is_Valid;

   procedure Message_Received
     (This : in out EC;
      Msg  : in     ACO.Messages.Message)
   is
   begin
      if EC_Commands.Is_Valid_Command (Msg, This.Id) then
         declare
            Hbt_State : constant EC_Commands.EC_State :=
               EC_Commands.Get_EC_State (Msg);
            Id : constant ACO.Messages.Node_Nr := ACO.Messages.Node_Id (Msg);
         begin
            This.Od.Events.Node_Events.Put
              ((Event              => ACO.Events.Heartbeat_Received,
                Received_Heartbeat =>
                  (Id    => Id,
                   State => EC_Commands.To_State (Hbt_State))));
            This.On_Heartbeat (Id, Hbt_State);
         end;
      end if;
   end Message_Received;

   procedure EC_Log
     (This    : in out EC;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
      pragma Unreferenced (This);
   begin
      ACO.Log.Put_Line (Level, "(EC) " & Message);
   end EC_Log;

end ACO.Protocols.Error_Control;
