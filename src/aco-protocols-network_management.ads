with ACO.Messages;
with ACO.OD;
with ACO.States;

private with ACO.Log;
private with Interfaces;

package ACO.Protocols.Network_Management is

   use ACO.Messages;

   NMT_CAN_Id : constant Id_Type := 0;

   type NMT (Od : not null access ACO.OD.Object_Dictionary'Class) is
      new Protocol (Od) with null record;

   procedure Message_Received
     (This    : in out NMT;
      Msg     : in     Message;
      Node_Id : in     Node_Nr);

   procedure Set_State
     (This  : in out NMT;
      State : in     ACO.States.State);

private

   overriding
   procedure On_State_Change
     (This     : in out NMT;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure NMT_Log
     (This    : in out NMT;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

   package Commands is
      type Cmd_Spec_Type is new Interfaces.Unsigned_8;

      type NMT_Command (As_Raw : Boolean := False) is record
         case As_Raw is
            when True =>
               Raw : Data_Array (0 .. 1);
            when False =>
               Command_Specifier : Cmd_Spec_Type;
               Node_Id           : Node_Nr;
         end case;
      end record
         with Unchecked_Union, Size => 16;

      for NMT_Command use record
         Raw               at 0 range 0 .. 15;
         Command_Specifier at 0 range 0 .. 7;
         Node_Id           at 0 range 8 .. 15;
      end record;

      Start               : constant := 1;
      Stop                : constant := 2;
      Pre_Op              : constant := 128;
      Reset_Node          : constant := 129;
      Reset_Communication : constant := 130;

      function Is_Valid_Cmd_Spec (Cmd : Cmd_Spec_Type) return Boolean is
         (Cmd = Start      or else
          Cmd = Stop       or else
          Cmd = Pre_Op     or else
          Cmd = Reset_Node or else
          Cmd = Reset_Communication);

      function To_NMT_Command (Msg : Message) return NMT_Command is
         ((As_Raw => True,
           Raw    => Msg.Data (0 .. 1)));

      function Is_Valid_Command (Msg : Message) return Boolean is
         ((Msg.Length = NMT_Command'Size / 8) and then
          Is_Valid_Cmd_Spec (To_NMT_Command (Msg).Command_Specifier));

   end Commands;

end ACO.Protocols.Network_Management;
