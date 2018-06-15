with Interfaces;

package body ACO.Protocols.Network_Management is

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

      function Is_Valid_Command (Msg : Message) return Boolean is
        (Msg.Length = NMT_Command'Size / 8);

      function To_NMT_Command (Msg : Message) return NMT_Command is
        ((As_Raw => True,
          Raw    => Msg.Data (0 .. 1)));
   end Commands;

   procedure Set_State
     (This  : in out NMT;
      State : in     ACO.States.State)
   is
      use ACO.States;

      Current : constant ACO.States.State := This.Od.Get_Node_State;
      Next    : ACO.States.State := Current;
   begin
      case Current is
         when Pre_Operational | Operational | Stopped =>
            Next := State;

         when Initializing =>
            if State = Pre_Operational then
               Next := State;
            end if;

         when Unknown_State =>
            --  ?
            Next := State;
      end case;

      if Next /= Current then
         This.Od.Set_Node_State (Next);
      end if;

      case Next is
         when Initializing =>
            This.Od.Set_Node_State (Pre_Operational);

         when Pre_Operational | Operational | Stopped | Unknown_State =>
            null;
      end case;
   end Set_State;

   overriding
   procedure On_State_Change
     (This     : in out NMT;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State)
   is
   begin
      This.NMT_Log (ACO.Log.Info, Previous'Img & " => " & Current'Img);
   end On_State_Change;

   procedure Message_Received
     (This    : in out NMT;
      Msg     : in     Message;
      Node_Id : in     Node_Nr)
   is
      use Commands;
      use ACO.States;

      Cmd : NMT_Command;
   begin
      if not Is_Valid_Command (Msg) then
         return;
      end if;

      case This.Od.Get_Node_State is
         when Initializing | Unknown_State =>
            return;

         when Pre_Operational | Operational | Stopped =>
            null;
      end case;

      Cmd := To_NMT_Command (Msg);

      if Cmd.Node_Id = Node_Id or else
         Cmd.Node_Id = Broadcast_Id
      then
         case Cmd.Command_Specifier is
            when Start =>
               This.Set_State (Operational);

            when Stop =>
               This.Set_State (Stopped);

            when Pre_Op =>
               This.Set_State (Pre_Operational);

            when Reset_Node | Reset_Communication =>
               This.Set_State (Initializing);

            when others =>
               null;
         end case;
      end if;

   end Message_Received;

   procedure NMT_Log
     (This    : in out NMT;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String)
   is
      pragma Unreferenced (This);
   begin
      ACO.Log.Put_Line (Level, "(NMT) " & Message);
   end NMT_Log;

end ACO.Protocols.Network_Management;
