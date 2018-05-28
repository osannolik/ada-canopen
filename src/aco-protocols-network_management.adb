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

      function To_NMT_Command (Msg : Message) return NMT_Command is
         ((As_Raw => True,
           Raw    => Msg.Data (0 .. 1)));
   end Commands;

   procedure Send_Bootup
     (This    : in out NMT;
      Node_Id : in     Node_Nr)
   is
      Msg : constant Message := Create (Code => NMT_Error_Code,
                                        Node => Node_Id,
                                        RTR  => False,
                                        Data => (Msg_Data'First => 0));
   begin
      This.Driver.Send_Message (Msg);
   end Send_Bootup;

   procedure Set_State
     (This    : in out NMT;
      Node_Id : in     Node_Nr;
      State   : in     ACO.States.State)
   is
      use ACO.States;

      Current : constant ACO.States.State := This.Od.Get_Node_State;
   begin
      case Current is
         when Pre_Operational | Operational | Stopped =>
            This.Od.Set_Node_State (State);

         when Initializing =>
            if State = Pre_Operational then
               Send_Bootup (This, Node_Id);
               This.Od.Set_Node_State (State);
            end if;

         when Unknown_State =>
            --  ?
            This.Od.Set_Node_State (State);
      end case;
   end Set_State;

   procedure Message_Received
     (This    : in out NMT;
      Msg     : in     Message;
      Node_Id : in     Node_Nr)
   is
      use Commands;
      use ACO.States;

      Cmd : constant NMT_Command := To_NMT_Command (Msg);
   begin
      case This.Od.Get_Node_State is
         when Pre_Operational | Operational | Stopped =>
            null;

         when Initializing | Unknown_State =>
            return;
      end case;

      if Cmd.Node_Id = Node_Id or else
         Cmd.Node_Id = Broadcast_Id
      then
         case Cmd.Command_Specifier is
            when Start =>
               Set_State (This, Node_Id, Operational);

            when Stop =>
               Set_State (This, Node_Id, Stopped);

            when Pre_Op =>
               Set_State (This, Node_Id, Pre_Operational);

            when Reset_Node =>
               Set_State (This, Node_Id, Initializing);

            when Reset_Communication =>
               Set_State (This, Node_Id, Initializing);

            when others =>
               null;
         end case;
      end if;

   end Message_Received;

end ACO.Protocols.Network_Management;
