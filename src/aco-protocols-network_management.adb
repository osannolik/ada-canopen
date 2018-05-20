package body ACO.Protocols.Network_Management is

   procedure Message_Received
     (This : in out NMT;
      Msg  : in     Message)
   is
      pragma Unreferenced (This);

      Cmd : constant NMT_Command := To_NMT_Command (Msg);
   begin

      case Cmd.Command_Specifier is
         when Start => null;
         when Stop => null;
         when Pre_Operational => null;
         when Reset_Node => null;
         when Reset_Communication => null;
         when others => null;
      end case;

   end Message_Received;

end ACO.Protocols.Network_Management;
