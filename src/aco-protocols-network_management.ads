with ACO.Messages;
with ACO.Drivers;

private with Interfaces;

package ACO.Protocols.Network_Management is

   pragma Preelaborate;

   use ACO.Messages;

   NMT_CAN_Id : constant Id_Type := 0;

   type NMT (Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with private;

   procedure Message_Received
     (This : in out NMT;
      Msg  : in     Message);

private

--     type Command_Specifier is
--       (Start,
--        Stop,
--        Pre_Operational,
--        Reset_Node,
--        Reset_Communication);

--     type CAN_Id_Type (As_Id : Boolean := False) is record
--        case As_Id is
--           when True =>
--              Id : Id_Type;
--           when False =>
--              Code : Function_Code;
--              Node : Node_Nr;
--        end case;
--     end record
--       with Unchecked_Union, Size => 11, Bit_Order => System.Low_Order_First;
--
--     for CAN_Id_Type use record
--        Id   at 0 range 0 .. 10;
--        Code at 0 range 7 .. 10;
--        Node at 0 range 0 .. 6;
--     end record;

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
   Pre_Operational     : constant := 128;
   Reset_Node          : constant := 129;
   Reset_Communication : constant := 130;

   function To_NMT_Command (Msg : Message) return NMT_Command is
     ((As_Raw => True,
       Raw    => Msg.Data (0 .. 1)));

--     for Command_Specifier use
--        (Start => 1,
--         Stop => 2,
--         Pre_Operational => 128,
--         Reset_Node => 129,
--         Reset_Communication => 130);

--     type NMT_Command is record
--        Command_Spec : Command_Specifier;
--        Node_Id      : Node_Nr;
--     end record;

--     function Command (Data : Msg_Data) return NMT_Command is
--       ((Command_Spec => Command_Specifier'Val (Data (Data'First)),
--         Node_Id      => Node_Nr (Data (Data'First + 1))));

   type NMT (Driver : not null access ACO.Drivers.Driver'Class) is
      new Protocol with null record;

end ACO.Protocols.Network_Management;
