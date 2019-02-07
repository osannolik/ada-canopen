with ACO.OD;
with ACO.States;

private with ACO.Events;
private with ACO.Log;
private with Interfaces;

package ACO.Protocols.Network_Management is

   NMT_CAN_Id : constant ACO.Messages.Id_Type := 0;

   type NMT
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Protocol with private;

   overriding
   function Is_Valid
      (This : in out NMT;
       Msg  : in     ACO.Messages.Message)
       return Boolean;

   procedure Message_Received
      (This : in out NMT;
       Msg  : in     ACO.Messages.Message)
      with Pre => This.Is_Valid (Msg);

   function Is_Allowed_Transition
      (Current : ACO.States.State;
       Next    : ACO.States.State)
       return Boolean;

   procedure Set
      (This  : in out NMT;
       State : in     ACO.States.State)
      with Pre => Is_Allowed_Transition (This.Od.Get_Node_State, State);

   function Get
      (This  : NMT)
       return ACO.States.State;

private

   procedure NMT_Log
     (This    : in out NMT;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

   package NMT_Commands is
      type Cmd_Spec_Type is new Interfaces.Unsigned_8;

      type NMT_Command (As_Raw : Boolean := False) is record
         case As_Raw is
            when True =>
               Raw : ACO.Messages.Data_Array (0 .. 1);
            when False =>
               Command_Specifier : Cmd_Spec_Type;
               Node_Id           : ACO.Messages.Node_Nr;
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

      To_CMD_Spec : constant array (ACO.States.State) of Cmd_Spec_Type :=
         (ACO.States.Initializing | ACO.States.Unknown_State => Reset_Node,
          ACO.States.Pre_Operational                         => Pre_Op,
          ACO.States.Operational                             => Start,
          ACO.States.Stopped                                 => Stop);

      function To_NMT_Command (Msg : ACO.Messages.Message) return NMT_Command is
         ((As_Raw => True,
           Raw    => Msg.Data (0 .. 1)));

      function To_Msg (Cmd : NMT_Command) return ACO.Messages.Message is
         (ACO.Messages.Create (CAN_Id => NMT_CAN_Id,
                               RTR    => False,
                               Data   => Cmd.Raw));

      function Is_Valid_Command (Msg : ACO.Messages.Message) return Boolean is
         ((Msg.Length = NMT_Command'Size / 8) and then
          Is_Valid_Cmd_Spec (To_NMT_Command (Msg).Command_Specifier));

   end NMT_Commands;

   type Node_State_Change_Subscriber
      (Ref : not null access NMT)
   is new ACO.Events.Event_Listener (ACO.Events.State_Transition)
   with null record;

   overriding
   procedure On_Event
      (This : in out Node_State_Change_Subscriber;
       Data : in     ACO.Events.Event_Data);

   type NMT
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Protocol (Od) with record
      State_Change : aliased Node_State_Change_Subscriber (NMT'Access);
   end record;

   overriding
   procedure Initialize (This : in out NMT);

   overriding
   procedure Finalize (This : in out NMT);

   procedure On_NMT_Command
      (This : in out NMT;
       Msg  : in     ACO.Messages.Message);

end ACO.Protocols.Network_Management;
