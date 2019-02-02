with ACO.Messages;
with ACO.OD;
with ACO.States;

private with ACO.Log;
private with Interfaces;

package ACO.Protocols.Error_Control is

   EC_Id : constant ACO.Messages.Function_Code := 16#E#;

   type EC
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Protocol with private;

   overriding
   function Is_Valid
      (This : in out EC;
       Msg  : in     ACO.Messages.Message)
       return Boolean;

   procedure Message_Received
     (This : in out EC;
      Msg  : in     ACO.Messages.Message)
      with Pre => This.Is_Valid (Msg);

private

   package EC_Commands is
      use Interfaces;
      use ACO.States;
      use type ACO.Messages.Node_Nr;
      use type ACO.Messages.Function_Code;

      type EC_State is new Interfaces.Unsigned_8;

      Bootup : constant := 0;
      Stop   : constant := 4;
      Op     : constant := 5;
      Pre_Op : constant := 127;

      To_EC_State : constant array (ACO.States.State) of EC_State :=
         (Unknown_State | Initializing => Bootup,
          Pre_Operational              => Pre_Op,
          Operational                  => Op,
          Stopped                      => Stop);

      function Get_EC_State (Msg : ACO.Messages.Message) return EC_State is
         (EC_State (Msg.Data (0)));

      function To_State (S : EC_State) return ACO.States.State is
         (case S is
             when Bootup => Initializing,
             when Pre_Op => Pre_Operational,
             when Op     => Operational,
             when Stop   => Stopped,
             when others => Unknown_State);

      function Get_State (Msg : ACO.Messages.Message) return ACO.States.State is
         (To_State (Get_EC_State (Msg)));

      function Is_Valid_Command
         (Msg : ACO.Messages.Message; Id : ACO.Messages.Node_Nr)
          return Boolean
      is
         (ACO.Messages.Func_Code (Msg) = EC_Id and then
          ACO.Messages.Node_Id (Msg) = Id and then
          Msg.Length = EC_State'Size / 8);

      function Is_Bootup (S : EC_State) return Boolean is
         (S = Bootup);

      function Is_Bootup (Msg : ACO.Messages.Message) return Boolean is
         (Get_EC_State (Msg) = Bootup);

   end EC_Commands;

   type EC
      (Id : ACO.Messages.Node_Nr;
       Od : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Protocol (Od) with null record;

   procedure On_Heartbeat
      (This      : in out EC;
       Id        : in     ACO.Messages.Node_Nr;
       Hbt_State : in     EC_Commands.EC_State) is null;

   procedure EC_Log
     (This    : in out EC;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

end ACO.Protocols.Error_Control;
