package body ACO.Protocols.Network_Management is

   function Is_Allowed_Transition
      (Current : ACO.States.State;
       Next    : ACO.States.State)
       return Boolean
   is
      use ACO.States;
   begin
      case Current is
         when Initializing =>
            return Next = Pre_Operational;

         when Pre_Operational | Operational | Stopped | Unknown_State =>
            return True;
      end case;
   end Is_Allowed_Transition;

   procedure Set
      (This  : in out NMT;
       State : in     ACO.States.State)
   is
      use ACO.States;
   begin
      if This.Od.Get_Node_State /= State then
         This.Od.Set_Node_State (State);

         case State is
            when Initializing =>
               This.Od.Set_Node_State (Pre_Operational);

            when Pre_Operational | Operational | Stopped | Unknown_State =>
               null;
         end case;
      end if;
   end Set;

   function Get
      (This  : NMT)
       return ACO.States.State
   is
   begin
      return This.Od.Get_Node_State;
   end Get;

   overriding
   function Is_Valid
      (This : in out NMT;
       Msg  : in     ACO.Messages.Message)
       return Boolean
   is
      pragma Unreferenced (This);

      use type ACO.Messages.Id_Type;
   begin
      return ACO.Messages.CAN_Id (Msg) = NMT_CAN_Id;
   end Is_Valid;

   procedure On_NMT_Command
      (This : in out NMT;
       Msg  : in     ACO.Messages.Message)
   is
      use ACO.States;
      use type ACO.Messages.Node_Nr;

      Cmd : constant NMT_Commands.NMT_Command :=
         NMT_Commands.To_NMT_Command (Msg);
   begin
      if Cmd.Node_Id = This.Id or else
         Cmd.Node_Id = ACO.Messages.Broadcast_Id
      then
         case Cmd.Command_Specifier is
            when NMT_Commands.Start =>
               This.Set (Operational);

            when NMT_Commands.Stop =>
               This.Set (Stopped);

            when NMT_Commands.Pre_Op =>
               This.Set (Pre_Operational);

            when NMT_Commands.Reset_Node | NMT_Commands.Reset_Communication =>
               This.Set (Initializing);

            when others =>
               null;
         end case;
      end if;
   end On_NMT_Command;

   procedure Message_Received
     (This : in out NMT;
      Msg  : in     ACO.Messages.Message)
   is
      use ACO.States;
   begin
      case This.Get is
         when Initializing | Unknown_State =>
            return;

         when Pre_Operational | Operational | Stopped =>
            null;
      end case;

      if NMT_Commands.Is_Valid_Command (Msg) then
         This.On_NMT_Command (Msg);
      end if;
   end Message_Received;

   overriding
   procedure Update
      (This : access Node_State_Change_Subscriber;
       Data : in     ACO.States.State_Transition)
   is
   begin
      This.Ref.NMT_Log
         (ACO.Log.Info, Data.Previous'Img & " => " & Data.Current'Img);
   end Update;

   overriding
   procedure Initialize
      (This : in out NMT)
   is
   begin
      Protocol (This).Initialize;

      This.Od.Events.Node_State_Modified.Attach
         (This.State_Change'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize
      (This : in out NMT)
   is
   begin
      Protocol (This).Finalize;

      This.Od.Events.Node_State_Modified.Detach
         (This.State_Change'Unchecked_Access);
   end Finalize;

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
