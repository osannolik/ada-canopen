package body ACO.Protocols.Error_Control.Masters is

   procedure Periodic_Actions
      (This  : in out Master;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.Timers.Process (T_Now);
      This.Monitor.Update_Alarms (T_Now);
   end Periodic_Actions;

   function Create_Heartbeat
      (State   : ACO.States.State;
       Node_Id : ACO.Messages.Node_Nr)
      return ACO.Messages.Message
   is
   begin
      return ACO.Messages.Create
         (Code => EC_Id,
          Node => Node_Id,
          RTR  => False,
          Data =>
             (ACO.Messages.Msg_Data'First =>
                    ACO.Messages.Data_Type (EC_Commands.To_EC_State (State))));
   end Create_Heartbeat;

   procedure Send_Heartbeat
      (This       : in out Master;
       Node_State : in     ACO.States.State)
   is
   begin
      This.Handler.Put (Create_Heartbeat (Node_State, This.Id));
   end Send_Heartbeat;

   procedure Send_Bootup
      (This : in out Master)
   is
   begin
      This.EC_Log (ACO.Log.Debug, "Sending bootup for node" & This.Id'Img);
      This.Send_Heartbeat (ACO.States.Initializing);
   end Send_Bootup;

   overriding
   procedure Signal
      (This  : access Heartbeat_Producer_Alarm;
       T_Now : in     Ada.Real_Time.Time)
   is
      use type Ada.Real_Time.Time;
      use Alarms;

      Ref : access Master renames This.Ref;

      Period : constant Natural := Ref.Od.Get_Heartbeat_Producer_Period;
   begin
      if Period > 0 then
         Ref.Timers.Set
            (Alarm_Access (This), T_Now + Ada.Real_Time.Milliseconds (Period));
         Ref.Send_Heartbeat (Ref.Od.Get_Node_State);
      end if;
   end Signal;

   procedure Heartbeat_Producer_Start
      (This : in out Master)
   is
      Period : constant Natural := This.Od.Get_Heartbeat_Producer_Period;
      Immediately : constant Ada.Real_Time.Time := This.Handler.Current_Time;
   begin
      if Period > 0 then
         This.Timers.Set
            (Alarm       => This.Producer_Alarm'Unchecked_Access,
             Signal_Time => Immediately);
      end if;
   end Heartbeat_Producer_Start;

   procedure Heartbeat_Producer_Stop
      (This : in out Master)
   is
   begin
      This.Timers.Cancel (This.Producer_Alarm'Unchecked_Access);
   end Heartbeat_Producer_Stop;

   overriding
   procedure Update
      (This : access Node_State_Change_Subscriber;
       Data : in     ACO.States.State_Transition)
   is
      use ACO.States;
      Ref : access Master renames This.Ref;
   begin
      case Data.Current is
         when Initializing | Unknown_State =>
            Ref.Heartbeat_Producer_Stop;

         when Pre_Operational =>
            if Data.Previous = Initializing then
               Ref.Send_Bootup;
               Ref.Heartbeat_Producer_Start;
            end if;

         when Operational | Stopped =>
            null;
      end case;
   end Update;

   procedure On_Heartbeat
      (This      : in out Master;
       Id        : in     ACO.Messages.Node_Nr;
       Hbt_State : in     EC_Commands.EC_State)
   is
      State : constant ACO.States.State := EC_Commands.To_State (Hbt_State);
      T_Now : constant Ada.Real_Time.Time := This.Handler.Current_Time;
   begin
      if This.Monitor.Is_Monitored (Id) then
         This.Monitor.Update_State (Id, State, T_Now);
      elsif EC_Commands.Is_Bootup (Hbt_State) then
         This.Monitor.Start (Id, State, T_Now);
      end if;
   end On_Heartbeat;

   overriding
   procedure Update
      (This : access Entry_Update_Subscriber;
       Data : in     ACO.OD_Types.Entry_Index)
   is
      Ref : access Master renames This.Ref;
   begin
      case Data.Object is
         when ACO.OD.Heartbeat_Consumer_Index =>
            Ref.Monitor.Restart (Ref.Handler.Current_Time);

         when ACO.OD.Heartbeat_Producer_Index =>
            if Ref.Timers.Is_Pending (Ref.Producer_Alarm'Access) then
               Ref.Heartbeat_Producer_Stop;
               Ref.Heartbeat_Producer_Start;
            end if;

         when others => null;
      end case;
   end Update;

   overriding
   procedure Initialize
      (This : in out Master)
   is
   begin
      EC (This).Initialize;

      This.Od.Events.Entry_Updated.Attach
         (This.Entry_Update'Unchecked_Access);
      This.Od.Events.Node_State_Modified.Attach
         (This.State_Change'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize
      (This : in out Master)
   is
   begin
      EC (This).Finalize;

      This.Od.Events.Entry_Updated.Detach
         (This.Entry_Update'Unchecked_Access);
      This.Od.Events.Node_State_Modified.Detach
         (This.State_Change'Unchecked_Access);
   end Finalize;

end ACO.Protocols.Error_Control.Masters;
