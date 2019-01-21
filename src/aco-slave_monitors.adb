package body ACO.Slave_Monitors is

   function Is_Monitored
      (This    : Slave_Monitor;
       Node_Id : ACO.Messages.Slave_Node_Nr)
       return Boolean
   is
      use type ACO.Messages.Node_Nr;
   begin
      for Alarm of This.Slaves loop
         if Alarm.Node_Id = Node_Id then
            return True;
         end if;
      end loop;

      return False;
   end Is_Monitored;

   function Get_State
      (This    : Slave_Monitor;
       Node_Id : ACO.Messages.Slave_Node_Nr)
       return ACO.States.State
   is
      use type ACO.Messages.Node_Nr;
   begin
      for Alarm of This.Slaves loop
         if Alarm.Node_Id = Node_Id then
            return Alarm.Slave_State.Current;
         end if;
      end loop;

      return ACO.States.Unknown_State;
   end Get_State;

   overriding
   procedure Signal
      (This  : access Slave_Alarm;
       T_Now : in     Ada.Real_Time.Time)
   is
      pragma Unreferenced (T_Now);
   begin
      This.Slave_State := (Previous => This.Slave_State.Current,
                           Current  => ACO.States.Unknown_State);
      if This.Ref /= null then
         This.Ref.Od.Events.Slave_State_Change.Put (This.Slave_State);
         This.Ref.Od.Events.Heartbeat_Timed_Out.Put (This.Node_Id);
      end if;

      This.Node_Id := ACO.Messages.Not_A_Slave;
   end Signal;

   procedure Restart
      (This : in out Slave_Monitor)
   is
      use type Ada.Real_Time.Time;
      use type ACO.Messages.Node_Nr;

      Period : Natural;
   begin
      for Alarm of This.Slaves loop
         if Alarm.Node_Id /= ACO.Messages.Not_A_Slave then
            This.Manager.Cancel (Alarm'Unchecked_Access);
            Period := This.Od.Get_Heartbeat_Consumer_Period (Alarm.Node_Id);

            if Period > 0 then
               This.Manager.Set
                  (Alarm'Unchecked_Access,
                   Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (Period));
            else
               Alarm.Node_Id := ACO.Messages.Not_A_Slave;
            end if;
         end if;
      end loop;
   end Restart;

   procedure Start
      (This        : in out Slave_Monitor;
       Node_Id     : in     ACO.Messages.Slave_Node_Nr;
       Slave_State : in     ACO.States.State)
   is
      use type Ada.Real_Time.Time;
      use type ACO.Messages.Node_Nr;

      Period : constant Natural := This.Od.Get_Heartbeat_Consumer_Period (Node_Id);
   begin
      if Period > 0 then
         for Alarm of This.Slaves loop
            if Alarm.Node_Id = ACO.Messages.Not_A_Slave then
               Alarm.Node_Id := Node_Id;
               Alarm.Slave_State := (Previous => ACO.States.Unknown_State,
                                     Current  => Slave_State);
               This.Manager.Set
                  (Alarm'Unchecked_Access,
                   Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (Period));
               This.Od.Events.Slave_State_Change.Put (Alarm.Slave_State);

               exit;
            end if;
         end loop;
      end if;
   end Start;

   procedure Update_State
      (This        : in out Slave_Monitor;
       Node_Id     : in     ACO.Messages.Slave_Node_Nr;
       Slave_State : in     ACO.States.State)
   is
      use type Ada.Real_Time.Time;
      use type ACO.Messages.Node_Nr;

      Period : constant Natural := This.Od.Get_Heartbeat_Consumer_Period (Node_Id);
   begin
      for Alarm of This.Slaves loop
         if Alarm.Node_Id = Node_Id then
            This.Manager.Cancel (Alarm'Unchecked_Access);

            if Period > 0 then
               Alarm.Slave_State := (Previous => Alarm.Slave_State.Current,
                                     Current  => Slave_State);
               This.Manager.Set
                  (Alarm'Unchecked_Access,
                   Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (Period));
               This.Od.Events.Slave_State_Change.Put (Alarm.Slave_State);
            else
               Alarm.Node_Id := ACO.Messages.Not_A_Slave;
            end if;

            exit;
         end if;
      end loop;
   end Update_State;

   procedure Update_Alarms
      (This  : in out Slave_Monitor;
       T_Now : in     Ada.Real_Time.Time)
   is
   begin
      This.Manager.Process (T_Now);
   end Update_Alarms;

end ACO.Slave_Monitors;
