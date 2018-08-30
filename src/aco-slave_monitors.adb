with Ada.Real_Time;

package body ACO.Slave_Monitors is

   function Is_Monitored
      (This    : Slave_Monitor;
       Node_Id : ACO.Messages.Slave_Node_Nr)
       return Boolean
   is
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
       return State
   is
   begin
      for Alarm of This.Slaves loop
         if Alarm.Node_Id = Node_Id then
            return Alarm.Slave_State.Current;
         end if;
      end loop;

      return Unknown_State;
   end Get_State;

   overriding
   procedure Signal (This : access Slave_Alarm)
   is
   begin
      This.Node_Id := Not_A_Slave;
      This.Slave_State := (Previous => This.Slave_State.Current,
                           Current  => Unknown_State);
      if This.Monitor_Ref /= null then
         This.Monitor_Ref.Od.Events.Slave_State_Change.Update (This.Slave_State);
      end if;
   end Signal;

   procedure Restart
      (This : in out Slave_Monitor)
   is
      use Ada.Real_Time;
      Period : Natural;
   begin
      for Alarm of This.Slaves loop
         if Alarm.Node_Id /= Not_A_Slave then
            This.Manager.Cancel (Alarm'Unchecked_Access);
            Period := This.Od.Get_Heartbeat_Consumer_Period (Alarm.Node_Id);

            if Period > 0 then
               This.Manager.Set
                  (Alarm'Unchecked_Access, Clock + Milliseconds (Period));
            else
               Alarm.Node_Id := Not_A_Slave;
            end if;
         end if;
      end loop;
   end Restart;

   procedure Start
      (This        : in out Slave_Monitor;
       Node_Id     : in     ACO.Messages.Slave_Node_Nr;
       Slave_State : in     State)
   is
      use Ada.Real_Time;
      Period : constant Natural := This.Od.Get_Heartbeat_Consumer_Period (Node_Id);
   begin
      if Period > 0 then
         for Alarm of This.Slaves loop
            if Alarm.Node_Id = Not_A_Slave then
               Alarm.Node_Id := Node_Id;
               Alarm.Slave_State := (Previous => Unknown_State,
                                     Current  => Slave_State);
               This.Manager.Set
                  (Alarm'Unchecked_Access, Clock + Milliseconds (Period));
               This.Od.Events.Slave_State_Change.Update (Alarm.Slave_State);

               exit;
            end if;
         end loop;
      end if;
   end Start;

   procedure Update_State
      (This        : in out Slave_Monitor;
       Node_Id     : in     ACO.Messages.Slave_Node_Nr;
       Slave_State : in     State)
   is
      use Ada.Real_Time;
      Period : constant Natural := This.Od.Get_Heartbeat_Consumer_Period (Node_Id);
   begin
      for Alarm of This.Slaves loop
         if Alarm.Node_Id = Node_Id then
            This.Manager.Cancel (Alarm'Unchecked_Access);

            if Period > 0 then
               Alarm.Slave_State := (Previous => Alarm.Slave_State.Current,
                                     Current  => Slave_State);
               This.Manager.Set
                  (Alarm'Unchecked_Access, Clock + Milliseconds (Period));
               This.Od.Events.Slave_State_Change.Update (Alarm.Slave_State);
            else
               Alarm.Node_Id := Not_A_Slave;
            end if;

            exit;
         end if;
      end loop;
   end Update_State;

   procedure Update_Alarms
      (This : in out Slave_Monitor)
   is
   begin
      This.Manager.Process;
   end Update_Alarms;

end ACO.Slave_Monitors;
