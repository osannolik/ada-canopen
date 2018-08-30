with ACO.States;
with ACO.Messages;
with ACO.OD;

private with ACO.Utils.Generic_Alarms;
private with ACO.Configuration;

package ACO.Slave_Monitors is

   use ACO.States;

   type Slave_Monitor (Od : not null access ACO.OD.Object_Dictionary'Class) is
      tagged limited private;

   type Slave_Monitor_Ref is access all Slave_Monitor'Class;

   function Is_Monitored
      (This    : Slave_Monitor;
       Node_Id : ACO.Messages.Slave_Node_Nr)
       return Boolean;

   function Get_State
      (This    : Slave_Monitor;
       Node_Id : ACO.Messages.Slave_Node_Nr)
       return State
      with Pre => This.Is_Monitored (Node_Id);

   procedure Restart
      (This : in out Slave_Monitor);

   procedure Start
      (This        : in out Slave_Monitor;
       Node_Id     : in     ACO.Messages.Slave_Node_Nr;
       Slave_State : in     State)
      with Pre => not This.Is_Monitored (Node_Id);

   procedure Update_State
      (This        : in out Slave_Monitor;
       Node_Id     : in     ACO.Messages.Slave_Node_Nr;
       Slave_State : in     State)
      with Pre => This.Is_Monitored (Node_Id);

   procedure Update_Alarms
      (This : in out Slave_Monitor);

private
   use ACO.Configuration;
   use ACO.Messages;

   package Alarms is new ACO.Utils.Generic_Alarms
      (Maximum_Nof_Alarms => Max_Nof_Heartbeat_Slaves);

   type Slave_Alarm (Monitor_Ref : access Slave_Monitor'Class := null) is
      new Alarms.Alarm_Type with
   record
      Node_Id     : Node_Nr;
      Slave_State : State_Transition;
   end record;

   overriding
   procedure Signal (This : access Slave_Alarm);

   type Slaves_Array is array (Positive range <>) of aliased Slave_Alarm;

   type Slave_Monitor (Od : not null access ACO.OD.Object_Dictionary'Class) is
      tagged limited
   record
      Manager : Alarms.Alarm_Manager;
      Slaves : Slaves_Array (1 .. Max_Nof_Heartbeat_Slaves) :=
         (others => (Slave_Monitor'Access,
                     Not_A_Slave,
                     (Unknown_State, Unknown_State)));
   end record;

end ACO.Slave_Monitors;
