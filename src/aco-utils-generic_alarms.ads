with Ada.Real_Time;
private with ACO.Utils.Generic_Sorted_List;

generic
   Maximum_Nof_Alarms : Positive;

package ACO.Utils.Generic_Alarms is

   type Alarm_Type is abstract tagged limited null record;

   type Alarm_Access is access all Alarm_Type'Class;

   procedure Signal
      (This  : access Alarm_Type;
       T_Now : in     Ada.Real_Time.Time) is abstract;

   type Alarm_Manager is tagged limited private;

   procedure Set
     (This        : in out Alarm_Manager;
      Alarm       : in     Alarm_Access;
      Signal_Time : in     Ada.Real_Time.Time);

   function Is_Pending
     (This  : in out Alarm_Manager;
      Alarm : in     Alarm_Access)
      return Boolean;

   procedure Cancel
     (This  : in out Alarm_Manager;
      Alarm : in     Alarm_Access);

   function Get_Next_Up
      (This  : Alarm_Manager;
       T_Now : Ada.Real_Time.Time)
       return Alarm_Access;

   procedure Process
      (This  : in out Alarm_Manager;
       T_Now : in     Ada.Real_Time.Time);

   No_Alarm : constant Alarm_Access := null;

private

   type Alarm_Data is record
      Alarm_Ref   : Alarm_Access       := null;
      Signal_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
   end record;

   function "<" (Left, Right : Alarm_Data) return Boolean;

   function "=" (Left, Right : Alarm_Data) return Boolean;

   package List_Pack is new ACO.Utils.Generic_Sorted_List
      (Item_Type            => Alarm_Data,
       "<"                  => "<",
       "="                  => "=",
       Maximum_Nof_Elements => Maximum_Nof_Alarms);

   type Alarm_Manager is tagged limited record
      Alarm_List : List_Pack.Sorted_List;
   end record;

end ACO.Utils.Generic_Alarms;
