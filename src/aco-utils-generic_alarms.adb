package body ACO.Utils.Generic_Alarms is

   function Get_Next_Up
      (This : Alarm_Manager)
       return Alarm_Access
   is
      use Ada.Real_Time;
   begin
      if not This.Alarm_List.Is_Empty then
         declare
            Next : constant Alarm_Data := This.Alarm_List.Get_First;
         begin
            if Next.Alarm_Ref /= No_Alarm and then
               Next.Signal_Time <= Clock
            then
               return Next.Alarm_Ref;
            end if;
         end;
      end if;

      return No_Alarm;
   end Get_Next_Up;

   procedure Process
      (This : in out Alarm_Manager)
   is
      Next : Alarm_Access := This.Get_Next_Up;
   begin
      while Next /= No_Alarm loop
         This.Cancel (Next);
         Next.Signal;
         Next := This.Get_Next_Up;
      end loop;
   end Process;

   procedure Set
     (This        : in out Alarm_Manager;
      Alarm       : in     Alarm_Access;
      Signal_Time : in     Ada.Real_Time.Time)
   is
   begin
      This.Alarm_List.Add ((Alarm, Signal_Time));
   end Set;

   function Is_Pending
     (This  : in out Alarm_Manager;
      Alarm : in     Alarm_Access)
      return Boolean
   is
   begin
      return This.Alarm_List.Is_Item_In_List
         ((Alarm_Ref   => Alarm,
           Signal_Time => Ada.Real_Time.Time_Last));
   end Is_Pending;

   procedure Cancel
     (This  : in out Alarm_Manager;
      Alarm : in     Alarm_Access)
   is
   begin
      This.Alarm_List.Remove
         ((Alarm_Ref   => Alarm,
           Signal_Time => Ada.Real_Time.Time_Last));
   end Cancel;

   function "<" (Left, Right : Alarm_Data) return Boolean
   is
      use Ada.Real_Time;
   begin
      return Left.Signal_Time < Right.Signal_Time;
   end "<";

   function "=" (Left, Right : Alarm_Data) return Boolean
   is
   begin
      return Left.Alarm_Ref = Right.Alarm_Ref;
   end "=";

end ACO.Utils.Generic_Alarms;
