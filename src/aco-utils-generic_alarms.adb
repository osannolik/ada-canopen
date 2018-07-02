package body ACO.Utils.Generic_Alarms is

   protected body List is

      procedure Add (Alarm : in Alarm_Data)
      is
      begin
         Alarm_List.Add (Alarm);
      end Add;

      procedure Remove (Alarm_Ref : in Alarm_Access)
      is
      begin
         Alarm_List.Remove
            ((Alarm_Ref   => Alarm_Ref,
              Signal_Time => Ada.Real_Time.Time_Last));
      end Remove;

      function Get_Next_Up return Alarm_Data
      is
      begin
         if Alarm_List.Is_Empty then
            return (Alarm_Ref   => No_Alarm,
                    Signal_Time => Ada.Real_Time.Time_Last);
         else
            return Alarm_List.Get_First;
         end if;
      end Get_Next_Up;

      function Is_Pending (Alarm_Ref : Alarm_Access) return Boolean
      is
      begin
         return Alarm_List.Is_Item_In_List
            ((Alarm_Ref   => Alarm_Ref,
              Signal_Time => Ada.Real_Time.Time_Last));
      end Is_Pending;

   end List;

   procedure Process
     (This : in out Alarm_Manager)
   is
      use Ada.Real_Time;

      Next : Alarm_Data := This.Alarm_List.Get_Next_Up;
   begin
      while Next.Alarm_Ref /= No_Alarm and then
            Next.Signal_Time <= Clock
      loop
         This.Alarm_List.Remove (Next.Alarm_Ref);
         Next.Alarm_Ref.Signal;

         Next := This.Alarm_List.Get_Next_Up;
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
      return This.Alarm_List.Is_Pending (Alarm);
   end Is_Pending;

   procedure Cancel
     (This  : in out Alarm_Manager;
      Alarm : in     Alarm_Access)
   is
   begin
      This.Alarm_List.Remove (Alarm);
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
