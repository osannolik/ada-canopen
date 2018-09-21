with AUnit.Assertions; use AUnit.Assertions;

with ACO.Drivers.Dummy;
with Ada.Real_Time;

with ACO.OD.Example;

package body ACO.Protocols.Error_Control.Test is
   pragma Assertion_Policy (Check);

   use ACO.Drivers.Dummy;
   use Commands;

   Id : constant Node_Nr := 1;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Error Control Test");
   end Name;

   T_Now : Ada.Real_Time.Time;

   procedure Let_Time_Pass
      (E       : in out EC;
       Time_Ms : in     Natural)
   is
      use Ada.Real_Time;
   begin
      E.Od.Events.Process;

      for DT in 1 .. Time_Ms loop
         T_Now := T_Now + Milliseconds (1);
         E.Periodic_Actions (T_Now);
      end loop;
   end Let_Time_Pass;

   procedure Assert_Nof_Sent
     (E : in out EC;
      N : in     Natural;
      S : in     String := "")
   is
      Sent : constant Natural := Nof_Sent (Dummy_Driver (E.Driver.all));
   begin
      Assert (Sent = N,
              "Number of sent messages =" & Sent'Img & " /=" & N'Img & ": " & S);
   end Assert_Nof_Sent;
   pragma Unreferenced (Assert_Nof_Sent);

   function Nof_Sent (E : EC) return Natural
   is
   begin
      return Nof_Sent (Dummy_Driver (E.Driver.all));
   end Nof_Sent;

   procedure Heartbeat_Consumer_Test is
   begin
      null;  --  TODO
   end Heartbeat_Consumer_Test;

   procedure Heartbeat_Producer_Test (Period : in Natural) is
      OD_Data : aliased ACO.OD.Example.Dictionary_Data;
      OD     : aliased ACO.OD.Object_Dictionary (OD_Data'Access);
      Driver : aliased ACO.Drivers.Dummy.Dummy_Driver;
      E      : EC (Id, OD'Access, Driver'Access);

      Margin : constant Natural := Period / 2;

      -- Bootup + Heartbeats at now and now+period, else bootup only
      Nof_Expected_Msg : constant Natural := (if Period > 0 then 1 + 2 else 1);

      Msg : Message;
   begin
      T_Now := Ada.Real_Time.Clock;

      E.Od.Set_Heartbeat_Producer_Period (Period);

      Let_Time_Pass (E, Period);
      Assert (Nof_Sent (E) = 0, "Heartbeats sent before start");

      E.Od.Set_Node_State (ACO.States.Initializing);
      E.Od.Set_Node_State (ACO.States.Pre_Operational);

      Let_Time_Pass (E, Period + Margin);

      Assert (Nof_Sent (E) = Nof_Expected_Msg,
              "Heartbeats incorrectly sent after start:" & Nof_Sent (E)'Img);

      Dummy_Driver (E.Driver.all).Get_First_Sent (Msg);

      Assert (Is_Bootup (Msg)      and
              Node_Id (Msg)   = Id and
              Func_Code (Msg) = EC_Id,
              "First sent heartbeat was not bootup");

      if Period > 0 then
         Dummy_Driver (E.Driver.all).Get_First_Sent (Msg);
         Assert (Get_EC_State (Msg) = Pre_Op and
                 Node_Id (Msg)      = Id     and
                 Func_Code (Msg)    = EC_Id,
                 "Incorrect heartbeat produced");

         Dummy_Driver (E.Driver.all).Get_First_Sent (Msg);
         Assert (Get_EC_State (Msg) = Pre_Op and
                 Node_Id (Msg)      = Id     and
                 Func_Code (Msg)    = EC_Id,
                 "Incorrect heartbeat produced");

         E.Od.Set_Node_State (ACO.States.Operational);
         Let_Time_Pass (E, Period);

         Dummy_Driver (E.Driver.all).Get_First_Sent (Msg);
         Assert (Get_EC_State (Msg) = Op and
                 Node_Id (Msg)      = Id and
                 Func_Code (Msg)    = EC_Id,
                 "Incorrect heartbeat produced");

         E.Od.Set_Node_State (ACO.States.Stopped);
         Let_Time_Pass (E, Period);

         Dummy_Driver (E.Driver.all).Get_First_Sent (Msg);
         Assert (Get_EC_State (Msg) = Stop and
                 Node_Id (Msg)      = Id   and
                 Func_Code (Msg)    = EC_Id,
                 "Incorrect heartbeat produced");
      else
         Let_Time_Pass (E, Period);
         Assert (Nof_Sent (E) = 0,
                 "Heartbeats sent when disabled due to Period = 0");
      end if;

      E.Od.Set_Node_State (ACO.States.Initializing);
      Let_Time_Pass (E, Period);
      Assert (Nof_Sent (E) = 0, "Heartbeats sent when stopped");
   end Heartbeat_Producer_Test;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Heartbeat_Producer_Test (10);
      Heartbeat_Producer_Test (0);
      Heartbeat_Consumer_Test;
   end Run_Test;

end ACO.Protocols.Error_Control.Test;
