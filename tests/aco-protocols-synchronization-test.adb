with AUnit.Assertions; use AUnit.Assertions;

with ACO.Drivers.Dummy;
with Ada.Real_Time;

package body ACO.Protocols.Synchronization.Test is
   pragma Assertion_Policy (Check);

   use ACO.Drivers.Dummy;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Synchronization Test");
   end Name;

   procedure Let_Time_Pass
     (S       : in out SYNC;
      Time_Ms : in     Natural)
   is
      use Ada.Real_Time;
      Next_Release : Time := Clock;
   begin
      for T in 1 .. Time_Ms loop
         Next_Release := Next_Release + Milliseconds (1);
         delay until Next_Release;
         S.Update_Alarms;
      end loop;
   end Let_Time_Pass;

   function Nof_Sent (S : SYNC) return Natural
   is
   begin
      return Nof_Sent (Dummy_Driver (S.Driver.all));
   end Nof_Sent;

   procedure Sync_Consumer_Test is
   begin
      null;  --  TODO
   end Sync_Consumer_Test;

   procedure Sync_Producer_Test (Period : in Natural) is
      OD     : aliased ACO.OD.Object_Dict;
      Driver : aliased ACO.Drivers.Dummy.Dummy_Driver;
      S      : SYNC (OD'Access, Driver'Access);

      Period_Ms : constant Natural := Period / 10;
      Margin : constant Natural := Period_Ms / 2;

      Msg : Message;
   begin
      S.Od.Set_Communication_Cycle_Period (Period);
      S.Od.Set_Sync_Counter_Overflow (4);

      Let_Time_Pass (S, Period_Ms + Margin);
      Assert (Nof_Sent (S) = 0, "Sync sent before start");

      S.Od.Set_Node_State (ACO.States.Initializing);
      S.Od.Set_Node_State (ACO.States.Pre_Operational);

      Let_Time_Pass (S, Period_Ms);

      if Period_Ms > 0 then
         Assert (Nof_Sent (S) = 1, "Sync not sent after start");
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         Assert (Msg.Length = 1, "Sync has wrong size, counter not included");

         S.Od.Set_Node_State (ACO.States.Operational);
         Let_Time_Pass (S, Period_Ms);

         Assert (Nof_Sent (S) = 1, "Sync not sent after start");
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      else
         Assert (Nof_Sent (S) = 0, "Sync sent although disabled by Period = 0");
      end if;

      S.Od.Set_Node_State (ACO.States.Stopped);
      Let_Time_Pass (S, Period_Ms);
      Assert (Nof_Sent (S) = 0, "Sync sent after stopping");
   end Sync_Producer_Test;

   procedure Sync_Producer_Counter_Test (Period : in Natural) is
      use Interfaces;
      use ACO.States;

      OD     : aliased ACO.OD.Object_Dict;
      Driver : aliased ACO.Drivers.Dummy.Dummy_Driver;
      S      : SYNC (OD'Access, Driver'Access);

      Period_Ms : constant Natural := Period / 10;
      Margin : constant Natural := Period_Ms / 2;
      Overflow : constant := 3;

      Msg : Message;
   begin
      S.Od.Set_Communication_Cycle_Period (Period);
      S.Od.Set_Sync_Counter_Overflow (Overflow);

      Let_Time_Pass (S, Period_Ms + Margin);
      Assert (Nof_Sent (S) = 0, "Sync sent before start");

      --  Initialization => Operational
      S.Od.Set_Node_State (Initializing);
      S.Od.Set_Node_State (Operational);
      Let_Time_Pass (S, Period_Ms);
      Assert (Nof_Sent (S) = 1, "Incorrect number of syncs sent");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      Assert (Msg.Data (Msg_Data'First) = 1, "Counter not 1 at start");

      --  Operational => Initialization
      S.Od.Set_Node_State (Initializing);
      Let_Time_Pass (S, Period_Ms);
      Assert (Nof_Sent (S) = 0, "Syncs sent when expected inactive");

      --  Initialization => Pre-Operational
      S.Od.Set_Node_State (Pre_Operational);
      Let_Time_Pass (S, 2 * Period_Ms + Margin);
      Assert (Nof_Sent (S) = 2, "Incorrect number of syncs sent");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      Assert (Msg.Data (Msg_Data'First) = 1,
              "Counter did not reset due to bootup");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      Assert (Msg.Data (Msg_Data'First) = 2, "Counter did not increment");

      --  Pre-Operational => Initialization
      S.Od.Set_Node_State (Initializing);
      Let_Time_Pass (S, Period_Ms);
      Assert (Nof_Sent (S) = 0, "Syncs sent when expected inactive");

      --  Initialization => Stopped
      S.Od.Set_Node_State (Stopped);
      Let_Time_Pass (S, Period_Ms);
      Assert (Nof_Sent (S) = 0, "Syncs sent when expected inactive");

      --  Stopped => Operational
      S.Od.Set_Node_State (Operational);
      Let_Time_Pass (S, 2 * Period_Ms + Margin);
      Assert (Nof_Sent (S) = 2, "Incorrect number of syncs sent");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      Assert (Msg.Data (Msg_Data'First) = 1,
              "Counter did not reset due to Stopped -> Operational");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      Assert (Msg.Data (Msg_Data'First) = 2, "Counter did not increment");

      --  Operational => Stopped
      S.Od.Set_Node_State (Stopped);
      Let_Time_Pass (S, Period_Ms);
      Assert (Nof_Sent (S) = 0, "Syncs sent when expected inactive");

      --  Stopped => Pre-Operational
      S.Od.Set_Node_State (Pre_Operational);
      Let_Time_Pass (S, 2 * Period_Ms + Margin);
      Assert (Nof_Sent (S) = 2, "Incorrect number of syncs sent");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      Assert (Msg.Data (Msg_Data'First) = 1,
              "Counter did not reset due to Stopped -> Pre-Operational");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
      Assert (Msg.Data (Msg_Data'First) = 2, "Counter did not increment");

      --  Pre-Operational => Operational
      S.Od.Set_Node_State (Operational);
      for I in 0 .. 1 loop
         Let_Time_Pass (S, Period_Ms);
         Assert (Nof_Sent (S) = 1, "Incorrect number of syncs sent");
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         Assert (Msg.Length = 1, "Sync has wrong size, counter not included");
         Assert (Natural (Msg.Data (Msg_Data'First)) =
                    Natural'Max (1, (3 + I) mod (Overflow + 1)),
                 "Counter did not increment or wrap at" & Overflow'Img);
      end loop;

      S.Od.Set_Sync_Counter_Overflow (0);
      Let_Time_Pass (S, Period_Ms + Margin);
      Assert (Nof_Sent (S) = 1, "Incorrect number of syncs sent");
      Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
      Assert (Msg.Length = 0, "Sync has wrong size, counter is included");
   end Sync_Producer_Counter_Test;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Sync_Producer_Test (1_00);
      Sync_Producer_Test (0);
      Sync_Producer_Counter_Test (1_00);
      Sync_Consumer_Test;
   end Run_Test;

end ACO.Protocols.Synchronization.Test;
