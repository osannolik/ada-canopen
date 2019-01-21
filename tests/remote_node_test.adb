with AUnit.Assertions; use AUnit.Assertions;

with Ada.Real_Time;

with ACO.Drivers.Dummy;
with ACO.CANopen;
with ACO.Nodes.Locals;
with ACO.Nodes.Remotes;
with ACO.OD.Example;
with ACO.Messages;
with ACO.States;

package body Remote_Node_Test is

   procedure Bus_Propagate
      (Driver_1 : in out ACO.Drivers.Dummy.Dummy_Driver;
       Driver_2 : in out ACO.Drivers.Dummy.Dummy_Driver)
   is
      N1 : constant Natural := Driver_1.Nof_Sent;
      Msg : ACO.Messages.Message;
   begin
      while Driver_2.Is_Message_Pending loop
         Driver_2.Get_First_Sent (Msg);
         Driver_1.Send_Message (Msg);
      end loop;

      for I in 1 .. N1 loop
         Driver_1.Get_First_Sent (Msg);
         Driver_2.Send_Message (Msg);
      end loop;
   end Bus_Propagate;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Remote Node Test");
   end Name;

   procedure Run_Test (T : in out Test)
   is
      pragma Unreferenced (T);
      use type ACO.States.State;

      T_Now : Ada.Real_Time.Time;

      procedure Let_Time_Pass
         (H1, H2  : in out ACO.CANopen.Handler;
          D1, D2  : in out ACO.Drivers.Dummy.Dummy_Driver;
          Time_Ms : in     Natural)
      is
         use Ada.Real_Time;
      begin
         for DT in 1 .. Time_Ms loop
            T_Now := T_Now + Milliseconds (1);

            H1.Periodic_Actions (T_Now);
            Bus_Propagate (D1, D2);
            H2.Periodic_Actions (T_Now);
            Bus_Propagate (D1, D2);
         end loop;
      end Let_Time_Pass;

      OLoc : aliased ACO.OD.Example.Dictionary;
      ORem : aliased ACO.OD.Example.Dictionary;

      DL : aliased ACO.Drivers.Dummy.Dummy_Driver;
      DR : aliased ACO.Drivers.Dummy.Dummy_Driver;

      HL : aliased ACO.CANopen.Handler (Driver => DL'Access);
      HR : aliased ACO.CANopen.Handler (Driver => DR'Access);

      L : ACO.Nodes.Locals.Local (Id => 1, Handler => HL'Access, Od => OLoc'Access);
      R : ACO.Nodes.Remotes.Remote (Id => 1, Handler => HR'Access, Od => ORem'Access);
   begin
      T_Now := Ada.Real_Time.Clock;

      -------------------------------------------------------------------------
      --  Startup Test
      -------------------------------------------------------------------------
      L.Start;
      R.Start;

      Let_Time_Pass (HL, HR, DL, DR, Time_Ms => 1);

      Assert (L.Get_State = ACO.States.Pre_Operational,
              "Expected Local to report Pre-Operational");
      Assert (R.Get_State = ACO.States.Pre_Operational,
              "Expected Remote to report Pre-Operational");

      -------------------------------------------------------------------------
      --  Change the state of the local node through the remote node object
      -------------------------------------------------------------------------
      R.Set_State (ACO.States.Operational);

      --  Wait for heartbeat local -> remote
      Let_Time_Pass (HL, HR, DL, DR,
                     Time_Ms => OLoc.Get_Heartbeat_Producer_Period + 1);

      Assert (L.Get_State = ACO.States.Operational,
              "Expected Local to report Operational");
      Assert (R.Get_State = ACO.States.Operational,
              "Expected Remote to report Operational");

      -------------------------------------------------------------------------
      --  Change the state of the local node
      -------------------------------------------------------------------------
      L.Set_State (ACO.States.Stopped);

      Assert (L.Get_State = ACO.States.Stopped,
              "Expected Local to report Stopped");

      Let_Time_Pass (HL, HR, DL, DR,
                     Time_Ms => OLoc.Get_Heartbeat_Producer_Period + 1);

      Assert (L.Get_State = ACO.States.Stopped,
              "Expected Local to report Stopped");
      Assert (R.Get_State = ACO.States.Stopped,
              "Expected Remote to report Stopped");




--        L.Write
--           (Node     => ,
--            Index    => ,
--            Subindex => ,
--            An_Entry => )
   end Run_Test;

end Remote_Node_Test;
