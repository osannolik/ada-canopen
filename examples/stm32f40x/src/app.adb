with Ada.Real_Time;
with ACO.Drivers.Stm32f40x;
with STM32.Device;
with ACO.Nodes;
with ACO.OD.Example;
with ACO.CANopen;
with ACO.Nodes.Locals;

package body App is

   D : aliased ACO.Drivers.Stm32f40x.CAN_Driver (STM32.Device.CAN_1'Access);

   H : aliased ACO.CANopen.Handler (Driver => D'Access);

   T : ACO.CANopen.Periodic_Task (H'Access, 10);

   O : aliased ACO.OD.Example.Dictionary;

   procedure Run
   is
      use Ada.Real_Time;

      N : ACO.Nodes.Locals.Local (Id => 1, Handler => H'Access, Od => O'Access);

      Next_Release : Time := Clock;
   begin

      D.Initialize;

      N.Start;

      loop
         --  H.Periodic_Actions (T_Now => Next_Release);

         Next_Release := Next_Release + Milliseconds (10);
         delay until Next_Release;
      end loop;
   end Run;

end App;
