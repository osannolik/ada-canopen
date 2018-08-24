with Ada.Real_Time;
with ACO.Drivers.Stm32f40x;
with STM32.Device;
with ACO.Nodes;
with ACO.States;
with ACO.OD.Example;

package body App is

   O_Data : aliased ACO.OD.Example.Dictionary_Data;

   O : aliased ACO.OD.Object_Dictionary (O_Data'Access);

   D : aliased ACO.Drivers.Stm32f40x.CAN_Driver (STM32.Device.CAN_1'Access);

   N : aliased ACO.Nodes.Node (Id => 1, Od => O'Access, Driver => D'Access);

   T : ACO.Nodes.Receiver_Task (N'Access);

   W : ACO.Nodes.Periodic_Task (N'Access);

   procedure Run
   is
      use Ada.Real_Time;

      Next_Release : Time := Clock;
   begin

      D.Initialize;

      N.Set_State (ACO.States.Initializing);

      loop

         Next_Release := Next_Release + Milliseconds (500);
         delay until Next_Release;
      end loop;
   end Run;

end App;
