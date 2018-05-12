with Ada.Real_Time; use Ada.Real_Time;
with ACO.Drivers.Stm32f40x;
with ACO.Messages;
with STM32.Device;
with ACO.Nodes;

package body App is

   D : aliased ACO.Drivers.Stm32f40x.CAN_Driver (STM32.Device.CAN_1'Access);

   N : aliased ACO.Nodes.Node;

   T : ACO.Nodes.Receiver_Task (N'Access);

   procedure Run
   is
      Next_Release : Time := Clock;

      --  Msg_Rx : ACO.Messages.Message;
      Msg_Tx : ACO.Messages.Message :=
         ACO.Messages.Create (CAN_Id => 16#1#,
                              RTR    => False,
                              Data   => (1, 2, 3, 4));
      X : ACO.Messages.Data_Type := 10;

      use type ACO.Messages.Data_Type;
   begin
      D.Initialize;

      N.Initialize
         (Driver => D'Access);

      loop
         D.Send_Message (Msg_Tx);

         --  D.Await_Message (Msg_Rx);

         if X >= 20 then
            X := 10;
         else
            X := X + 1;
         end if;

         Msg_Tx.Data (Msg_Tx.Data'First) := X;

         Next_Release := Next_Release + Milliseconds (500);
         delay until Next_Release;
      end loop;
   end Run;

end App;
