with Ada.Real_Time; use Ada.Real_Time;
with ACO.Drivers.Socket;
with ACO.Messages;
with ACO.Nodes;
with ACO.States;
with ACO.OD;
with ACO.Log;
with Ada.Text_IO.Text_Streams;

package body App is
   use Ada.Text_IO;

   O : aliased ACO.OD.Object_Dict;

   D : aliased ACO.Drivers.Socket.CAN_Driver;

   N : aliased ACO.Nodes.Node (Id => 1, Od => O'Access, Driver => D'Access);

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
      ACO.Log.Set_Stream (Text_Streams.Stream (Current_Output));
      ACO.Log.Set_Level (ACO.Log.Debug);

      D.Initialize;

      N.Set_State (ACO.States.Initializing);

      loop
         --  D.Send_Message (Msg_Tx);

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
