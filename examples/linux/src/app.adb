with Ada.Real_Time; use Ada.Real_Time;
with ACO.Drivers.Socket;
with ACO.Nodes;
with ACO.States;
with ACO.OD;
with ACO.Log;
with Ada.Text_IO.Text_Streams;
with ACO.OD.Example;

with Ada.Text_IO;
with Ada.Exceptions;

package body App is
   use Ada.Text_IO;

   O_Data : aliased ACO.OD.Example.Dictionary_Data;

   O : aliased ACO.OD.Object_Dictionary (O_Data'Access);

   D : aliased ACO.Drivers.Socket.CAN_Driver;

   N : aliased ACO.Nodes.Node (Id => 1, Od => O'Access, Driver => D'Access);

   T : ACO.Nodes.Receiver_Task (N'Access);

   W : ACO.Nodes.Periodic_Task (N'Access);

   procedure Run
   is
      Next_Release : Time := Clock;
   begin
      ACO.Log.Set_Stream (Text_Streams.Stream (Current_Output));
      ACO.Log.Set_Level (ACO.Log.Debug);

      D.Initialize;

      N.Set_State (ACO.States.Initializing);

      loop
         Next_Release := Next_Release + Milliseconds (500);
         delay until Next_Release;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

   end Run;

end App;
