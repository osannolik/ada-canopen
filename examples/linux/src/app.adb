with Ada.Real_Time;
with ACO.Drivers.Socket;
with ACO.CANopen;
with ACO.States;
with ACO.Nodes.Locals;
with ACO.OD;
with ACO.Log;
with Ada.Text_IO.Text_Streams;
with ACO.OD.Example;
with ACO.Configuration;

with Ada.Text_IO;
with Ada.Exceptions;

package body App is

   O : aliased ACO.OD.Example.Dictionary;

   D : aliased ACO.Drivers.Socket.CAN_Driver;

   H : aliased ACO.CANopen.Handler (Driver => D'Access);

   N : aliased ACO.Nodes.Locals.Local
      (Id => 1, Handler => H'Access, Od => O'Access);

   W : ACO.CANopen.Periodic_Task
      (H'Access, Period_Ms => ACO.Configuration.Periodic_Task_Period_Ms);

   procedure Run
   is
      use Ada.Text_IO;
      use type Ada.Real_Time.Time;

      Next_Release : Ada.Real_Time.Time;
   begin
      ACO.Log.Set_Stream (Text_Streams.Stream (Current_Output));
      ACO.Log.Set_Level (ACO.Log.Debug);

      D.Initialize;

      N.Set_State (ACO.States.Initializing);

      Next_Release := Ada.Real_Time.Clock;

      loop
         --  H.Periodic_Actions (T_Now => Next_Release);

         Next_Release := Next_Release + Ada.Real_Time.Milliseconds (1);
         delay until Next_Release;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

   end Run;

end App;
