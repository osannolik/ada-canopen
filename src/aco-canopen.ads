with Ada.Real_Time;
with Ada.Finalization;
with ACO.Messages;
with ACO.Configuration;
with ACO.Events;
with ACO.Drivers;

private with ACO.Messages.Buffer;

package ACO.CANopen is

   type Handler_Base
   is abstract new Ada.Finalization.Limited_Controlled with record
      Events : ACO.Events.Event_Manager;
   end record;

   type Handler
      (Driver : not null access ACO.Drivers.Driver'Class)
   is new Handler_Base with private;

   procedure Put
      (This : in out Handler;
       Msg  : in     ACO.Messages.Message);

   procedure Periodic_Actions
      (This  : in out Handler;
       T_Now : in     Ada.Real_Time.Time);

   task type Periodic_Task
      (This      : not null access Handler;
       Period_Ms : Positive)
      with Priority => ACO.Configuration.Periodic_Task_Priority;

private

   type Handler
      (Driver : not null access ACO.Drivers.Driver'Class)
   is new Handler_Base with record
      Messages : ACO.Messages.Buffer.Protected_Queue
         (Max_Nof_Items => ACO.Configuration.Messages_Buffer_Size,
          Ceiling       => ACO.Configuration.Messages_Buffer_Ceiling);
   end record;

end ACO.CANopen;
