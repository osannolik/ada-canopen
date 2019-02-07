with Ada.Real_Time;
with Ada.Finalization;
with ACO.Messages;
with ACO.Configuration;
with ACO.Events;
with ACO.Drivers;

private with Ada.Synchronous_Task_Control;
private with ACO.Messages.Buffer;

package ACO.CANopen is

   type Handler_Base
   is abstract new Ada.Finalization.Limited_Controlled with record
      Events : ACO.Events.Handler_Event_Manager;
   end record;

   type Handler
      (Driver : not null access ACO.Drivers.Driver'Class)
   is new Handler_Base with private;

   procedure Start
      (This : in out Handler);

   procedure Put
      (This : in out Handler;
       Msg  : in     ACO.Messages.Message);

   procedure Periodic_Actions
      (This  : in out Handler;
       T_Now : in     Ada.Real_Time.Time);

   function Current_Time
     (This : Handler)
      return Ada.Real_Time.Time;

   task type Periodic_Task
      (This      : not null access Handler;
       Period_Ms : Positive)
      with Priority => ACO.Configuration.Periodic_Task_Priority;

private

   type Handler
      (Driver : not null access ACO.Drivers.Driver'Class)
   is new Handler_Base with record
      Messages : ACO.Messages.Buffer.Protected_Queue
         (Ceiling => ACO.Configuration.Messages_Buffer_Ceiling);
      Suspension : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end ACO.CANopen;
