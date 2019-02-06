with ACO.Messages;
with Ada.Real_Time;

package ACO.Drivers is

   type Driver is abstract tagged limited null record;

   type Driver_Access is access all Driver'Class;

   procedure Receive_Message_Blocking
      (This : in out Driver;
       Msg  :    out ACO.Messages.Message) is abstract;

   procedure Send_Message
      (This : in out Driver;
       Msg  : in     ACO.Messages.Message) is abstract;

   procedure Initialize
      (This : in out Driver) is abstract;

   procedure Finalize
      (This : in out Driver) is abstract;

   function Is_Message_Pending
      (This : Driver)
       return Boolean is abstract;

   function Current_Time
      (This : Driver)
       return Ada.Real_Time.Time is abstract;

end ACO.Drivers;
