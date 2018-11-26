with ACO.Messages;

package ACO.Drivers is

   pragma Preelaborate;

   use ACO.Messages;

   type Driver is abstract tagged limited null record;

   type Driver_Access is access all Driver'Class;

   procedure Receive_Message_Blocking
     (This : in out Driver;
      Msg  :    out Message) is abstract;

   procedure Send_Message
     (This : in out Driver;
      Msg  : in     Message) is abstract;

   procedure Initialize
     (This : in out Driver) is abstract;

   function Is_Message_Pending
      (This : Driver)
       return Boolean is abstract;

end ACO.Drivers;
