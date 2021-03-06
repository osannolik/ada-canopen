private with ADA.Containers.Vectors;

package ACO.Drivers.Dummy is

   type Dummy_Driver is new Driver with private;

   overriding
   procedure Receive_Message_Blocking
      (This : in out Dummy_Driver;
       Msg  :    out ACO.Messages.Message);
   overriding
   procedure Send_Message
     (This : in out Dummy_Driver;
      Msg  : in     ACO.Messages.Message);

   overriding
   procedure Initialize
      (This : in out Dummy_Driver);

   overriding
   procedure Finalize
      (This : in out Dummy_Driver);

   overriding
   function Is_Message_Pending
      (This : Dummy_Driver)
       return Boolean;

   procedure Get_First_Sent
     (This : in out Dummy_Driver;
      Msg  :    out ACO.Messages.Message);

   function Nof_Sent (This : Dummy_Driver) return Natural;

   overriding
   function Current_Time (This : Dummy_Driver) return Ada.Real_Time.Time;

   procedure Set_Time
      (This : in out Dummy_Driver;
       T    : in     Ada.Real_Time.Time);

private

   use type ACO.Messages.Message;

   package V_Message is new Ada.Containers.Vectors
      (Natural, ACO.Messages.Message);

   subtype Message_Vec is V_Message.Vector;

   type Dummy_Driver is new Driver with record
      T_Now     : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
      Tx_Buffer : Message_Vec;
   end record;

end ACO.Drivers.Dummy;
