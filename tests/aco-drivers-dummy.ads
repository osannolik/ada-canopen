private with ADA.Containers.Vectors;

package ACO.Drivers.Dummy is

   type Dummy_Driver is new Driver with private;

   overriding
   procedure Await_Message (This : in out Dummy_Driver;
                            Msg  :    out Message);
   overriding
   procedure Send_Message
     (This : in out Dummy_Driver;
      Msg  : in     Message);

   overriding
   procedure Initialize (This : in out Dummy_Driver);

   procedure Get_First_Sent
     (This : in out Dummy_Driver;
      Msg  :    out Message);

   function Nof_Sent (This : Dummy_Driver) return Natural;

private

   package V_Message is new Ada.Containers.Vectors (Natural, Message);

   subtype Message_Vec is V_Message.Vector;

   type Dummy_Driver is new Driver with record
      Tx_Buffer : Message_Vec;
   end record;

end ACO.Drivers.Dummy;
