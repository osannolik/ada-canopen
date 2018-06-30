package body ACO.Drivers.Dummy is

   overriding
   procedure Await_Message
     (This : in out Dummy_Driver;
      Msg  :    out Message)
   is
      pragma Unreferenced (This, Msg);
   begin
      null;
   end Await_Message;

   overriding
   procedure Send_Message
     (This : in out Dummy_Driver;
      Msg  : in     Message)
   is
   begin
      This.Tx_Buffer.Append (Msg);
   end Send_Message;

   overriding
   procedure Initialize (This : in out Dummy_Driver)
   is
   begin
      This.Tx_Buffer.Clear;
   end Initialize;

   procedure Get_First_Sent
     (This : in out Dummy_Driver;
      Msg  :    out Message)
   is
   begin
      if Natural (This.Tx_Buffer.Length) > 0 then
         Msg := This.Tx_Buffer.First_Element;
         This.Tx_Buffer.Delete_First;
      else
         raise Constraint_Error with "No messages sent";
      end if;
   end Get_First_Sent;

   function Nof_Sent (This : Dummy_Driver) return Natural is
     (Natural (This.Tx_Buffer.Length));

end ACO.Drivers.Dummy;
