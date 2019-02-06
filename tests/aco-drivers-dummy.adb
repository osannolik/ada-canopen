package body ACO.Drivers.Dummy is

   overriding
   procedure Receive_Message_Blocking
     (This : in out Dummy_Driver;
      Msg  :    out ACO.Messages.Message)
   is
   begin
      This.Get_First_Sent (Msg);
   end Receive_Message_Blocking;

   overriding
   procedure Send_Message
     (This : in out Dummy_Driver;
      Msg  : in     ACO.Messages.Message)
   is
   begin
      This.Tx_Buffer.Append (Msg);
   end Send_Message;

   overriding
   procedure Initialize
      (This : in out Dummy_Driver)
   is
   begin
      This.Tx_Buffer.Clear;
   end Initialize;

   overriding
   procedure Finalize
      (This : in out Dummy_Driver)
   is
   begin
      This.Tx_Buffer.Clear;
   end Finalize;

   overriding
   function Is_Message_Pending
      (This : Dummy_Driver)
       return Boolean
   is
   begin
      return This.Nof_Sent > 0;
   end Is_Message_Pending;

   procedure Get_First_Sent
     (This : in out Dummy_Driver;
      Msg  :    out ACO.Messages.Message)
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

   overriding
   function Current_Time
      (This : Dummy_Driver)
       return Ada.Real_Time.Time
   is
      (This.T_Now);

   procedure Set_Time
      (This : in out Dummy_Driver;
       T    : in     Ada.Real_Time.Time)
   is
   begin
      This.T_Now := T;
   end Set_Time;

end ACO.Drivers.Dummy;
