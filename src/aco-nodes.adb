package body ACO.Nodes is

   procedure Initialize
     (This          : in out Node;
      Driver        : in     ACO.Drivers.Driver_Access;
      Logger_Stream : access Ada.Streams.Root_Stream_Type'Class := null)
   is
      use ACO.Loggers;
   begin
      This.Driver := Driver;

      This.Log.Set_Level (This.Log_Level);
      This.Log.Set_Stream (Logger_Stream);

      This.Log.Put_Line (Info, "Initialized CANopen");
   end Initialize;

   procedure Dispatch
     (This : in out Node;
      Msg  : in     Message)
   is
      Func : constant Function_Code := Func_Code (Msg);
      use ACO.Protocols;
      use ACO.Loggers;
   begin

      This.Log.Put_Line (Debug, "Received message " & Image (Msg));

      if Func = This.NMT.Code then
         This.NMT.Message_Received (Msg);

      elsif Func = This.SYNC.Code or else
            Func = This.EMCY.Code
      then
         if Node_Id (Msg) = Broadcast_Id then
            This.SYNC.Message_Received (Msg);
         else
            This.NMT.Message_Received (Msg);
         end if;
      end if;

   end Dispatch;

   task body Receiver_Task
   is
      Msg : Message;
   begin
      loop
         This.Driver.Await_Message (Msg);
         This.Dispatch (Msg);
      end loop;
   end Receiver_Task;

end ACO.Nodes;
