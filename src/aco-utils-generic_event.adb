package body ACO.Utils.Generic_Event is

   function Events_Waiting
      (This : Queued_Event_Publisher)
       return Natural
   is
   begin
      return This.Queue.Count;
   end Events_Waiting;

   procedure Put
      (This : in out Queued_Event_Publisher;
       Data : in     Item_Type)
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      This.Queue.Put (Data, Success);
   end Put;

   procedure Process
      (This : in out Queued_Event_Publisher)
   is
      Data : Item_Type;
   begin
      while not This.Queue.Is_Empty loop
         This.Queue.Get (Data);
         PS.Pub (This).Update (Data);
      end loop;
   end Process;

end ACO.Utils.Generic_Event;
