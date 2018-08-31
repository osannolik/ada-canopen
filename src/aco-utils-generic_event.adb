package body ACO.Utils.Generic_Event is

   function Events_Waiting
      (This : Event_Publisher)
       return Natural
   is
   begin
      return This.Queue.Count;
   end Events_Waiting;

   procedure Put
      (This : in out Event_Publisher;
       Data : in     Item_Type)
   is
   begin
      This.Queue.Put_Blocking (Data);
   end Put;

   procedure Process
      (This : in out Event_Publisher)
   is
      Data : Item_Type;
   begin
      while not This.Queue.Is_Empty loop
         This.Queue.Get_Blocking (Data);
         PS.Pub (This).Update (Data);
      end loop;
   end Process;

end ACO.Utils.Generic_Event;
