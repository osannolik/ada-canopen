package body ACO.Events is

   procedure Process
      (This : in out Node_Event_Manager)
   is
   begin
      This.Node_Events.Process;
   end Process;

   overriding
   procedure Update
      (This : access Handler_Event_Listener;
       Data : in     Handler_Event_Data)
   is
      type Ref is access all Handler_Event_Listener'Class;
   begin
      if This.Event = Data.Event then
         Ref (This).On_Event (Data);
      end if;
   end Update;

   overriding
   procedure Update
      (This : access Event_Listener;
       Data : in     Event_Data)
   is
      type Ref is access all Event_Listener'Class;
   begin
      if This.Event = Data.Event then
         Ref (This).On_Event (Data);
      end if;
   end Update;

end ACO.Events;
