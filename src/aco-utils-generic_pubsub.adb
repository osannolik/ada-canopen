package body ACO.Utils.Generic_Pubsub is

   function Nof_Subscribers
      (This : in out Pub)
       return Natural
   is
      S : ACO.Utils.Scope_Locks.Scope_Lock (This.Mutex'Access);
      pragma Unreferenced (S);
   begin
      return This.Subscribers.Length;
   end Nof_Subscribers;

   function Get_Subscribers
      (This : in out Pub)
       return Sub_Array
   is
      S : ACO.Utils.Scope_Locks.Scope_Lock (This.Mutex'Access);
      pragma Unreferenced (S);
   begin
      declare
         L : constant Natural := This.Subscribers.Length;
         Subscribers : Sub_Array (1 .. L);
      begin
         for I in Subscribers'Range loop
            Subscribers (I) := This.Subscribers.Item_At (I);
         end loop;

         return Subscribers;
      end;
   end Get_Subscribers;

   procedure Update
     (This : in out Pub;
      Data : in     Item_Type)
   is
      Subscribers : constant Sub_Array := This.Get_Subscribers;
   begin
      --  Subscriber notification must be unprotected (no seized mutex)
      --  otherwise a call to e.g. Detach by the subscriber would cause deadlock
      for Subscriber of Subscribers loop
         Subscriber.Update (Data);
      end loop;
   end Update;

   procedure Attach
     (This       : in out Pub;
      Subscriber : in     Sub_Access)
   is
      S : ACO.Utils.Scope_Locks.Scope_Lock (This.Mutex'Access);
      pragma Unreferenced (S);
   begin
      This.Subscribers.Append (Subscriber);
   end Attach;

   procedure Detach
     (This       : in out Pub;
      Subscriber : in     Sub_Access)
   is
      S : ACO.Utils.Scope_Locks.Scope_Lock (This.Mutex'Access);
      pragma Unreferenced (S);
      I : Positive;
   begin
      I := This.Subscribers.Location (Subscriber);
      This.Subscribers.Remove (I);
   end Detach;

end ACO.Utils.Generic_Pubsub;
