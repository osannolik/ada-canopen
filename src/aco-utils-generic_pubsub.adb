package body ACO.Utils.Generic_Pubsub is

   procedure Update
     (This : in out Pub;
      Data : in     Item_Type)
   is
   begin
      for Subscriber of This.Subscribers loop
         if Subscriber /= null then
            Subscriber.Update (Data);
         end if;
      end loop;
   end Update;

   procedure Attach
     (This       : in out Pub;
      Subscriber : access Sub'Class)
   is
      Found : Boolean := False;
   begin
      for I in This.Subscribers'Range loop
         if This.Subscribers (I) = null then
            This.Subscribers (I) := Ref (Subscriber);
            Found := True;
         elsif This.Subscribers (I) = Subscriber then
            Found := True;
         end if;
         exit when Found;
      end loop;

      if not Found then
         raise Already_Maximum_Nof_Subscribers;
      end if;
   end Attach;

   procedure Detach
     (This       : in out Pub;
      Subscriber : access Sub'Class)
   is
   begin
      for I in This.Subscribers'Range loop
         if This.Subscribers (I) = Subscriber then
            This.Subscribers (I) := null;
         end if;
      end loop;
   end Detach;

end ACO.Utils.Generic_Pubsub;
