package body ACO.Utils.Generic_Pubsub is

   function Nof_Subscribers (This : Pub) return Natural is
      (This.N);

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
      Subscriber : in     Sub_Access)
   is
      Found : Boolean := False;
   begin
      for I in This.Subscribers'Range loop
         if This.Subscribers (I) = null then
            This.Subscribers (I) := Subscriber;
            Found := True;
         elsif This.Subscribers (I) = Subscriber then
            Found := True;
         end if;
         exit when Found;
      end loop;

      if Found then
         This.N := This.N + 1;
      end if;
   end Attach;

   procedure Detach
     (This       : in out Pub;
      Subscriber : in     Sub_Access)
   is
   begin
      for I in This.Subscribers'Range loop
         if This.Subscribers (I) = Subscriber then
            This.Subscribers (I) := null;
            This.N := This.N - 1;
         end if;
      end loop;
   end Detach;

end ACO.Utils.Generic_Pubsub;
