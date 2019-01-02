package body ACO.Utils.DS.Generic_Queue is

   function Is_Full (This : Queue) return Boolean is
      (This.Count >= Max_Nof_Items);

   function Is_Empty (This : Queue) return Boolean is
      (This.Count = 0);

   function Length (This : Queue) return Natural is
      (This.Count);

   function Free_Slots (This : Queue) return Natural is
      (Max_Nof_Items - This.Count);

   procedure Inc (I : in out Index)
   is
   begin
      I := (if I >= Index'Last then Index'First else Index'Succ (I));
   end Inc;

   procedure Put
      (This : in out Queue;
       Item : in     Item_Type)
   is
   begin
      This.Items (This.Next) := Item;
      Inc (This.Next);
      This.Count := This.Count + 1;
   end Put;

   procedure Put
      (This  : in out Queue;
       Items : in     Item_Array)
   is
   begin
      for Item of Items loop
         This.Put (Item);
      end loop;
   end Put;

   procedure Get
      (This : in out Queue;
       Item :    out Item_Type)
   is
   begin
      Item := This.Items (This.Old);
      Inc (This.Old);
      This.Count := This.Count - 1;
   end Get;

   procedure Get
      (This  : in out Queue;
       Items :    out Item_Array)
   is
   begin
      for Item of Items loop
         This.Get (Item);
      end loop;
   end Get;

   procedure Flush
      (This : in out Queue)
   is
   begin
      This.Count := 0;
      This.Old := This.Next;
   end Flush;

   function Peek (This : Queue) return Item_Type is
      (This.Items (This.Old));

   function Peek (This : Queue) return Item_Array
   is
      Items : Item_Array (Index'First .. Index'First + This.Count - 1);
      I : Index := This.Old;
   begin
      for Item of Items loop
         Item := This.Items (I);
         Inc (I);
      end loop;
      return Items;
   end Peek;

end ACO.Utils.DS.Generic_Queue;
