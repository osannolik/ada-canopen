package body ACO.Utils.Generic_Ring_Buffer is

   function Is_Empty (This : Ring_Buffer) return Boolean is
      (This.Idx_Old = This.Idx_New);

   function Is_Full (This : Ring_Buffer) return Boolean is
      (This.Idx_Old = ((This.Idx_New + 1) mod (Index'Last + 1)));

   procedure Push
      (This : in out Ring_Buffer;
       Item : in     Item_Type)
   is
   begin
      This.Items (This.Idx_New) := Item;
      This.Idx_New := (This.Idx_New + 1) mod (Index'Last + 1);
   end Push;

   procedure Push
      (This  : in out Ring_Buffer;
       Items : in     Item_Array)
   is
   begin
      for I of Items loop
         This.Push (Item => I);
      end loop;
   end Push;

   procedure Pull
      (This : in out Ring_Buffer;
       Item : out    Item_Type)
   is
   begin
      Item := This.Items (This.Idx_Old);
      This.Idx_Old := (This.Idx_Old + 1) mod (Index'Last + 1);
   end Pull;

   procedure Pull
      (This         : in out Ring_Buffer;
       N            : in     Natural;
       Items_Access : access Item_Array)
   is
      Start : constant Index := Items_Access.all'First;
   begin
      if N = 0 then
         return;
      end if;

      for Idx in Start .. Start + N - 1 loop
         This.Pull (Item => Items_Access (Idx));
      end loop;
   end Pull;

   function Peek (This : Ring_Buffer; N : Positive) return Item_Type is
      (This.Items ((This.Idx_Old + N - 1) mod (Index'Last + 1)));

   function Peek (This : Ring_Buffer) return Item_Type is
      (This.Items (This.Idx_Old));

   function Peek (This : Ring_Buffer; N : Positive) return Item_Array
   is
      Items : Item_Array (Index'First .. Index'First + N - 1);
   begin
      for Idx in Items'Range loop
         Items (Idx) := This.Peek (N => Idx + 1);
      end loop;

      return Items;
   end Peek;

   function Occupied_Slots (This : Ring_Buffer) return Natural
   is
   begin
      if This.Idx_New >= This.Idx_Old then
         return This.Idx_New - This.Idx_Old;
      else
         return Index'Last - This.Idx_Old + 1 + This.Idx_New - Index'First;
      end if;
   end Occupied_Slots;

   function Empty_Slots (This : Ring_Buffer) return Natural is
   begin
      return Max_Nof_Items - This.Occupied_Slots;
   end Empty_Slots;

   procedure Flush
      (This : in out Ring_Buffer;
       N    : in     Natural)
   is
   begin
      This.Idx_Old := (This.Idx_Old + N) mod (Index'Last + 1);
   end Flush;

   procedure Flush_All (This : in out Ring_Buffer)
   is
   begin
      This.Idx_Old := This.Idx_New;
   end Flush_All;

end ACO.Utils.Generic_Ring_Buffer;
