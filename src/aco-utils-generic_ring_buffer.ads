generic
   type Item_Type is private;
   Max_Nof_Items : Positive;

package ACO.Utils.Generic_Ring_Buffer is

   pragma Preelaborate;

   type Ring_Buffer is tagged limited private;

   type Item_Array is array (Natural range <>) of Item_Type;

   function Is_Empty (This : Ring_Buffer) return Boolean;
   --  @return True if the queue if empty.

   function Is_Full (This : Ring_Buffer) return Boolean;
   --  @return True if the queue if full.

   procedure Push
      (This : in out Ring_Buffer;
       Item : in     Item_Type)
      with Pre => not This.Is_Full;
   --  Push an item into the queue and put it last.
   --  @param Item The item.

   procedure Push
      (This  : in out Ring_Buffer;
       Items : in     Item_Array)
      with Pre => Items'Length <= This.Empty_Slots;
   --  Push an array of items into the queue and put it last.
   --  @param Items The item array.

   procedure Pull
      (This : in out Ring_Buffer;
       Item : out    Item_Type)
      with Pre => not This.Is_Empty;
   --  Pull an item from the front of the queue.
   --  @param Item The item.

   procedure Pull
      (This         : in out Ring_Buffer;
       N            : in     Natural;
       Items_Access : access Item_Array)
      with Pre => N <= This.Occupied_Slots;
   --  Pull N items from the front of the queue.
   --  @param N The number of items.
   --  @param Items_Access A reference to the item array the pulled items will
   --  be placed in.

   function Peek (This : Ring_Buffer; N : Positive) return Item_Type
      with Pre => not This.Is_Empty;
   --  Peek N slots into the queue. N=1 is the front most item.
   --  @param N The number of queue slots.

   function Peek (This : Ring_Buffer) return Item_Type
      with Pre => not This.Is_Empty;
   --  Peek the front item.
   --  @return The front most queue item.

   function Peek (This : Ring_Buffer; N : Positive) return Item_Array
      with Pre => N <= This.Occupied_Slots;
   --  Peek N number of items.
   --  @param N The number of items to peek.
   --  @return An array of the peeked items.

   function Occupied_Slots (This : Ring_Buffer) return Natural;
   --  @return The number of items in the queue.

   function Empty_Slots (This : Ring_Buffer) return Natural;
   --  @return The number of available slots in the queue.

   procedure Flush
      (This : in out Ring_Buffer;
       N    : in     Natural);
   --  Throw away N number of items from the front of the queue.
   --  @param N The number of items to flush.

   procedure Flush_All (This : in out Ring_Buffer);
   --  Empty the queue, i.e. remove all items.

private

   subtype Index is Natural range 0 .. Max_Nof_Items;
   --  Length is Max_Nof_Items + 1

   type Ring_Buffer is tagged limited record
      Items   : Item_Array (Index);
      Idx_New : Index := Index'First;
      Idx_Old : Index := Index'First;
   end record;

end ACO.Utils.Generic_Ring_Buffer;
