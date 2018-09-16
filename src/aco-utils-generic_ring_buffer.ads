generic
   type Item_Type is private;
   Max_Nof_Items : Positive;

package ACO.Utils.Generic_Ring_Buffer is

   pragma Preelaborate;

   type Ring_Buffer is tagged limited private;

   type Item_Array is array (Natural range <>) of Item_Type;

   function Is_Full (This : Ring_Buffer) return Boolean
      with Inline;

   function Is_Empty (This : Ring_Buffer) return Boolean
      with Inline;

   function Length (This : Ring_Buffer) return Natural
      with Inline;

   function Free_Slots (This : Ring_Buffer) return Natural
      with Inline;

   procedure Put
      (This : in out Ring_Buffer;
       Item : in     Item_Type)
      with Pre => not This.Is_Full;

   procedure Put
      (This  : in out Ring_Buffer;
       Items : in     Item_Array)
      with Pre => Items'Length <= This.Free_Slots;

   procedure Get
      (This : in out Ring_Buffer;
       Item :    out Item_Type)
      with Pre => not This.Is_Empty;

   procedure Get
      (This  : in out Ring_Buffer;
       Items :    out Item_Array)
      with Pre => Items'Length <= This.Length;

   procedure Flush
      (This : in out Ring_Buffer)
      with Post => This.Is_Empty;

   function Peek (This : Ring_Buffer) return Item_Type
      with Pre => not This.Is_Empty;

   function Peek (This : Ring_Buffer) return Item_Array
      with Pre => not This.Is_Empty;

private

   subtype Index is Positive range 1 .. Max_Nof_Items;

   type Ring_Buffer is tagged limited record
      Items : Item_Array (Index);
      Next  : Index   := Index'First;
      Old   : Index   := Index'First;
      Count : Natural := 0;
   end record;

   procedure Inc (I : in out Index)
      with Inline;

end ACO.Utils.Generic_Ring_Buffer;
