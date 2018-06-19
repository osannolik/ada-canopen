generic
   type Item_Type is private;

   with function "<" (Left, Right : in Item_Type) return Boolean is <>;
   with function "=" (Left, Right : in Item_Type) return Boolean is <>;

   Maximum_Nof_Elements : Positive;

package ACO.Utils.Generic_Sorted_List is
   --  Simple, but not very efficient for large list and item sizes...

   type Sorted_List is tagged limited private;

   procedure Add
     (This : in out Sorted_List;
      Item : in     Item_Type)
   with
      Pre  => not This.Is_Full,
      Post => not This.Is_Empty;

   procedure Remove
     (This : in out Sorted_List;
      Item : in     Item_Type);

   function Length (This : Sorted_List) return Natural;

   function Is_Full (This : Sorted_List) return Boolean;

   function Is_Empty (This : Sorted_List) return Boolean;

   function Get_First (This : Sorted_List) return Item_Type
      with
         Pre => not This.Is_Empty;

private

   type Item_Array is array (Positive range <>) of Item_Type;

   type Sorted_List is tagged limited record
      Items : Item_Array (1 .. Maximum_Nof_Elements);
      Nof_Items : Natural := 0;
   end record;

end ACO.Utils.Generic_Sorted_List;
