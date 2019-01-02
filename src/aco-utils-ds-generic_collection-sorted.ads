generic
   with function "<" (L : Item_Type; R : Item_Type) return Boolean is <>;
package ACO.Utils.DS.Generic_Collection.Sorted is

   pragma Preelaborate;

   type Sorted_Collection (Max_Size : Positive) is
      new Collection (Max_Size) with null record;

   overriding
   procedure Insert
      (C    : in out Sorted_Collection;
       Item : in     Item_Type)
      with Pre => not Is_Full (C);

   overriding
   procedure Insert
      (C      : in out Sorted_Collection;
       Item   : in     Item_Type;
       Before : in     Positive)
      with Pre => Is_Valid_Index (C, Before) and not Is_Full (C);

   overriding
   procedure Append
      (C    : in out Sorted_Collection;
       Item : in     Item_Type)
      with Pre => not Is_Full (C);

   overriding
   procedure Append
      (C     : in out Sorted_Collection;
       Item  : in     Item_Type;
       After : in     Positive)
      with Pre => Is_Valid_Index (C, After) and not Is_Full (C);

   overriding
   procedure Replace
      (C        : in out Sorted_Collection;
       At_Index : in     Positive;
       Item     : in     Item_Type)
      with Pre => Is_Valid_Index (C, At_Index);

end ACO.Utils.DS.Generic_Collection.Sorted;
