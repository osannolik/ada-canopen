with Ada.Finalization;

generic
   type Item_Type is private;
   with function "=" (L : Item_Type; R : Item_Type) return Boolean is <>;
package ACO.Utils.DS.Generic_Collection is

   pragma Preelaborate;

   type Collection
      (Max_Size : Positive)
   is new Ada.Finalization.Controlled with private;

   No_Index : constant Natural := 0;

   function Is_Full (C : Collection) return Boolean
      with Inline;

   function Is_Empty (C : Collection) return Boolean
      with Inline;

   function Is_Valid_Index (C : Collection; Index : Positive) return Boolean
      with Inline;

   function Available (C : Collection) return Natural
      with Inline;

   function Length (C : Collection) return Natural
      with Inline;

   function First (C : Collection) return Item_Type
      with Inline, Pre => not Is_Empty (C);

   function Last (C : Collection) return Item_Type
      with Inline, Pre => not Is_Empty (C);

   procedure Clear
      (C : in out Collection)
      with Post => Is_Empty (C);

   procedure Insert
      (C    : in out Collection;
       Item : in     Item_Type)
      with Pre => not Is_Full (C);

   procedure Insert
      (C      : in out Collection;
       Item   : in     Item_Type;
       Before : in     Positive)
      with Pre => not Is_Full (C);

   procedure Append
      (C    : in out Collection;
       Item : in     Item_Type)
      with Pre => not Is_Full (C);

   procedure Append
      (C     : in out Collection;
       Item  : in     Item_Type;
       After : in     Positive)
      with Pre => not Is_Full (C);

   procedure Remove
      (C    : in out Collection;
       From : in     Positive)
      with Pre => Is_Valid_Index (C, From);

   procedure Replace
      (C     : in out Collection;
       Index : in     Positive;
       Item  : in     Item_Type)
      with Inline, Pre => Is_Valid_Index (C, Index);

   function Item_At (C : Collection; Index : Positive) return Item_Type
      with Inline, Pre => Is_Valid_Index (C, Index);

   function Location
      (C     : Collection;
       Item  : Item_Type;
       Start : Positive := 1)
       return Natural;

private

   subtype Item_Index is Positive;
   type Item_Array is array (Item_Index range <>) of Item_Type;

   type Collection
      (Max_Size : Positive)
   is new Ada.Finalization.Controlled with record
      Items : Item_Array (1 .. Max_Size);
      Start : Item_Index := 1;
      Size  : Natural := 0;
   end record;

   procedure Initialize (C : in out Collection);

end ACO.Utils.DS.Generic_Collection;
