package body ACO.Utils.DS.Generic_Collection.Sorted is

   overriding
   procedure Insert
      (C    : in out Sorted_Collection;
       Item : in     Item_Type)
   is
      P : Collection renames Collection (C);
   begin
      for Index in 1 .. Length (P) loop
         if not (Item_At (P, Index) < Item) then
            Insert (P, Item, Index);

            return;
         end if;
      end loop;

      Append (P, Item);
   end Insert;

   overriding
   procedure Insert
      (C      : in out Sorted_Collection;
       Item   : in     Item_Type;
       Before : in     Positive)
   is
      P : Collection renames Collection (C);
      Current : constant Item_Type := Item_At (C, Before);
   begin
      if Item < Current or else Current < Item then
         Insert (C, Item);
      else
         Insert (P, Before => Before, Item => Item);
      end if;
   end Insert;

   overriding
   procedure Append
      (C    : in out Sorted_Collection;
       Item : in     Item_Type)
   is
      P : Collection renames Collection (C);
   begin
      for Index in 1 .. Length (P) loop
         if Item < Item_At (P, Index) then
            Insert (P, Item, Index);

            return;
         end if;
      end loop;

      Append (P, Item);
   end Append;

   overriding
   procedure Append
      (C     : in out Sorted_Collection;
       Item  : in     Item_Type;
       After : in     Positive)
   is
      P : Collection renames Collection (C);
      Current : constant Item_Type := Item_At (C, After);
   begin
      if Item < Current or else Current < Item then
         Append (C, Item);
      else
         Append (P, After => After, Item => Item);
      end if;
   end Append;

   overriding
   procedure Replace
      (C        : in out Sorted_Collection;
       At_Index : in     Positive;
       Item     : in     Item_Type)
   is
      P : Collection renames Collection (C);
      Current : constant Item_Type := Item_At (C, At_Index);
   begin
      if Item < Current then
         Remove (C, At_Index);
         Append (C, Item);
      elsif Current < Item then
         Remove (C, At_Index);
         Insert (C, Item);
      else
         Replace (P, Index => At_Index, Item => Item);
      end if;
   end Replace;

end ACO.Utils.DS.Generic_Collection.Sorted;
