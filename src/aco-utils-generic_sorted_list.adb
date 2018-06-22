package body ACO.Utils.Generic_Sorted_List is

   function Length (This : Sorted_List) return Natural
   is
   begin
      return This.Nof_Items;
   end Length;

   function Is_Full (This : Sorted_List) return Boolean
   is
   begin
      return This.Nof_Items >= Maximum_Nof_Elements;
   end Is_Full;

   function Is_Empty (This : Sorted_List) return Boolean
   is
   begin
      return This.Nof_Items = 0;
   end Is_Empty;

   procedure Add
     (This : in out Sorted_List;
      Item : in     Item_Type)
   is
      subtype Idx is Positive range
         This.Items'First .. This.Items'First + This.Nof_Items - 1;
   begin
      --  Insert
      for I in Idx'Range loop
         if Item < This.Items (I) then
            This.Items (I + 1 .. Idx'Last + 1) := This.Items (I .. Idx'Last);
            This.Items (I) := Item;
            This.Nof_Items := This.Nof_Items + 1;

            return;
         end if;
      end loop;

      --  Append at end
      This.Items (Idx'Last + 1) := Item;
      This.Nof_Items := This.Nof_Items + 1;
   end Add;

   procedure Remove
     (This : in out Sorted_List;
      Item : in     Item_Type)
   is
      Start : Positive := This.Items'First;
      Found : Boolean;
   begin
      while This.Nof_Items > 0 loop
         declare
            subtype Idx is Positive range
               Start .. This.Items'First + This.Nof_Items - 1;
         begin
            Found := False;
            Find:
            for I in Idx'Range loop
               Found := Item = This.Items (I);
               if Found then
                  This.Items (I .. Idx'Last - 1) := This.Items (I + 1 .. Idx'Last);
                  This.Nof_Items := This.Nof_Items - 1;
                  Start := I;

                  exit Find;
               end if;
            end loop Find;
         end;

         exit when not Found;
      end loop;
   end Remove;

   function Get_First (This : Sorted_List) return Item_Type
   is
   begin
      return This.Items (This.Items'First);
   end Get_First;

end ACO.Utils.Generic_Sorted_List;
