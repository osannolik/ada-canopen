with AUnit.Assertions; use AUnit.Assertions;

with ACO.Utils.Generic_Sorted_List;

package body Generic_Sorted_List_Test is
   pragma Assertion_Policy (Check);

   Max_Length : constant := 8;

   subtype T is Natural;

   subtype Values is T range 1 .. Max_Length;

   package List_Pack is new ACO.Utils.Generic_Sorted_List
      (Item_Type            => T,
       "<"                  => "<",
       "="                  => "=",
       Maximum_Nof_Elements => Max_Length);

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Generic Sorted List Test");
   end Name;

   procedure Fill (L : in out List_Pack.Sorted_List)
   is
   begin
      for I in Values'Range loop
         L.Add (I);
      end loop;
   end Fill;

   procedure Add_Too_Many
   is
      An_Item : constant T := 1337;
      L : List_Pack.Sorted_List;
   begin
      L.Clear;
      Fill (L);
      L.Add (An_Item);
   end Add_Too_Many;

   procedure Get_First_From_Empty
   is
      L : List_Pack.Sorted_List;
      Tmp : T;
      pragma Unreferenced (Tmp);
   begin
      L.Clear;
      Tmp := L.Get_First;
   end Get_First_From_Empty;

   procedure Sorting_Test
   is
      L : List_Pack.Sorted_List;
   begin
      L.Clear;

      L.Add (1);
      Assert (L.Get_Item (1) = 1, "Failed to add from empty");

      L.Add (2);
      Assert (L.Get_Item (1) = 1, "Failed to add to end");
      Assert (L.Get_Item (2) = 2, "Failed to add to end");

      L.Add (0);
      Assert (L.Get_Item (1) = 0, "Failed to insert at front");
      Assert (L.Get_Item (2) = 1, "Failed to insert at front");
      Assert (L.Get_Item (3) = 2, "Failed to insert at front");

      L.Add (1);
      Assert (L.Get_Item (1) = 0, "Failed to insert in middle");
      Assert (L.Get_Item (2) = 1, "Failed to insert in middle");
      Assert (L.Get_Item (3) = 1, "Failed to insert in middle");
      Assert (L.Get_Item (4) = 2, "Failed to insert in middle");

      L.Remove (1);
      Assert (L.Get_Item (1) = 0, "Failed to remove multiple from middle");
      Assert (L.Get_Item (2) = 2, "Failed to remove multiple from middle");

      L.Remove (0);
      Assert (L.Get_Item (1) = 2, "Failed to remove from front");

      L.Remove (2);
      Assert (L.Is_Empty, "Failed to remove last");
   end Sorting_Test;

   procedure Length_Test
   is
      L : List_Pack.Sorted_List;
   begin
      L.Clear;

      for I in Values'Range loop
         Assert ((if I = Values'First then L.Is_Empty else not L.Is_Empty),
                 "Is_Empty incorrect when adding");

         L.Add (I);

         Assert (L.Length = I, "Length incorrect when adding");
         Assert ((if I = Values'Last then L.Is_Full else not L.Is_Full),
                 "Is_Full incorrect when adding");
      end loop;

      L.Clear;

      Assert (L.Length = 0,  "Length incorrect after clearing");
      Assert (L.Is_Empty,    "Is not empty after clearing");
      Assert (not L.Is_Full, "Is full after clearing");

      Fill (L);

      for I in Values'Range loop
         Assert ((if I = Values'First then L.Is_Full else not L.Is_Full),
                 "Is_Full incorrect when removing");

         L.Remove (I);

         Assert (L.Length = Values'Last - I, "Length incorrect when removing");
         Assert ((if I = Values'Last then L.Is_Empty else not L.Is_Empty),
                 "Is_Empty incorrect when removing");
      end loop;

   end Length_Test;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Length_Test;

      Sorting_Test;

      Assert_Exception
         (Add_Too_Many'Access,
          "Expected an exception when adding too many items");

      Assert_Exception
         (Get_First_From_Empty'Access,
          "Expected an exception when getting first item from empty list");
   end Run_Test;

end Generic_Sorted_List_Test;
