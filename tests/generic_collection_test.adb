with AUnit.Assertions; use AUnit.Assertions;

with ACO.Utils.DS.Generic_Collection;
with ACO.Utils.DS.Generic_Collection.Sorted;

package body Generic_Collection_Test is
   pragma Assertion_Policy (Check);

   type Item_Type is new Character;

   subtype Test_Items is Item_Type range 'a' .. 'z';

   package Collection_Pack is new ACO.Utils.DS.Generic_Collection
      (Item_Type => Item_Type,
       "="       => "=");

   Max_Size : constant :=
      Test_Items'Pos(Test_Items'Last) - Test_Items'Pos(Test_Items'First) + 1;

   subtype Collection is Collection_Pack.Collection (Max_Size);

   package Sorted is
      type T is record
         Char : Character;
         Id : Natural;
      end record;

      function "=" (L : T; R : T) return Boolean is
         (L.Char = R.Char);

      package Collection_Pack is new ACO.Utils.DS.Generic_Collection
         (Item_Type => T,
          "="       => "=");

      function "<" (L : T; R : T) return Boolean is
         (L.Char < R.Char);

      package Pack is new Collection_Pack.Sorted
         ("<" => "<");

      Max_Size : constant := 10;

      subtype Collection is Pack.Sorted_Collection (Max_Size);
   end Sorted;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Data Structures: Generic (Sorted) Collection Test");
   end Name;

   procedure Init_Test
   is
      C : Collection;
   begin
      Assert (C.Is_Empty, "Collection is not initially empty");
      Assert (C.Length = 0, "Collection has not initially the length zero");
      Assert (not C.Is_Full, "Collection is initially full");
      Assert (C.Available = Max_Size, "Nof availables in collection incorrect");

      for I in 1 .. C.Max_Size loop
         Assert (not C.Is_Valid_Index (I), "Empty collection has valid index");
      end loop;
   end Init_Test;

   procedure Fill_And_Empty
   is
      C : Collection;
      L : Natural := 0;
      Loc : Natural;
   begin
      C.Clear;

      for I in Test_Items'Range loop
         C.Append (I);
         L := L + 1;

         Assert (C.Length = L, "Length incorrect");
         Assert (C.Available = C.Max_Size - L, "Available incorrect");
         Assert (C.Is_Valid_Index (L), "Index of added item invalid");
         Assert (not C.Is_Valid_Index (L + 1), "Index of not added item valid");
         Assert (C.Item_At (L) = I, "Added item incorrect");
         Assert (C.Last = I, "Appended item not the last one");
         Assert (C.First = Test_Items'First, "First item not the first added");
         Assert (not C.Is_Empty, "Collection is empty after adding items");
         Assert (C.Location (I) = L, "Location of added item incorrect");
      end loop;

      Assert (C.Is_Full, "Collection not full after filling it");

      for I in Test_Items'Range loop
         Loc := C.Location (I);
         C.Remove (Loc);
         L := L - 1;

         Assert (C.Length = L, "Length incorrect");
         Assert (C.Available = C.Max_Size - L, "Available incorrect");
         if not C.Is_Empty then
            Assert (C.Is_Valid_Index (Loc),
                    "Front index invalid after removing front item");
            Assert (C.First = Test_Items'Succ (I),
                    "Item after removed not set as front item");
            Assert (C.Last = Test_Items'Last, "Last item not kept");
         else
            Assert (not C.Is_Valid_Index (Loc),
                    "Front index valid when collection is empty");
         end if;
      end loop;

      Assert (C.Is_Empty, "Not empty after removing all items");
   end Fill_And_Empty;

   type Result (L : Natural) is record
      Description : String (1 .. L);
      Status : Boolean;
   end record;

   function Failed
      (Description : String)
       return Result
   is
      (Result'(L           => Description'Length,
               Description => Description,
               Status      => False));

   function Success return Result
   is
      (Result'(L           => 0,
               Description => "",
               Status      => True));

   type State_Array is array (Positive range <>) of Item_Type;

   function Check
      (C     : in Collection'Class;
       State : in State_Array)
       return Result
   is
      First  : Item_Type renames State (State'First);
      Last   : Item_Type renames State (State'Last);
      Length : constant Natural := State'Length;
   begin
      if not (C.First = First) then
         return Failed ("First item incorrect");
      end if;

      if not (C.Last = Last) then
         return Failed ("Last item incorrect");
      end if;

      if not (C.Length = Length) then
         return Failed ("Incorrect length");
      end if;

      if not (C.Available = C.Max_Size - Length) then
         return Failed ("Available incorrect");
      end if;

      if not (if C.Length = 0 then C.Is_Empty else not C.Is_Empty) then
         return Failed
            ("Empty flag not consistent with length " &
                "Length =" & C.Length'Img & "=> Is_Empty = " & C.Is_Empty'Img);
      end if;

      if not (if C.Length = C.Max_Size then C.Is_Full else not C.Is_Full) then
         return Failed
            ("Full flag not consistent with length " &
             "Length =" & C.Length'Img & "=> Is_Full = " & C.Is_Full'Img);
      end if;

      for Index in 1 .. C.Max_Size loop
         if Index <= C.Length then
            if not (C.Is_Valid_Index (Index)) then
               return Failed ("Valid index reported as invalid");
            end if;
         else
            if C.Is_Valid_Index (Index) then
               return Failed ("Invalid index reported as valid");
            end if;
         end if;
      end loop;

      for Index in 1 .. C.Length loop
         if not (State (Index) = C.Item_At (Index)) then
            return Failed ("Item at index" & Index'Img & " does not match");
         end if;
      end loop;

      return Success;
   end Check;

   procedure Insert
   is
      C : Collection;
   begin
      C.Insert ('a'); --  Insert front to empty
      declare
         S : constant Result := Check (C, (1 => 'a'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Insert ('b'); --  Insert front to non-empty
      declare
         S : constant Result := Check (C, ('b', 'a'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Clear;

      C.Insert ('a', Before => 1); --  Directed insert front to empty
      declare
         S : constant Result := Check (C, (1 => 'a'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Insert ('b', Before => 1); --  Directed insert front to non-empty
      declare
         S : constant Result := Check (C, ('b', 'a'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Insert ('c', Before => 2); --  Directed insert middle to non-empty
      declare
         S : constant Result := Check (C, ('b', 'c', 'a'));
      begin
         Assert (S.Status, S.Description);
      end;
   end Insert;

   procedure Append
   is
      C : Collection;
   begin
      C.Append ('a'); --  Append last to empty
      declare
         S : constant Result := Check (C, (1 => 'a'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Append ('b'); --  Append last to non-empty
      declare
         S : constant Result := Check (C, ('a', 'b'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Clear;

      C.Append ('a', After => 1); --  Directed append last to empty
      declare
         S : constant Result := Check (C, (1 => 'a'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Append ('b', After => 1); --  Directed append last to non-empty
      declare
         S : constant Result := Check (C, ('a', 'b'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Append ('c', After => 1); --  Directed append middle to non-empty
      declare
         S : constant Result := Check (C, ('a', 'c', 'b'));
      begin
         Assert (S.Status, S.Description);
      end;
   end Append;

   procedure Remove
   is
      C : Collection;
   begin
      C.Append ('a');
      C.Append ('b');
      C.Append ('c');
      C.Append ('d');

      C.Remove (2); --  Remove from middle
      declare
         S : constant Result := Check (C, ('a', 'c', 'd'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Remove (1); --  Remove from front
      declare
         S : constant Result := Check (C, ('c', 'd'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Remove (2); --  Remove from back
      declare
         S : constant Result := Check (C, (1 => 'c'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Remove (1); --  Remove last
      Assert (C.Is_Empty, "Not empty after removing last item");
   end Remove;

   procedure Location
   is
      C : Collection;
   begin
      Assert (C.Location ('a') = Collection_Pack.No_Index, "Found in empty");

      C.Append ('a'); --  a

      Assert (C.Location ('a') = 1, "Did not find location for size 1");

      C.Append ('b'); --  ab

      Assert (C.Location ('b') = 2, "Did not find location for last");

      C.Append ('c'); --  abc

      Assert (C.Location ('b') = 2, "Did not find location for middle");

      C.Append ('c'); --  abcc

      Assert (C.Location ('c') = 3, "Did not find location for first duplicate");
   end Location;

   procedure Replace
   is
      C : Collection;
   begin
      C.Append ('a');

      C.Replace (1, 'b');
      declare
         S : constant Result := Check (C, (1 => 'b'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Append ('c');

      C.Replace (1, 'd');
      declare
         S : constant Result := Check (C, ('d', 'c'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Replace (2, 'e');
      declare
         S : constant Result := Check (C, ('d', 'e'));
      begin
         Assert (S.Status, S.Description);
      end;

      C.Append ('f');

      C.Replace (2, 'g');
      declare
         S : constant Result := Check (C, ('d', 'g', 'f'));
      begin
         Assert (S.Status, S.Description);
      end;
   end Replace;

   procedure Insert_Sorted
   is
      use type Sorted.T;
      C : Sorted.Collection;
   begin
      C.Insert (('a', 1)); --  a1
      C.Insert (('c', 1)); --  a1c1
      C.Insert (('b', 1)); --  a1b1c1
      C.Insert (('b', 2)); --  a1b2b1c1
      Assert (C.Item_At (1) = ('a', 1), "");
      Assert (C.Item_At (2) = ('b', 2), "");
      Assert (C.Item_At (3) = ('b', 1), "");
      Assert (C.Item_At (4) = ('c', 1), "");

      C.Clear;

      C.Insert (('a', 1));              --  a1
      C.Insert (('c', 1), Before => 1); --  a1c1
      C.Insert (('c', 2), Before => 2); --  a1c2c1
      C.Insert (('c', 3), Before => 3); --  a1c2c3c1
      Assert (C.Item_At (1) = ('a', 1), "");
      Assert (C.Item_At (2) = ('c', 2), "");
      Assert (C.Item_At (3) = ('c', 3), "");
      Assert (C.Item_At (4) = ('c', 1), "");
   end Insert_Sorted;

   procedure Append_Sorted
   is
      use type Sorted.T;
      C : Sorted.Collection;
   begin
      C.Append (('b', 1)); --  b1
      C.Append (('a', 1)); --  a1b1
      C.Append (('a', 2)); --  a1a2b1
      C.Append (('a', 3)); --  a1a2a3b1
      Assert (C.Item_At (1) = ('a', 1), "");
      Assert (C.Item_At (2) = ('a', 2), "");
      Assert (C.Item_At (3) = ('a', 3), "");
      Assert (C.Item_At (4) = ('b', 1), "");

      C.Clear;

      C.Append (('b', 1));             --  b1
      C.Append (('a', 1), After => 1); --  a1b1
      C.Append (('a', 2), After => 2); --  a1a2b1
      C.Append (('a', 3), After => 1); --  a1a3a2b1
      Assert (C.Item_At (1) = ('a', 1), "");
      Assert (C.Item_At (2) = ('a', 3), "");
      Assert (C.Item_At (3) = ('a', 2), "");
      Assert (C.Item_At (4) = ('b', 1), "");
   end Append_Sorted;

   procedure Replace_Sorted
   is
      use type Sorted.T;
      C : Sorted.Collection;
   begin
      C.Append (('b', 1));
      C.Append (('c', 1));
      C.Append (('c', 2));
      C.Append (('c', 3));
      C.Append (('d', 1)); --  b1 c1 c2 c3 d1

      C.Replace (3, ('b', 0)); --  b1 b0 c1 c3 d1
      Assert (C.Item_At (1) = ('b', 1), "");
      Assert (C.Item_At (2) = ('b', 0), "");
      Assert (C.Item_At (3) = ('c', 1), "");
      Assert (C.Item_At (4) = ('c', 3), "");
      Assert (C.Item_At (5) = ('d', 1), "");

      C.Replace (3, ('d', 0)); --  b1 b0 c3 d0 d1
      Assert (C.Item_At (1) = ('b', 1), "");
      Assert (C.Item_At (2) = ('b', 0), "");
      Assert (C.Item_At (3) = ('c', 3), "");
      Assert (C.Item_At (4) = ('d', 0), "");
      Assert (C.Item_At (5) = ('d', 1), "");

      C.Replace (3, ('c', 0)); --  b1 b0 c0 d0 d1
      Assert (C.Item_At (1) = ('b', 1), "");
      Assert (C.Item_At (2) = ('b', 0), "");
      Assert (C.Item_At (3) = ('c', 0), "");
      Assert (C.Item_At (4) = ('d', 0), "");
      Assert (C.Item_At (5) = ('d', 1), "");
   end Replace_Sorted;

   procedure First_Empty is
      C : Collection;
      Tmp : Item_Type;
      pragma Unreferenced (Tmp);
   begin
      C.Clear;
      Tmp := C.First;
   end First_Empty;

   procedure Last_Empty is
      C : Collection;
      Tmp : Item_Type;
      pragma Unreferenced (Tmp);
   begin
      C.Clear;
      Tmp := C.Last;
   end Last_Empty;

   procedure Item_At_Empty is
      C : Collection;
      Tmp : Item_Type;
      pragma Unreferenced (Tmp);
   begin
      C.Clear;
      Tmp := C.Item_At (1);
   end Item_At_Empty;

   procedure Remove_Empty is
      C : Collection;
   begin
      C.Clear;
      C.Remove (1);
   end Remove_Empty;

   procedure Replace_Empty is
      C : Collection;
   begin
      C.Clear;
      C.Replace (1, 'x');
   end Replace_Empty;

   procedure Fill
      (C : in out Collection)
   is
   begin
      while not C.Is_Full loop
         C.Append ('X');
      end loop;
   end Fill;

   procedure Insert_Full is
      C : Collection;
   begin
      Fill (C);
      C.Insert ('X');
   end Insert_Full;

   procedure Insert_Before_Full is
      C : Collection;
   begin
      Fill (C);
      C.Insert ('X', Before => 1);
   end Insert_Before_Full;

   procedure Append_Full is
      C : Collection;
   begin
      Fill (C);
      C.Append ('X');
   end Append_Full;

   procedure Append_After_Full is
      C : Collection;
   begin
      Fill (C);
      C.Append ('X', After => 1);
   end Append_After_Full;

   procedure Preconditions
   is
   begin
      Assert_Exception
         (First_Empty'Access,
          "Expected an exception when accessing first item for empty");

      Assert_Exception
         (Last_Empty'Access,
          "Expected an exception when accessing last item for empty");

      Assert_Exception
         (Item_At_Empty'Access,
          "Expected an exception when accessing item for empty");

      Assert_Exception
         (Remove_Empty'Access,
          "Expected an exception when removing item for empty");

      Assert_Exception
         (Replace_Empty'Access,
          "Expected an exception when replacing item for empty");

      Assert_Exception
         (Insert_Full'Access,
          "Expected an exception when inserting item to full");

      Assert_Exception
         (Insert_Before_Full'Access,
          "Expected an exception when inserting item to full");

      Assert_Exception
         (Append_Full'Access,
          "Expected an exception when appending item to full");

      Assert_Exception
         (Append_After_Full'Access,
          "Expected an exception when appending item to full");
   end Preconditions;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Init_Test;
      Fill_And_Empty;
      Insert;
      Append;
      Remove;
      Location;
      Replace;
      Preconditions;

      Insert_Sorted;
      Append_Sorted;
      Replace_Sorted;
   end Run_Test;

end Generic_Collection_Test;
