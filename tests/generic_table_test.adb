with AUnit.Assertions; use AUnit.Assertions;

with ACO.Utils.DS.Generic_Table;

package body Generic_Table_Test is
   pragma Assertion_Policy (Check);

   type Key_Type is new Character;

   subtype Test_Keys is Key_Type range 'a' .. 'z';

   type Value_Type is new Positive;

   function Hashit (Key : Key_Type) return Natural is
      (Key_Type'Pos (Key));

   Number_Of_Buckets : constant := 3;
   Maximum_Size      : constant :=
      Test_Keys'Pos(Test_Keys'Last) - Test_Keys'Pos(Test_Keys'First) + 1;

   package Table_Pack is new ACO.Utils.DS.Generic_Table
      (Key_Type   => Key_Type,
       "="        => "=",
       Hash       => Hashit,
       Value_Type => Value_Type);

   subtype T is Table_Pack.Table
      (Number_Of_Buckets => Number_Of_Buckets,
       Maximum_Size      => Maximum_Size);

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Data Structures: Generic Table Test");
   end Name;

   procedure Init_Test
   is
      Table : T;
   begin
      Assert (Table.Is_Empty, "Table is not initially empty");
      Assert (Table.Length = 0, "Table has not initially the length zero");
      Assert (not Table.Is_Full, "Table is initially full");

      for K in Key_Type'Range loop
         Assert (not Table.Is_Bound (K), "Table has a bound key at init");
      end loop;
   end Init_Test;

   procedure Asserted_Fill
      (Table : in out T)
   is
      V : Value_Type;
      L : Natural := 0;
   begin
      for K in Test_Keys'Range loop
         V := Key_Type'Pos(K);

         Table.Bind (K, V);

         L := L + 1;

         Assert (Table.Is_Bound (K), "Key is not bound");
         Assert (Table.Value_Of (K) = V, "Value for key incorrect");
         Assert (Table.Length = L, "Length is not" & L'Img);
      end loop;
      Assert (Table.Is_Full, "Table not full");
      Assert (not Table.Is_Empty, "Table is empty");
   end Asserted_Fill;

   procedure Bind
   is
      Table : T;
   begin
      Asserted_Fill (Table);
   end Bind;

   procedure Bind_New_To_Full
   is
      Table : T;
      K : constant Key_Type := Key_Type'Succ (Test_Keys'Last);
   begin
      Asserted_Fill (Table);
      Table.Bind (K, 1337);
   end Bind_New_To_Full;

   procedure Bind_Existing_To_Full
   is
      Table : T;
      K : constant Key_Type := Test_Keys'First;
      V : constant Value_Type := 1337;
      L : Natural;
   begin
      Asserted_Fill (Table); --  Should fill with keys in Test_Keys'Range
      L := Table.Length;

      Assert (Table.Value_Of (K) /= V,
              "Test precondition fail: test value already bound to test key");

      Table.Bind (K, V);

      Assert (Table.Value_Of (K) = V,
              "Value of existing key not replaced when binding to full table");
      Assert (Table.Length = L, "Length has changed");
      Assert (Table.Is_Full, "Table shall still be full");
   end Bind_Existing_To_Full;

   procedure Unbind
   is
      Table : T;
      L : Natural;
   begin
      Asserted_Fill (Table);

      L := Table.Length;

      for K in Test_Keys'Range loop
         Table.Unbind (K);

         L := L - 1;

         Assert (not Table.Is_Bound (K), "Key is bound after unbind");
         Assert (Table.Length = L, "Length is not" & L'Img);
      end loop;
      Assert (Table.Is_Empty, "Table not empty");
      Assert (not Table.Is_Full, "Table is full");
   end Unbind;

   procedure Unbind_Empty
   is
      Table : T;
      K : constant Key_Type := 'X';
   begin
      Table.Unbind (K);
      Assert (not Table.Is_Bound (K), "Key is bound after unbinding empty table");
      Assert (Table.Length = 0, "Length not 0 after unbinding to empty table");
      Assert (Table.Is_Empty, "Table not empty after unbinding to empty table");
      Assert (not Table.Is_Full, "Table is full after unbinding to empty table");
   end Unbind_Empty;

   procedure Value_Of_Nonexisting
   is
      Table : T;
      V : Value_Type;
      K : constant Key_Type := Key_Type'Succ (Test_Keys'Last);
      pragma Unreferenced (V);
   begin
      V := Table.Value_Of (K);
   end Value_Of_Nonexisting;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Init_Test;
      Bind;
      Unbind;
      Unbind_Empty;
      Bind_Existing_To_Full;
      Assert_Exception
         (Bind_New_To_Full'Access,
          "Expected an exception when binding a new key to an already full table");
      Assert_Exception
         (Value_Of_Nonexisting'Access,
          "Expected an exception when accessing value of key not bound");
   end Run_Test;

end Generic_Table_Test;
