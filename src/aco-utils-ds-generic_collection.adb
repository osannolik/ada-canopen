package body ACO.Utils.DS.Generic_Collection is

   procedure Initialize
      (C : in out Collection)
   is
   begin
      Clear (C);
   end Initialize;

   procedure Clear
      (C : in out Collection)
   is
   begin
      C.Start := 1;
      C.Size := 0;
   end Clear;

   function Is_Full
      (C : Collection)
       return Boolean
   is
      (C.Size >= C.Max_Size);

   function Is_Empty
      (C : Collection)
       return Boolean
   is
      (C.Size = 0);

   function Is_Valid_Index
      (C : Collection;
       Index : Positive)
       return Boolean
   is
      (Index <= C.Size);

   function Available
      (C : Collection)
       return Natural
   is
      (C.Max_Size - C.Size);

   function Length
      (C : Collection)
       return Natural
   is
      (C.Size);

   function First
      (C : Collection)
       return Item_Type
   is
      (C.Items (C.Start));

   function Last
      (C : Collection)
       return Item_Type
   is
      (C.Items (((C.Start - 1 + C.Size - 1) mod C.Max_Size) + 1));

   procedure Insert
      (C    : in out Collection;
       Item : in     Item_Type)
   is
   begin
      C.Start := ((C.Start - 2) mod C.Max_Size) + 1;
      C.Size := C.Size + 1;
      C.Items (C.Start) := Item;
   end Insert;

   procedure Insert
      (C      : in out Collection;
       Item   : in     Item_Type;
       Before : in     Positive)
   is
   begin
      if C.Size = 0 or else Before = 1 then
         Insert (C, Item);
      else
         --  We are inserting in the middle.
         --
         --  In the comments below, 'left' means the part of Items
         --  before the Itement which the new entry is to be inserted
         --  before (indexed by Actual), 'right' means the part after.
         declare
            Start : Item_Index renames C.Start;
            Actual : constant Item_Index :=
               ((Start - 1 + Before - 1) mod C.Max_Size) + 1;
            Last : constant Item_Index :=
               ((Start - 1 + C.Size - 1) mod C.Max_Size) + 1;
         begin
            if Start = 1 or else Start > Actual then
               --  the left part is wedged, shift the right part up
               C.Items (Actual + 1 .. Last + 1) := C.Items (Actual .. Last);
               C.Items (Actual) := Item;
            elsif Last = C.Items'Last or else Last < Actual then
               --  the right part is wedged, shift the left part down
               C.Items (Start - 1 .. Actual - 2) :=
                  C.Items (Start .. Actual - 1);
               Start := Start - 1;
               C.Items (Actual - 1) := Item;
            elsif Before < C.Size / 2 then
               --  the left part is shorter, shift it down
               C.Items (Start - 1 .. Actual - 2) :=
                  C.Items (Start .. Actual - 1);
               Start := Start - 1;
               C.Items (Actual - 1) := Item;
            else
               --  the right part is shorter, shift it up
               C.Items (Actual + 1 .. Last + 1) := C.Items (Actual .. Last);
               C.Items (Actual) := Item;
            end if;
            C.Size := C.Size + 1;
         end;
      end if;
   end Insert;

   procedure Append
      (C    : in out Collection;
       Item : in     Item_Type)
   is
   begin
      C.Size := C.Size + 1;
      C.Items (((C.Start - 1 + C.Size - 1) mod C.Max_Size) + 1) := Item;
   end Append;

   procedure Append
      (C     : in out Collection;
       Item  : in     Item_Type;
       After : in     Positive)
   is
   begin
      if C.Size = 0 or else After = C.Size then
         Append (C, Item);
      else
         Insert (C, Item, Before => After + 1);
      end if;
   end Append;

   procedure Remove
      (C    : in out Collection;
       From : in     Positive)
   is
   begin
      if C.Size = 1 then
         Clear (C);
      elsif From = 1 then
         C.Start := (C.Start mod C.Max_Size) + 1;
         C.Size := C.Size - 1;
      elsif From = C.Size then
         C.Size := C.Size - 1;
      else
         --  We are removing from the middle.
         --
         --  In the comments below, 'left' means the part of Items
         --  before the Itement to be removed (indexed by Actual),
         --  'right' means the part after.
         declare
            Start : Item_Index renames C.Start;
            Actual : constant Item_Index :=
               ((Start - 1 + From - 1) mod C.Max_Size) + 1;
            Last : constant Item_Index :=
               ((Start - 1 + C.Size - 1) mod C.Max_Size) + 1;
         begin
            if Start > Actual then
               --  the left part wraps round; shift the right part down
               C.Items (Actual .. Last - 1) := C.Items (Actual + 1 .. Last);
            elsif Actual > Last then
               --  the right part wraps round; shift the left part up
               C.Items (Start + 1 .. Actual) := C.Items (Start .. Actual - 1);
               Start := Start + 1;
            elsif C.Max_Size > 1 and then From < C.Size / 2 then
               --  the left part is shorter
               C.Items (Start + 1 .. Actual) := C.Items (Start .. Actual - 1);
               Start := Start + 1;
            else
               --  the right part is shorter
               C.Items (Actual .. Last - 1) := C.Items (Actual + 1 .. Last);
            end if;
            C.Size := C.Size - 1;
         end;
      end if;
   end Remove;

   procedure Replace
      (C     : in out Collection;
       Index : in     Positive;
       Item  : in     Item_Type)
   is
   begin
      C.Items (((C.Start - 1 + Index - 1) mod C.Max_Size) + 1) := Item;
   end Replace;

   function Item_At
      (C     : Collection;
       Index : Positive)
       return Item_Type
   is
      (C.Items (((C.Start - 1 + Index - 1) mod C.Max_Size) + 1));

   function Location
      (C     : Collection;
       Item  : Item_Type;
       Start : Positive := 1)
       return Natural
   is
   begin
      if C.Size = 0 then
         return No_Index;
      end if;

      for I in Start .. C.Size loop
         if C.Items (((C.Start - 1 + I - 1) mod C.Max_Size) + 1) = Item then
            return I;
         end if;
      end loop;

      return No_Index;
   end Location;

end ACO.Utils.DS.Generic_Collection;
