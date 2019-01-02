package body ACO.Utils.DS.Generic_Table is

   procedure Initialize
      (This : in out Table)
   is
   begin
      Clear (This);
   end Initialize;

   procedure Clear
      (This : in out Table)
   is
   begin
      This.Buckets := (others => No_Index);

      for C in This.Data'First .. This.Data'Last - 1 loop
         This.Data (C).Next := C + 1;
      end loop;

      This.Data (This.Data'Last).Next := No_Index;
      This.Unused := This.Data'First;
      This.Size := 0;
   end Clear;

   function Length
      (This : Table)
       return Natural
   is
      (This.Size);

   function Is_Full
      (This : Table)
       return Boolean
   is
      (This.Size >= This.Maximum_Size);

   function Is_Empty
      (This : Table)
       return Boolean
   is
      (This.Size = 0);

   function Location
      (This  : Table;
       Start : Index;
       Key   : Key_Type)
       return Index
   is
      Result : Index := Start;
   begin
      while Result in Cell_Index'Range loop
         if This.Data (Result).Key = Key then
            return Result;
         end if;
         Result := This.Data (Result).Next;
      end loop;

      return No_Index;
   end Location;

   function To_Bucket_Index
      (This : Table;
       Key  : Key_Type)
       return Bucket_Index
   is
      ((Hash (Key) mod This.Number_Of_Buckets) + 1);

   procedure Bind
      (This  : in out Table;
       Key   : in     Key_Type;
       Value : in     Value_Type)
   is
      Bucket : constant Bucket_Index := To_Bucket_Index (This, Key);
      B      : Index renames This.Buckets (Bucket);
      C      : constant Index := Location (This, B, Key);
   begin
      if C in Cell_Index'Range then
         --  Key already exist, rebind to new value
         This.Data (C).Value := Value;
      else
         --  New key-value binding
         declare
            Unused_Cell : Cell renames This.Data (This.Unused);
            Next : constant Index := Unused_Cell.Next;
         begin
            Unused_Cell := (Key   => Key,
                            Value => Value,
                            Next  => B);
            B := This.Unused;
            This.Unused := Next;
         end;

         This.Size := This.Size + 1;
      end if;
   end Bind;

   procedure Unbind
      (This : in out Table;
       Key  : in     Key_Type)
   is
      Bucket   : constant Bucket_Index := To_Bucket_Index (This, Key);
      Current  : Index := This.Buckets (Bucket);
      Previous : Index := 0;
   begin
      while Current in Cell_Index'Range loop
         exit when This.Data (Current).Key = Key;

         Previous := Current;
         Current := This.Data (Current).Next;
      end loop;

      if Current in Cell_Index'Range then
         if Previous = No_Index then
            This.Buckets (Bucket) := This.Data (Current).Next;
         else
            This.Data (Previous).Next := This.Data (Current).Next;
         end if;

         This.Data (Current).Next := This.Unused;
         This.Unused := Current;
         This.Size := This.Size - 1;
      end if;
   end Unbind;

   function Is_Bound
      (This : Table;
       Key  : Key_Type)
       return Boolean
   is
      Bucket : constant Bucket_Index := To_Bucket_Index (This, Key);
   begin
      return Location (This, This.Buckets (Bucket), Key) in Cell_Index'Range;
   end Is_Bound;

   function Value_Of
      (This : Table;
       Key  : Key_Type)
       return Value_Type
   is
      Bucket : constant Bucket_Index := To_Bucket_Index (This, Key);
      C : constant Cell_Index := Location (This, This.Buckets (Bucket), Key);
   begin
      return This.Data (C).Value;
   end Value_Of;

end ACO.Utils.DS.Generic_Table;
