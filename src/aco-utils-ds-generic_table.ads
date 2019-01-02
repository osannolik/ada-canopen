with Ada.Finalization;

generic

   type Key_Type is private;
   with function "=" (L : Key_Type; R : Key_Type) return Boolean;
   with function Hash (Key : Key_Type) return Natural;

   type Value_Type is private;

package ACO.Utils.DS.Generic_Table is

   pragma Preelaborate;

   type Table
      (Number_Of_Buckets : Positive;
       Maximum_Size      : Positive)
   is new Ada.Finalization.Controlled with private;

   function Length (This : Table) return Natural;

   function Is_Full (This : Table) return Boolean;

   function Is_Empty (This : Table) return Boolean;

   function Is_Bound (This : Table; Key : Key_Type) return Boolean;

   procedure Bind
      (This  : in out Table;
       Key   : in     Key_Type;
       Value : in     Value_Type)
      with Pre => (if not This.Is_Bound (Key) then not This.Is_Full),
           Post => This.Is_Bound (Key);

   function Value_Of
      (This : Table;
       Key  : Key_Type)
       return Value_Type
      with Pre => This.Is_Bound (Key);

   procedure Unbind
      (This : in out Table;
       Key  : in     Key_Type)
      with Post => not This.Is_Bound (Key);

private

   subtype Index is Natural;

   No_Index : constant Index := Index'First;

   subtype Cell_Index is Index range No_Index + 1 .. Index'Last;

   subtype Bucket_Index is Positive;

   type Cell is record
      Key   : Key_Type;
      Value : Value_Type;
      Next  : Index;
   end record;

   type Cell_Array is array (Cell_Index range <>) of Cell;

   type Bucket_Array is array (Bucket_Index range <>) of Index;

   type Table
      (Number_Of_Buckets : Positive;
       Maximum_Size      : Positive)
   is new Ada.Finalization.Controlled with record
      Buckets : Bucket_Array (1 .. Number_Of_Buckets);
      Data    : Cell_Array (1 .. Maximum_Size);
      Size    : Natural;
      Unused  : Index;
   end record;

   procedure Initialize (This : in out Table);

   procedure Clear (This : in out Table)
      with Post => This.Is_Empty;

   function To_Bucket_Index (This : Table; Key : Key_Type) return Bucket_Index
      with Inline;

   function Location (This : Table; Start : Index; Key : Key_Type) return Index;

end ACO.Utils.DS.Generic_Table;
