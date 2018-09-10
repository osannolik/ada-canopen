with Ada.Unchecked_Conversion;
with ACO.Utils.Byte_Order;

package body ACO.Generic_Entry_Types is
   use ACO.Utils.Byte_Order;

   function Swap (X : Byte_Array) return Byte_Array is
      (Byte_Array (Swap_Bus (Octets (X))))
   with Inline;

   function Read (This : Entry_Type) return Item_Type is
      (This.Data);

   function Read (This : Entry_Type) return Byte_Array is
      (Swap (Convert (This.Data)));

   function Data_Length (This : Entry_Type) return Natural is
      (Item_Type'Size / 8);

   procedure Write (This : in out Entry_Type;
                    Data : in     Item_Type)
   is
   begin
      This.Data := Data;
   end Write;

   procedure Write (This  : in out Entry_Type;
                    Bytes : in     Byte_Array)
   is
   begin
      This.Data := Convert (Swap (Bytes));
   end Write;

   function Create
      (Accessability : Access_Mode;
       Data          : Item_Type) return Entry_Type
   is ((Accessability, Data));

   function Convert (Data : Item_Type) return Byte_Array
   is
      Bytes : constant Byte_Array (0 .. Data'Size / 8 - 1);
      for Bytes'Address use Data'Address;
      pragma Import (Convention => Ada, Entity => Bytes);
   begin
      return Bytes;
   end Convert;

   function Convert (Bytes : Byte_Array) return Item_Type is
      --  Might get alignment issues?
      function To_Item is new Ada.Unchecked_Conversion
         (Source => Byte_Array,
          Target => Item_Type);
   begin
      return To_Item (Bytes);
   end Convert;

   function "=" (L : Entry_Type; R : Item_Type) return Boolean is
      (L.Data = R);

   function "=" (L : Item_Type;  R : Entry_Type) return Boolean is
      (L = R.Data);

end ACO.Generic_Entry_Types;
