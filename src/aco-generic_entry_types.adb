package body ACO.Generic_Entry_Types is

   function Read (This : Entry_Type) return Item_Type is
      (This.Data);

   function Read (This : Entry_Type) return Byte_Array is
      (Convert (This.Data));

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
      This.Data := Convert (Bytes);
   end Write;

   function Create
      (Accessability : Access_Mode;
       Data          : Item_Type) return Entry_Type
   is ((Accessability, Data));

   function Convert (Data : Item_Type) return Byte_Array
   is
      Bytes : constant Byte_Array (0 .. Item_Type'Size / 8 - 1);
      for Bytes'Address use Data'Address;
      pragma Import (Convention => Ada, Entity => Bytes);
   begin
      return Bytes;
   end Convert;

   function Convert (Bytes : Byte_Array) return Item_Type
   is
      Data : constant Item_Type;
      for Data'Address use Bytes'Address;
      pragma Import (Convention => Ada, Entity => Data);
   begin
      return Data;
   end Convert;

   function "=" (L : Entry_Type; R : Item_Type) return Boolean is
      (L.Data = R);

   function "=" (L : Item_Type;  R : Entry_Type) return Boolean is
      (L = R.Data);

end ACO.Generic_Entry_Types;
