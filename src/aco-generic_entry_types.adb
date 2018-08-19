package body ACO.Generic_Entry_Types is

   function Read (This : Entry_Type) return Item_Type is
      (This.Data);

   function Read (This : Entry_Type) return Byte_Array is
      (Convert (This.Data));

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

end ACO.Generic_Entry_Types;
