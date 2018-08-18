package body ACO.Generic_Entry_Types is

   function Read (This : Entry_Type) return Item_Type is
      (This.Data);

   function Read (This : Entry_Type) return Byte_Array is
      (Convert (This.Data));

   function Create
      (Accessability : Access_Mode;
       Data          : Item_Type) return Entry_Type
   is ((Accessability, Data));

end ACO.Generic_Entry_Types;
