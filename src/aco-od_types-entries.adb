package body ACO.OD_Types.Entries is

   function Convert (Data : U8_Type) return Byte_Array is
      (Byte_Array'(0 => Data));

   function Convert (Data : F32_Type) return Byte_Array
   is
      Bytes : constant Byte_Array (0 .. 3);
      for Bytes'Address use Data'Address;
      pragma Import (Convention => Ada, Entity => Bytes);
   begin
      return Bytes;
   end Convert;

end ACO.OD_Types.Entries;
