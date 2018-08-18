with ACO.Generic_Entry_Types;

package ACO.OD_Types.Entries is

   pragma Preelaborate;

   use Interfaces;

   subtype U8_Type is Unsigned_8;

   function Convert (Data : U8_Type) return Byte_Array;

   package U8_Pack is new ACO.Generic_Entry_Types
      (Item_Type => U8_Type,
       Convert   => Convert);

   type Entry_U8 is new U8_Pack.Entry_Type with null record;


   subtype F32_Type is IEEE_Float_32;

   function Convert (Data : F32_Type) return Byte_Array;

   package F32_Pack is new ACO.Generic_Entry_Types
      (Item_Type => F32_Type,
       Convert   => Convert);

   type Entry_F32 is new F32_Pack.Entry_Type with null record;

end ACO.OD_Types.Entries;
