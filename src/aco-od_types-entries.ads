with ACO.Generic_Entry_Types;

package ACO.OD_Types.Entries is

   pragma Preelaborate;

   use Interfaces;

--   type U8 is new Natural range 0 .. 2**8-1 with Size => 8;

   subtype U8 is Interfaces.Unsigned_8;

   package U8_Pack is new ACO.Generic_Entry_Types (U8);

   type Entry_U8 is new U8_Pack.Entry_Type with null record;


   type U16 is new Interfaces.Unsigned_16 with Alignment => 1;

   package U16_Pack is new ACO.Generic_Entry_Types (U16);

   type Entry_U16 is new U16_Pack.Entry_Type with null record;


--   type U32 is new Natural range 0 .. 2**32-1 with Size => 32;

   type U32 is new Interfaces.Unsigned_32 with Alignment => 1;

   package U32_Pack is new ACO.Generic_Entry_Types (U32);

   type Entry_U32 is new U32_Pack.Entry_Type with null record;


   type F32 is new IEEE_Float_32;

   package F32_Pack is new ACO.Generic_Entry_Types (F32);

   type Entry_F32 is new F32_Pack.Entry_Type with null record;


   subtype Visible_String is String;

end ACO.OD_Types.Entries;
