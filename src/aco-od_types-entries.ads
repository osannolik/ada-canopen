with ACO.Generic_Entry_Types;

package ACO.OD_Types.Entries is

   pragma Preelaborate;

   use Interfaces;

   package U8_Pack is new ACO.Generic_Entry_Types (Unsigned_8);

   type Entry_U8 is new U8_Pack.Entry_Type with null record;


   package U32_Pack is new ACO.Generic_Entry_Types (Unsigned_32);

   type Entry_U32 is new U32_Pack.Entry_Type with null record;


   package F32_Pack is new ACO.Generic_Entry_Types (IEEE_Float_32);

   type Entry_F32 is new F32_Pack.Entry_Type with null record;


   subtype Visible_String is String;

end ACO.OD_Types.Entries;
