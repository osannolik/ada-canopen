with ACO.OD_Types;

generic
   type Item_Type is private;
   with function Convert (Data : Item_Type) return ACO.OD_Types.Byte_Array;

package ACO.Generic_Entry_Types is

   pragma Preelaborate;

   use ACO.OD_Types;

   type Entry_Type is new ACO.OD_Types.Entry_Base with private;

   function Create (Accessability : Access_Mode;
                    Data          : Item_Type)
                    return Entry_Type;

   function Read (This : Entry_Type) return Item_Type
      with Pre => This.Is_Readable;

   overriding
   function Read (This : Entry_Type) return Byte_Array
      with Pre => This.Is_Readable;

private

   type Entry_Type is new ACO.OD_Types.Entry_Base with record
      Data : Item_Type;
   end record;

end ACO.Generic_Entry_Types;
