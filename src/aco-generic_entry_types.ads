with ACO.OD_Types;

generic
   type Item_Type is private;

package ACO.Generic_Entry_Types is
   --  Numeric type.
   --  When filled with byte array the data is byte swapped if needed.

   pragma Preelaborate;

   use ACO.OD_Types;

   type Entry_Type is new ACO.OD_Types.Entry_Base with private;

   function Create (Accessability : Access_Mode;
                    Data          : Item_Type)
                    return Entry_Type;

   function Read (This : Entry_Type) return Item_Type;

   procedure Write (This : in out Entry_Type;
                    Data : in     Item_Type);

   overriding
   function Data_Length (This : Entry_Type) return Natural;

   overriding
   function Read (This : Entry_Type) return Byte_Array
      with Post => Read'Result'Length = Item_Type'Size / 8;

   overriding
   procedure Write (This  : in out Entry_Type;
                    Bytes : in     Byte_Array)
      with Pre => Bytes'Length = Item_Type'Size / 8;

   function "=" (L : Entry_Type; R : Item_Type) return Boolean
      with Inline;

   function "=" (L : Item_Type;  R : Entry_Type) return Boolean
      with Inline;

private

   function Convert (Data : Item_Type) return Byte_Array;

   function Convert (Bytes : Byte_Array) return Item_Type;

   type Entry_Type is new ACO.OD_Types.Entry_Base with record
      Data : Item_Type;
   end record;

end ACO.Generic_Entry_Types;
