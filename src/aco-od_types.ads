with Interfaces;

package ACO.OD_Types is

   pragma Preelaborate;

   subtype Object_Index is Interfaces.Unsigned_16;

   subtype Object_Subindex is Interfaces.Unsigned_8;

   type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8;

   Empty : Byte_Array (1 .. 0);

   type Access_Mode is (RW, RO, WO);

   type Entry_Base is abstract tagged record
      Accessability : Access_Mode := RW;
   end record;

   type Entry_Base_Ref is access all Entry_Base'Class;

   function Is_Readable (This : Entry_Base) return Boolean is
      (case This.Accessability is
          when RW | RO => True,
          when WO      => False);

   function Is_Writable (This : Entry_Base) return Boolean is
      (case This.Accessability is
          when RW | WO => True,
          when RO      => False);

   function Read (This : Entry_Base) return Byte_Array is abstract;


   type Entry_Ref is not null access all Entry_Base'Class;

   type Entry_Array is array (Object_Subindex range <>) of Entry_Ref;

   type Object_Base (Entries : not null access Entry_Array) is tagged null record;

   type Object_Ref is access all Object_Base'Class;

   No_Object : constant Object_Ref := null;

   subtype Index_Type is Integer range -1 .. Integer'Last;

   No_Index : constant := Index_Type'First;

   subtype Profile_Index_Type is Index_Type range 0 .. Index_Type'Last;

   type Profile_Objects is array (Profile_Index_Type range <>) of Object_Ref;

   type Profile_Objects_Ref is access all Profile_Objects;

end ACO.OD_Types;
