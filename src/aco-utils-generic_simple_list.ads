generic
   type Element_Type is private;
   with function "=" (L, R : Element_Type) return Boolean;
   Max_Nof_Elements : Positive;

package ACO.Utils.Generic_Simple_List is
   --  Incredibly and stupidly simple bounded list/set

   pragma Preelaborate;

   type List_Type is tagged private;

   No_Index : constant Natural := 0;

   function In_List
      (This    : List_Type;
       Element : Element_Type)
       return Boolean;

   function Is_Full
      (This : List_Type)
       return Boolean;

   procedure Add
      (This    : in out List_Type;
       Element : in     Element_Type)
      with Pre => not This.Is_Full;

   procedure Remove
      (This    : in out List_Type;
       Element : in     Element_Type);

   procedure Remove
      (This  : in out List_Type;
       Index : in     Natural);

   function Get
      (This  : List_Type;
       Index : Natural)
       return Element_Type
      with Pre => Index /= No_Index;

   function Length
      (This : List_Type)
       return Natural;

   function Index
      (This    : List_Type;
       Element : Element_Type)
       return Natural;

private

   type Item_Type is record
      Is_Used : Boolean := False;
      Element : Element_Type;
   end record;

   type Item_Array is array (Natural range <>) of Item_Type;

   type List_Type is tagged record
      Items : Item_Array (1 .. Max_Nof_Elements);
   end record;

end ACO.Utils.Generic_Simple_List;
