package body ACO.Utils.Generic_Simple_List is

   function In_List
      (This    : List_Type;
       Element : Element_Type)
       return Boolean
   is
      (for some E of This.Items => E.Is_Used and then E.Element = Element);

   function Is_Full
      (This : List_Type)
       return Boolean
   is
      (for all E of This.Items => E.Is_Used);

   procedure Add
      (This    : in out List_Type;
       Element : in     Element_Type)
   is
   begin
      for E of This.Items loop
         if not E.Is_Used then
            E := (True, Element);
         end if;
      end loop;
   end Add;

   procedure Remove
      (This    : in out List_Type;
       Element : in     Element_Type)
   is
   begin
      for E of This.Items loop
         if E.Is_Used and then E.Element = Element then
            E.Is_Used := False;
         end if;
      end loop;
   end Remove;

   procedure Remove
      (This  : in out List_Type;
       Index : in     Natural)
   is
   begin
      This.Items (Index).Is_Used := False;
   end Remove;

   function Get
      (This  : List_Type;
       Index : Natural)
       return Element_Type
   is
      (This.Items (Index).Element);

   function Length
      (This : List_Type)
       return Natural
   is
      L : Natural := 0;
   begin
      for E of This.Items loop
         if E.Is_Used then
            L := L + 1;
         end if;
      end loop;

      return L;
   end Length;

   function Index
      (This    : List_Type;
       Element : Element_Type)
       return Natural
   is
   begin
      for I in This.Items'Range loop
         if This.Items (I).Is_Used and then
            This.Items (I).Element = Element
         then
            return I;
         end if;
      end loop;

      return No_Index;
   end Index;

end ACO.Utils.Generic_Simple_List;
