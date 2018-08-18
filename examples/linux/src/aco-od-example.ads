with ACO.OD_Types.Entries;

package ACO.OD.Example is
   --  Shall be generated based on an EDS file

   type Example_Dict is new Object_Dict with private;

private



   use type ACO.OD_Types.Object_Subindex;

   use ACO.OD_Types.Entries;

   subtype Bulle_Subindex is Object_Subindex range 0 .. 2;

   Bulle_Highest_Subindex : aliased Entry_U8  := Create (RW, Bulle_Subindex'Last);
   Bulle_Russin           : aliased Entry_F32 := Create (WO, 13.0);

   Bulle_Data : aliased Entry_Array :=
      (Bulle_Subindex'First     => Bulle_Highest_Subindex'Access,
       Bulle_Subindex'First + 1 => Bulle_Russin'Access,
       Bulle_Subindex'Last      => No_Entry);

   Bulle : aliased Object_Base (Bulle_Data'Access);

   Com_Profile : aliased Profile_Objects :=
      (0 => Bulle'Access);

   overriding
   function Index_Map (This : Example_Dict; Index : Object_Index)
                       return Index_Type
   is (case Index is
          when 16#1004# => 0,
          when others   => No_Index);

   overriding
   function Objects (This : Example_Dict) return Profile_Objects_Ref is
      (Com_Profile'Access);

   type Example_Dict is new Object_Dict with null record;

end ACO.OD.Example;
