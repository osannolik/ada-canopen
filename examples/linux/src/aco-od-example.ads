with ACO.OD_Types.Entries;
with ACO.Generic_Entry_Types;

package ACO.OD.Example is
   --  Shall be generated based on an EDS file

   type Dictionary_Data is new Object_Data_Base with private;

   use ACO.OD_Types.Entries;

   --  0x1008 Manufacturer Device Name VAR

   Device_Name_Str : constant String := "A device name";

   subtype Device_Name_String is Visible_String (Device_Name_Str'Range);

   package Device_Name_Pack is new ACO.Generic_Entry_Types (Device_Name_String);

   type Device_Name_Entry is new Device_Name_Pack.Entry_Type with null record;

private

   use type ACO.OD_Types.Object_Subindex;




   --  0x1000 Device Type VAR

   Device_Type_Var : aliased Entry_U32 := Create (RO, 16#00000000#);

   Device_Type_Data : aliased Entry_Array :=
      (0 => Device_Type_Var'Access);

   Device_Type : aliased Object_Base (Device_Type_Data'Access);

   --  0x1001 Error Register VAR

   Error_Register_Var : aliased Entry_U8 := Create (RO, 16#00#);

   Error_Register_Data : aliased Entry_Array :=
      (0 => Error_Register_Var'Access);

   Error_Register : aliased Object_Base (Error_Register_Data'Access);

   --  0x1003 Pre-defined Error Field ARRAY

   Predef_Err_Field_Nof : aliased Entry_U8 := Create (RW, 16#00#);

   Predef_Err_Field_1 : aliased Entry_U32 := Create (RO, 16#00000000#);

   Predef_Err_Field_Data : aliased Entry_Array :=
      (0 => Predef_Err_Field_Nof'Access,
       1 => Predef_Err_Field_1'Access);

   Predef_Err_Field : aliased Object_Base (Predef_Err_Field_Data'Access);

   --  0x1005 Sync COB-ID VAR

   Sync_COB_ID_Var : aliased Entry_U32 := Create (RW, 16#00000080#);

   Sync_COB_ID_Data : aliased Entry_Array :=
      (0 => Sync_COB_ID_Var'Access);

   Sync_COB_ID : aliased Object_Base (Sync_COB_ID_Data'Access);

   --  0x1006 Communication Cycle Period VAR

   Comm_Cycle_Per_Var : aliased Entry_U32 := Create (RW, 10_000);

   Comm_Cycle_Per_Data : aliased Entry_Array :=
      (0 => Comm_Cycle_Per_Var'Access);

   Comm_Cycle_Per : aliased Object_Base (Comm_Cycle_Per_Data'Access);

   --  0x1007 Synchronous Window Length VAR

   Sync_Win_Length_Var : aliased Entry_U32 := Create (RW, 16#00000000#);

   Sync_Win_Length_Data : aliased Entry_Array :=
      (0 => Sync_Win_Length_Var'Access);

   Sync_Win_Length : aliased Object_Base (Sync_Win_Length_Data'Access);

   --  0x1008 Manufacturer Device Name VAR

   Device_Name_Var : aliased Device_Name_Entry :=
      Create (RO, Device_Name_Str);

   Device_Name_Data : aliased Entry_Array :=
      (0 => Device_Name_Var'Access);

   Device_Name : aliased Object_Base (Device_Name_Data'Access);

   --  0x1016 Consumer Heartbeat Time ARRAY

   Consumer_Hbt_Nof : aliased Entry_U8 := Create (RO, 16#01#);

   Consumer_Hbt_1 : aliased Entry_U32 := Create (RW, 16#00040010#);

   Consumer_Hbt_Data : aliased Entry_Array :=
      (0 => Consumer_Hbt_Nof'Access,
       1 => Consumer_Hbt_1'Access);

   Consumer_Hbt : aliased Object_Base (Consumer_Hbt_Data'Access);

   --  0x1017 Producer Heartbeat Time

   Producer_Hbt_Var : aliased Entry_U16 := Create (RW, 500);

   Producer_Hbt_Data : aliased Entry_Array :=
      (0 => Producer_Hbt_Var'Access);

   Producer_Hbt : aliased Object_Base (Producer_Hbt_Data'Access);

   --  0x1019 Synchronous Counter Overflow Value VAR

   Sync_Counter_Overflow_Var : aliased Entry_U8 := Create (RW, 16);

   Sync_Counter_Overflow_Data : aliased Entry_Array :=
      (0 => Sync_Counter_Overflow_Var'Access);

   Sync_Counter_Overflow : aliased Object_Base (Sync_Counter_Overflow_Data'Access);



   --  Communication Profile Data

   Com_Profile : aliased Profile_Objects :=
      (0 => Device_Type'Access,
       1 => Error_Register'Access,
       2 => Predef_Err_Field'Access,
       3 => Sync_COB_ID'Access,
       4 => Comm_Cycle_Per'Access,
       5 => Sync_Win_Length'Access,
       6 => Device_Name'Access,
       7 => Consumer_Hbt'Access,
       8 => Producer_Hbt'Access,
       9 => Sync_Counter_Overflow'Access);


   function Index_Map (This : Dictionary_Data; Index : Object_Index)
                       return Index_Type
   is (case Index is
          when 16#1000# => 0,
          when 16#1001# => 1,
          when 16#1003# => 2,
          when 16#1005# => 3,
          when 16#1006# => 4,
          when 16#1007# => 5,
          when 16#1008# => 6,
          when 16#1016# => 7,
          when 16#1017# => 8,
          when 16#1019# => 9,
          when others   => No_Index);


   function Objects (This : Dictionary_Data) return Profile_Objects_Ref is
      (Com_Profile'Access);

   type Dictionary_Data is new Object_Data_Base with null record;

end ACO.OD.Example;
