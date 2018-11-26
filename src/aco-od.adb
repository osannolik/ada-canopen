with Ada.Tags;
with ACO.OD_Types.Entries;
with Interfaces;

package body ACO.OD is

   use ACO.OD_Types.Entries;
   use Interfaces;

   function Object
      (This  : Object_Dictionary'Class;
       Index : Object_Index)
       return Object_Ref
   is
      (This.Objects (This.Index_Map (Index)));

   function Object_Exist
      (This  : Object_Dictionary'Class;
       Index : Object_Index) return Boolean
   is (This.Index_Map (Index) /= No_Index);

   function Entry_Exist
      (This     : Object_Dictionary'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Boolean
   is
      Arr_Idx : constant Index_Type := This.Index_Map (Index);
   begin
      return Arr_Idx /= No_Index and then
         Subindex in This.Objects (Arr_Idx).Entries'Range;
   end Entry_Exist;

   function Is_Entry_Compatible
      (This     : Object_Dictionary;
       An_Entry : Entry_Base'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex)
       return Boolean
   is
      use type Ada.Tags.Tag;

      OD_Entry : constant Entry_Ref := This.Object (Index).Entries (Subindex);
   begin
      return OD_Entry'Tag = An_Entry'Tag;
   end Is_Entry_Compatible;

   function Is_Entry_Writable
      (This  : Object_Dictionary;
       Index : Entry_Index)
       return Boolean
   is
   begin
      if This.Entry_Exist (Index.Object, Index.Sub) then
         return This.Object (Index.Object).Entries (Index.Sub).Is_Writable;
      else
         return False;
      end if;
   end Is_Entry_Writable;

   function Is_Entry_Readable
      (This  : Object_Dictionary;
       Index : Entry_Index)
       return Boolean
   is
   begin
      if This.Entry_Exist (Index.Object, Index.Sub) then
         return This.Object (Index.Object).Entries (Index.Sub).Is_Readable;
      else
         return False;
      end if;
   end Is_Entry_Readable;

   function Get_Entry
      (This     : Object_Dictionary;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Base'Class
   is
      (This.Object (Index).Entries (Subindex).all);

   procedure Set_Entry
      (This      : in out Object_Dictionary;
       New_Entry : in     Entry_Base'Class;
       Index     : in     Object_Index;
       Subindex  : in     Object_Subindex;
       Silently  : in     Boolean := False)
   is
   begin
      This.Object (Index).Entries (Subindex).all := New_Entry;
      if not Silently then
         This.Events.Entry_Updated.Put ((Index, Subindex));
      end if;
   end Set_Entry;

   procedure Set_Node_State
      (This       : in out Object_Dictionary;
       Node_State : in     ACO.States.State)
   is
      Prev : ACO.States.State;
   begin
      Prev            := This.Node_State;
      This.Node_State := Node_State;
      This.Events.Node_State_Modified.Put ((Previous => Prev, Current => Node_State));
   end Set_Node_State;

   function Get_Node_State (This : Object_Dictionary) return ACO.States.State is
      (This.Node_State);

   function Get_Hbt_Node_Id (Reg : U32) return ACO.Messages.Node_Nr is
      (ACO.Messages.Node_Nr (Shift_Right (Reg, 16) and 16#FF#));

   function Get_Hbt_Slave_Subindex
      (This    : Object_Dictionary;
       Node_Id : ACO.Messages.Node_Nr)
       return Object_Subindex
   is
      Object : constant Object_Ref := This.Object (Heartbeat_Consumer_Index);
   begin
      for I in 1 .. Object.Entries'Last loop
         declare
            E_Ref : constant Entry_Ref := Object.Entries (I);
            Reg : constant U32 := Entry_U32 (E_Ref.all).Read;
            use type ACO.Messages.Node_Nr;
         begin
            if Get_Hbt_Node_Id (Reg) = Node_Id then
               return Object_Subindex (I);
            end if;
         end;
      end loop;
      return 0; --  First entry is always max subindex, not slave data
   end Get_Hbt_Slave_Subindex;

   procedure Set_Heartbeat_Consumer_Period
      (This    : in out Object_Dictionary;
       Node_Id : in     ACO.Messages.Node_Nr;
       Period  : in     Natural)
   is
      function Set_Hbt_Period (Reg : U32; Period : Natural) return U32 is
         ((Reg and 16#FFFF0000#) or (U32 (Period) and 16#FFFF#));

      Subindex : Object_Subindex;
      E        : Entry_U32;
   begin
      Subindex := This.Get_Hbt_Slave_Subindex (Node_Id);

      if This.Entry_Exist (Heartbeat_Consumer_Index, Subindex) then
         E := Entry_U32 (This.Get_Entry (Heartbeat_Consumer_Index, Subindex));
         E.Write (Set_Hbt_Period (E.Read, Period));
         This.Set_Entry (E, Heartbeat_Consumer_Index, Subindex);
         This.Events.Entry_Updated.Put ((Heartbeat_Consumer_Index, Subindex));
      end if;
   end Set_Heartbeat_Consumer_Period;

   function Get_Heartbeat_Consumer_Period
      (This    : Object_Dictionary;
       Node_Id : ACO.Messages.Node_Nr)
       return Natural
   is
      function Get_Hbt_Period (Reg : U32) return Natural is
         (Natural (Reg and 16#FFFF#));

      Subindex : Object_Subindex;
      Reg      : U32;
   begin
      Subindex := This.Get_Hbt_Slave_Subindex (Node_Id);

      if This.Entry_Exist (Heartbeat_Consumer_Index, Subindex) then
         Reg := Entry_U32 (This.Get_Entry (Heartbeat_Consumer_Index, Subindex)).Read;
         return Get_Hbt_Period (Reg);
      else
         return 0;
      end if;
   end Get_Heartbeat_Consumer_Period;

   procedure Set_Heartbeat_Producer_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
   is
      E : Entry_U16;
   begin
      E.Write (U16 (Period));
      This.Set_Entry (E, Heartbeat_Producer_Index, 0);
   end Set_Heartbeat_Producer_Period;

   function Get_Heartbeat_Producer_Period
      (This : Object_Dictionary)
       return Natural
   is
      Period : U16;
   begin
      Period := Entry_U16 (This.Get_Entry (Heartbeat_Producer_Index, 0)).Read;
      return Natural (Period);
   end Get_Heartbeat_Producer_Period;

   procedure Set_Communication_Cycle_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
   is
      E : Entry_U32;
   begin
      E.Write (U32 (Period));
      This.Set_Entry (E, Comm_Cycle_Period_Index, 0);
   end Set_Communication_Cycle_Period;

   function Get_Communication_Cycle_Period
      (This : Object_Dictionary)
       return Natural
   is
      Period : U32;
   begin
      Period := Entry_U32 (This.Get_Entry (Comm_Cycle_Period_Index, 0)).Read;
      return Natural (Period);
   end Get_Communication_Cycle_Period;

   procedure Set_Sync_Counter_Overflow
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
   is
      E : Entry_U8;
   begin
      E.Write (U8 (Period));
      This.Set_Entry (E, Sync_Counter_Overflow_Index, 0);
   end Set_Sync_Counter_Overflow;

   function Get_Sync_Counter_Overflow
      (This : Object_Dictionary)
       return Natural
   is
      Period : U8;
   begin
      Period := Entry_U8 (This.Get_Entry (Sync_Counter_Overflow_Index, 0)).Read;
      return Natural (Period);
   end Get_Sync_Counter_Overflow;

   function Get_SDO_Server_Parameters
      (This : Object_Dictionary)
       return ACO.SDO_Sessions.SDO_Parameter_Array
   is
      J : Object_Index := SDO_Server_Base_Index;
   begin
      while This.Object_Exist (J) loop
         J := J + 1;
      end loop;

      declare
         use ACO.Messages, ACO.SDO_Sessions;
         Result : SDO_Parameter_Array (0 .. Natural (J - SDO_Server_Base_Index) - 1);
      begin
         J := SDO_Server_Base_Index;

         for I in Result'Range loop
            Result (I) :=
               (CAN_Id_C2S =>
                   Id_Type (Entry_U32 (This.Get_Entry (J, 1)).Read and 16#7FF#),
                CAN_Id_S2C =>
                   Id_Type (Entry_U32 (This.Get_Entry (J, 2)).Read and 16#7FF#),
                Node =>
                   Node_Nr (Entry_U8 (This.Get_Entry (J, 3)).Read and 16#7F#));
            J := J + 1;
         end loop;

         return Result;
      end;
   end Get_SDO_Server_Parameters;

   function Get_SDO_Client_Parameters
      (This : Object_Dictionary)
       return ACO.SDO_Sessions.SDO_Parameter_Array
   is
      J : Object_Index := SDO_Client_Base_Index;
   begin
      while This.Object_Exist (J) loop
         J := J + 1;
      end loop;

      declare
         use ACO.Messages, ACO.SDO_Sessions;
         Result : SDO_Parameter_Array (0 .. Natural (J - SDO_Client_Base_Index) - 1);
      begin
         J := SDO_Client_Base_Index;

         for I in Result'Range loop
            Result (I) :=
               (CAN_Id_C2S =>
                   Id_Type (Entry_U32 (This.Get_Entry (J, 1)).Read and 16#7FF#),
                CAN_Id_S2C =>
                   Id_Type (Entry_U32 (This.Get_Entry (J, 2)).Read and 16#7FF#),
                Node =>
                   Node_Nr (Entry_U8 (This.Get_Entry (J, 3)).Read and 16#7F#));
            J := J + 1;
         end loop;

         return Result;
      end;
   end Get_SDO_Client_Parameters;

end ACO.OD;
