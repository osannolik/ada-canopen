with Ada.Tags;
with ACO.OD_Types.Entries;
with Interfaces;

package body ACO.OD is

   use ACO.OD_Types.Entries;
   use Interfaces;

   function Object_Exist
      (This  : Object_Dictionary'Class;
       Index : Object_Index) return Boolean
   is (This.Data.Index_Map (Index) /= No_Index);

   function Entry_Exist
      (This     : Object_Dictionary'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Boolean
   is
      Arr_Idx : constant Index_Type := This.Data.Index_Map (Index);
   begin
      return Arr_Idx /= No_Index and then
         Subindex in This.Data.Objects (Arr_Idx).Entries'Range;
   end Entry_Exist;

   function Is_Entry_Compatible
      (This     : Object_Dictionary'Class;
       An_Entry : Entry_Base'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex)
       return Boolean
   is
      use type Ada.Tags.Tag;

      OD_Entry : constant Entry_Ref :=
         This.Data.Objects (This.Data.Index_Map (Index)).Entries (Subindex);
   begin
      return OD_Entry'Tag = An_Entry'Tag;
   end Is_Entry_Compatible;

   function Get_Entry
      (This     : Object_Dictionary'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Base'Class
   is
      (This.Protected_Data.Get_Entry (Index, Subindex));

   procedure Set_Entry
      (This      : in out Object_Dictionary'Class;
       New_Entry : in     Entry_Base'Class;
       Index     : in     Object_Index;
       Subindex  : in     Object_Subindex;
       Silently  : in     Boolean := False)
   is
   begin
      This.Protected_Data.Set_Entry (New_Entry, Index, Subindex);
      if not Silently then
         This.Events.Entry_Updated.Update ((Index, Subindex));
      end if;
   end Set_Entry;

   procedure Set_Node_State
      (This       : in out Object_Dictionary;
       Node_State : in     ACO.States.State)
   is
      Prev : ACO.States.State;
   begin
      This.Protected_Data.Set_Node_State (Node_State, Prev);
      This.Events.Node_State_Modified.Put ((Previous => Prev, Current => Node_State));
   end Set_Node_State;

   function Get_Node_State (This : Object_Dictionary) return ACO.States.State is
      (This.Protected_Data.Get_Node_State);

   procedure Set_Heartbeat_Consumer_Period
      (This    : in out Object_Dictionary;
       Node_Id : in     ACO.Messages.Node_Nr;
       Period  : in     Natural)
   is
      At_Subindex : Object_Subindex;
   begin
      if not This.Object_Exist (Heartbeat_Consumer_Index) then
         return;
      end if;

      This.Protected_Data.Set_Heartbeat_Consumer_Period (Node_Id, Period, At_Subindex);
      if At_Subindex > 0 then
         This.Events.Entry_Updated.Update ((Heartbeat_Consumer_Index, At_Subindex));
      end if;
   end Set_Heartbeat_Consumer_Period;

   function Get_Heartbeat_Consumer_Period
      (This    : Object_Dictionary;
       Node_Id : ACO.Messages.Node_Nr)
       return Natural
   is
      Period : Natural := 0;
   begin
      if This.Object_Exist (Heartbeat_Consumer_Index) then
         Period := This.Protected_Data.Get_Heartbeat_Consumer_Period (Node_Id);
      end if;
      return Period;
   end Get_Heartbeat_Consumer_Period;

   procedure Set_Heartbeat_Producer_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
   is
      E : Entry_U16;
   begin
      if This.Object_Exist (Heartbeat_Producer_Index) then
         E.Write (U16 (Period));
         This.Set_Entry (E, Heartbeat_Producer_Index, 0);
      end if;
   end Set_Heartbeat_Producer_Period;

   function Get_Heartbeat_Producer_Period
      (This : Object_Dictionary)
       return Natural
   is
      Period : U16 := 0;
   begin
      if This.Object_Exist (Heartbeat_Producer_Index) then
         Period := Entry_U16
            (This.Protected_Data.Get_Entry (Heartbeat_Producer_Index, 0)).Read;
      end if;
      return Natural (Period);
   end Get_Heartbeat_Producer_Period;

   procedure Set_Communication_Cycle_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
   is
      E : Entry_U32;
   begin
      if This.Object_Exist (Comm_Cycle_Period_Index) then
         E.Write (U32 (Period));
         This.Set_Entry (E, Comm_Cycle_Period_Index, 0);
      end if;
   end Set_Communication_Cycle_Period;

   function Get_Communication_Cycle_Period
      (This : Object_Dictionary)
       return Natural
   is
      Period : U32 := 0;
   begin
      if This.Object_Exist (Comm_Cycle_Period_Index) then
         Period := Entry_U32
            (This.Protected_Data.Get_Entry (Comm_Cycle_Period_Index, 0)).Read;
      end if;
      return Natural (Period);
   end Get_Communication_Cycle_Period;

   procedure Set_Sync_Counter_Overflow
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
   is
      E : Entry_U8;
   begin
      if This.Object_Exist (Sync_Counter_Overflow_Index) then
         E.Write (U8 (Period));
         This.Set_Entry (E, Sync_Counter_Overflow_Index, 0);
      end if;
   end Set_Sync_Counter_Overflow;

   function Get_Sync_Counter_Overflow
      (This : Object_Dictionary)
       return Natural
   is
      Period : U8 := 0;
   begin
      if This.Object_Exist (Sync_Counter_Overflow_Index) then
         Period := Entry_U8
            (This.Protected_Data.Get_Entry (Sync_Counter_Overflow_Index, 0)).Read;
      end if;
      return Natural (Period);
   end Get_Sync_Counter_Overflow;

   function Get_SDO_Server_Rx_CAN_Ids
      (This : Object_Dictionary)
       return ACO.Messages.Id_Array
   is
      Nothing : ACO.Messages.Id_Array (1 .. 0);
   begin
      if This.Object_Exist (SDO_Server_Base_Index) then
         return This.Protected_Data.Get_SDO_Server_Rx_CAN_Ids;
      end if;
      return Nothing;
   end Get_SDO_Server_Rx_CAN_Ids;

   function Get_SDO_Client_Rx_CAN_Ids
      (This : Object_Dictionary)
       return ACO.Messages.Id_Array
   is
      Nothing : ACO.Messages.Id_Array (1 .. 0);
   begin
      if This.Object_Exist (SDO_Client_Base_Index) then
         return This.Protected_Data.Get_SDO_Client_Rx_CAN_Ids;
      end if;
      return Nothing;
   end Get_SDO_Client_Rx_CAN_Ids;


   protected body Barrier_Type is

      function Get_Entry
         (Index    : Object_Index;
          Subindex : Object_Subindex)
          return Entry_Base'Class
      is (Data.Objects (Data.Index_Map (Index)).Entries (Subindex).all);

      procedure Set_Entry
         (New_Entry : in Entry_Base'Class;
          Index     : in Object_Index;
          Subindex  : in Object_Subindex)
      is
      begin
         Data.Objects (Data.Index_Map (Index)).Entries (Subindex).all := New_Entry;
      end Set_Entry;

      function Get_Node_State return ACO.States.State is
         (Node_State);

      procedure Set_Node_State
         (New_State  : in     ACO.States.State;
          Prev_State :    out ACO.States.State)
      is
      begin
         Prev_State := Node_State;
         Node_State := New_State;
      end Set_Node_State;

      function Get_Hbt_Node_Id (Reg : U32) return ACO.Messages.Node_Nr is
         (ACO.Messages.Node_Nr (Shift_Right (Reg, 16) and 16#FF#));

      function Get_Hbt_Slave_Subindex
         (Node_Id : ACO.Messages.Node_Nr)
          return Object_Subindex
      is
         Object_Ref : constant access Object_Base :=
            Data.Objects (Data.Index_Map (Heartbeat_Consumer_Index));
      begin
         for I in 1 .. Object_Ref.Entries'Last loop
            declare
               E_Ref : constant Entry_Ref := Object_Ref.Entries (I);
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
         (Node_Id  : in     ACO.Messages.Node_Nr;
          Period   : in     Natural;
          Subindex :    out Object_Subindex)
      is
         function Set_Hbt_Period (Reg : U32; Period : Natural) return U32 is
            ((Reg and 16#FFFF0000#) or (U32 (Period) and 16#FFFF#));

         E : Entry_U32;
      begin
         Subindex := Get_Hbt_Slave_Subindex (Node_Id);
         if Subindex > 0 then
            E := Entry_U32 (Get_Entry (Heartbeat_Consumer_Index, Subindex));
            E.Write (Set_Hbt_Period (E.Read, Period));
            Set_Entry (E, Heartbeat_Consumer_Index, Subindex);
         end if;
      end Set_Heartbeat_Consumer_Period;

      function Get_Heartbeat_Consumer_Period
         (Node_Id : ACO.Messages.Node_Nr) return Natural
      is
         function Get_Hbt_Period (Reg : U32) return Natural is
            (Natural (Reg and 16#FFFF#));

         Subindex : constant Object_Subindex := Get_Hbt_Slave_Subindex (Node_Id);
         Reg : U32;
      begin
         if Subindex > 0 then
            Reg := Entry_U32 (Get_Entry (Heartbeat_Consumer_Index, Subindex)).Read;
            return Get_Hbt_Period (Reg);
         else
            return 0;
         end if;
      end Get_Heartbeat_Consumer_Period;

      function Get_SDO_Server_Rx_CAN_Ids return ACO.Messages.Id_Array
      is
         J : Object_Index := SDO_Server_Base_Index;
      begin
         while Data.Index_Map (J) /= No_Index loop
            J := J + 1;
         end loop;

         declare
            use ACO.Messages;
            Result : Id_Array (0 .. Natural (J - SDO_Server_Base_Index) - 1);
         begin
            J := SDO_Server_Base_Index;

            for I in Result'Range loop
               Result (I) := Id_Type (Entry_U32 (Get_Entry (J, 1)).Read and 16#7FF#);
               J := J + 1;
            end loop;

            return Result;
         end;
      end Get_SDO_Server_Rx_CAN_Ids;

      function Get_SDO_Client_Rx_CAN_Ids return ACO.Messages.Id_Array
      is
         J : Object_Index := SDO_Client_Base_Index;
      begin
         while Data.Index_Map (J) /= No_Index loop
            J := J + 1;
         end loop;

         declare
            use ACO.Messages;
            Result : Id_Array (0 .. Natural (J - SDO_Client_Base_Index) - 1);
         begin
            J := SDO_Client_Base_Index;

            for I in Result'Range loop
               Result (I) := Id_Type (Entry_U32 (Get_Entry (J, 2)).Read and 16#7FF#);
               J := J + 1;
            end loop;

            return Result;
         end;
      end Get_SDO_Client_Rx_CAN_Ids;

   end Barrier_Type;

end ACO.OD;
