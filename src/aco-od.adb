with Ada.Tags;
with ACO.OD_Types.Entries;
with Interfaces;

package body ACO.OD is

   use ACO.OD_Types.Entries;

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
      This.Events.Node_State_Change.Update ((Previous => Prev, Current => Node_State));
   end Set_Node_State;

   function Get_Node_State (This : Object_Dictionary) return ACO.States.State is
      (This.Protected_Data.Get_Node_State);

   function Get_Heartbeat_Consumer_Period
      (This    : Object_Dictionary;
       Node_Id : ACO.Messages.Node_Nr)
       return Natural
   is
   begin
      if This.Object_Exist (Heartbeat_Consumer_Index) then
         return This.Protected_Data.Get_Heartbeat_Consumer_Period (Node_Id);
      else
         return 0;
      end if;
   end Get_Heartbeat_Consumer_Period;

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

      function Get_Heartbeat_Consumer_Period
         (Node_Id : ACO.Messages.Node_Nr) return Natural
      is
         use Interfaces;

         Object_Ref : constant access Object_Base :=
            Data.Objects (Data.Index_Map (Heartbeat_Consumer_Index));

         function Get_Node_Id (Reg : U32) return ACO.Messages.Node_Nr is
            (ACO.Messages.Node_Nr (Shift_Right (Reg, 16) and 16#FF#));

         function Get_Period (Reg : U32) return Natural is
            (Natural (Reg and 16#FF#));
      begin
         for I in 1 .. Object_Ref.Entries'Last loop
            declare
               E : constant U32 := Entry_U32 (Object_Ref.Entries (I).all).Read;
               use type ACO.Messages.Node_Nr;
            begin
               if Get_Node_Id (E) = Node_Id then
                  return Get_Period (E);
               end if;
            end;
         end loop;

         return 0;
      end Get_Heartbeat_Consumer_Period;

   end Barrier_Type;




   function Get_Communication_Cycle_Period (This : Object_Dictionary) return Natural is
      (This.Communication_Cycle_Period);

   function Get_Sync_Counter_Overflow (This : Object_Dictionary) return Sync_Counter is
      (This.Sync_Counter_Overflow_Value);



end ACO.OD;
