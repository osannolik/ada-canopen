package body ACO.OD is

   function Object_Exist
      (This  : Object_Dict'Class;
       Index : Object_Index) return Boolean
   is (This.Index_Map (Index) /= No_Index);

   function Entry_Exist
      (This     : Object_Dict'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Boolean
   is
      Arr_Idx : constant Index_Type := This.Index_Map (Index);
   begin
      return Arr_Idx /= No_Index and then
         Subindex in This.Objects (Arr_Idx).Entries'Range;
   end Entry_Exist;

   function Get_Object_Ref
      (This  : Object_Dict'Class;
       Index : Object_Index) return Object_Ref
   is
      Arr_Idx : constant Index_Type := This.Index_Map (Index);
   begin
      if Arr_Idx = No_Index then
         return No_Object;
      else
         return This.Objects (Arr_Idx);
      end if;
   end Get_Object_Ref;

   function Get_Object
      (This  : Object_Dict'Class;
       Index : Object_Index) return Object_Base'Class
   is
      (This.Objects (This.Index_Map (Index)).all);

   function Get_Entry_Ref
      (This     : Object_Dict'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Ref
   is
      Arr_Idx : constant Index_Type := This.Index_Map (Index);
      Entry_Arr : constant access Entry_Array := This.Objects (Arr_Idx).Entries;
   begin
      if Subindex in Entry_Arr'Range then
         return Entry_Arr (Subindex);
      else
         return No_Entry;
      end if;
   end Get_Entry_Ref;

   function Get_Entry
      (This     : Object_Dict'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Base'Class
   is
      (This.Objects (This.Index_Map (Index)).Entries (Subindex).all);

   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State)
   is
      Tmp : constant ACO.States.State_Transition :=
         (Previous => This.Node_State,
          Current  => Node_State);
   begin
      This.Node_State := Node_State;
      This.Events.Node_State_Change.Update (Tmp);
   end Set_Node_State;

   function Get_Node_State (This : Object_Dict) return ACO.States.State is
     (This.Node_State);

   function Get_Heartbeat_Producer_Period (This : Object_Dict) return Natural is
      (This.Heartbeat_Producer_Period);

   function Get_Communication_Cycle_Period (This : Object_Dict) return Natural is
      (This.Communication_Cycle_Period);

   function Get_Sync_Counter_Overflow (This : Object_Dict) return Sync_Counter is
      (This.Sync_Counter_Overflow_Value);

   function Get_Heartbeat_Consumer_Period
     (This    : Object_Dict;
      Node_Id : ACO.Messages.Node_Nr)
      return Natural
   is
      pragma Unreferenced (This);

      use ACO.Messages;
   begin
      --  Temporary for test
      if Node_Id = 2 then
         return 0;
      else
         return 600;
      end if;
   end Get_Heartbeat_Consumer_Period;

end ACO.OD;
