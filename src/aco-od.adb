package body ACO.OD is

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

   function Get_Entry
      (This     : Object_Dictionary'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Base'Class
   is
      (This.Data.Objects (This.Data.Index_Map (Index)).Entries (Subindex).all);


--     protected type Barrier_Type (This : not null access Object_Dict'Class) is
--
--        function Get_Entry
--
--        private
--
--     end Barrier_Type;



   procedure Set_Node_State
     (This       : in out Object_Dictionary;
      Node_State : in     ACO.States.State)
   is
      Tmp : constant ACO.States.State_Transition :=
         (Previous => This.Node_State,
          Current  => Node_State);
   begin
      This.Node_State := Node_State;
      This.Events.Node_State_Change.Update (Tmp);
   end Set_Node_State;

   function Get_Node_State (This : Object_Dictionary) return ACO.States.State is
     (This.Node_State);

   function Get_Heartbeat_Producer_Period (This : Object_Dictionary) return Natural is
      (This.Heartbeat_Producer_Period);

   function Get_Communication_Cycle_Period (This : Object_Dictionary) return Natural is
      (This.Communication_Cycle_Period);

   function Get_Sync_Counter_Overflow (This : Object_Dictionary) return Sync_Counter is
      (This.Sync_Counter_Overflow_Value);

   function Get_Heartbeat_Consumer_Period
     (This    : Object_Dictionary;
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
