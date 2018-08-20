with ACO.States;
with ACO.Messages;
with ACO.Events;
with ACO.OD_Types;

package ACO.OD is

   pragma Preelaborate;

   use ACO.OD_Types;

   subtype Comm_Profile_Index is Object_Index range 16#1000# .. 16#1FFF#;

   type Object_Dictionary_Base is abstract tagged limited record
      Events : ACO.Events.Event_Manager;
   end record;

   type Object_Data_Base is abstract tagged limited private;

   type Object_Dictionary (Data : not null access Object_Data_Base'Class) is
      new Object_Dictionary_Base with private;


   function Object_Exist
      (This  : Object_Dictionary'Class;
       Index : Object_Index)
       return Boolean;

   function Entry_Exist
      (This     : Object_Dictionary'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex)
       return Boolean;

   function Is_Entry_Compatible
      (This     : Object_Dictionary'Class;
       An_Entry : Entry_Base'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex)
       return Boolean;

   function Get_Entry
      (This     : Object_Dictionary'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Base'Class
      with Pre => This.Entry_Exist (Index, Subindex);

   procedure Set_Entry
      (This      : in out Object_Dictionary'Class;
       New_Entry : in     Entry_Base'Class;
       Index     : in     Object_Index;
       Subindex  : in     Object_Subindex;
       Silently  : in     Boolean := False)
      with Pre => This.Entry_Exist (Index, Subindex) and then
                  This.Is_Entry_Compatible (New_Entry, Index, Subindex);

   procedure Set_Node_State
      (This       : in out Object_Dictionary;
       Node_State : in     ACO.States.State);

   function Get_Node_State (This : Object_Dictionary) return ACO.States.State;





   Max_Nof_Heartbeat_Slaves : constant := 8;

   type State_Array is array (Positive range <>) of ACO.States.State;

   subtype Sync_Counter is Natural range 0 .. 240;







   function Get_Heartbeat_Producer_Period (This : Object_Dictionary) return Natural;

   function Get_Communication_Cycle_Period (This : Object_Dictionary) return Natural;

   function Get_Sync_Counter_Overflow (This : Object_Dictionary) return Sync_Counter;

   function Get_Heartbeat_Consumer_Period
     (This    : Object_Dictionary;
      Node_Id : ACO.Messages.Node_Nr)
      return Natural;

private

   protected type Barrier_Type (Data : not null access Object_Data_Base'Class)
   is

      function Get_Entry
         (Index    : Object_Index;
          Subindex : Object_Subindex)
          return Entry_Base'Class;

      procedure Set_Entry
         (New_Entry : in Entry_Base'Class;
          Index     : in Object_Index;
          Subindex  : in Object_Subindex);

      function Get_Node_State return ACO.States.State;

      procedure Set_Node_State
         (New_State  : in     ACO.States.State;
          Prev_State :    out ACO.States.State);

   private
      Node_State : ACO.States.State := ACO.States.Unknown_State;
   end Barrier_Type;


   type Object_Data_Base is abstract tagged limited null record;

   function Objects
      (This : Object_Data_Base) return Profile_Objects_Ref is (null);

   function Index_Map
      (This : Object_Data_Base; Index : Object_Index) return Index_Type is (No_Index);

   type Object_Dictionary (Data : not null access Object_Data_Base'Class) is
      new Object_Dictionary_Base with
   record
      Protected_Data : Barrier_Type (Data);
      Communication_Cycle_Period : Natural := 10_000; --  Multiples of 100us
      Sync_Counter_Overflow_Value : Sync_Counter := 16;
      Heartbeat_Producer_Period : Natural := 500;
      Slave_States : State_Array (1 .. Max_Nof_Heartbeat_Slaves) :=
         (others => ACO.States.Unknown_State);
   end record;

end ACO.OD;
