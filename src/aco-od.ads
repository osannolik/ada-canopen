with ACO.States;
with ACO.Messages;
with ACO.Events;
with ACO.OD_Types;

package ACO.OD is

   pragma Preelaborate;

   use ACO.OD_Types;

   subtype Comm_Profile_Index is Object_Index range 16#1000# .. 16#1FFF#;

   type Object_Dict is abstract tagged limited private;

   function Objects
      (This : Object_Dict) return Profile_Objects_Ref is abstract;

   function Index_Map
      (This : Object_Dict; Index : Object_Index) return Index_Type is abstract;

   function Object_Exist
      (This  : Object_Dict'Class;
       Index : Object_Index)
       return Boolean;

   function Entry_Exist
      (This     : Object_Dict'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex)
       return Boolean;

   function Get_Object_Ref
      (This  : Object_Dict'Class;
       Index : Object_Index) return Object_Ref;

   function Get_Object
      (This  : Object_Dict'Class;
       Index : Object_Index) return Object_Base'Class
      with Pre => This.Object_Exist (Index);

   function Get_Entry_Ref
      (This     : Object_Dict'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Ref
      with Pre => This.Object_Exist (Index);

   function Get_Entry
      (This     : Object_Dict'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Base'Class
      with Pre => This.Entry_Exist (Index, Subindex);


   Max_Nof_Heartbeat_Slaves : constant := 8;

   type State_Array is array (Positive range <>) of ACO.States.State;

   subtype Sync_Counter is Natural range 0 .. 240;



   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State);

   function Get_Node_State (This : Object_Dict) return ACO.States.State;

   function Get_Heartbeat_Producer_Period (This : Object_Dict) return Natural;

   function Get_Communication_Cycle_Period (This : Object_Dict) return Natural;

   function Get_Sync_Counter_Overflow (This : Object_Dict) return Sync_Counter;

   function Get_Heartbeat_Consumer_Period
     (This    : Object_Dict;
      Node_Id : ACO.Messages.Node_Nr)
      return Natural;

private

   type Object_Dict is abstract tagged limited record
      Events : ACO.Events.Event_Manager;
      Node_State : ACO.States.State := ACO.States.Unknown_State;
      Communication_Cycle_Period : Natural := 10_000; --  Multiples of 100us
      Sync_Counter_Overflow_Value : Sync_Counter := 16;
      Heartbeat_Producer_Period : Natural := 500;
      Slave_States : State_Array (1 .. Max_Nof_Heartbeat_Slaves) :=
         (others => ACO.States.Unknown_State);
   end record;

end ACO.OD;
