with ACO.States;
with ACO.Messages;
with ACO.Events;

package ACO.OD is
   --  This package (or a child package of it) will eventually be generated
   --  TODO: Generalize such that reading and writing to it is possible using
   --        e.g. SDO and application. Also, generalize indication to stack.

   pragma Preelaborate;

   Max_Nof_Heartbeat_Slaves : constant := 8;

   type State_Array is array (Positive range <>) of ACO.States.State;

   subtype Sync_Counter is Natural range 0 .. 240;

   type Object_Dict is tagged limited record
      Events : ACO.Events.Event_Manager;
      Node_State : ACO.States.State := ACO.States.Unknown_State;
      Communication_Cycle_Period : Natural := 10_000; --  Multiples of 100us
      Sync_Counter_Overflow_Value : Sync_Counter := 16;
      Heartbeat_Producer_Period : Natural := 500;
      Slave_States : State_Array (1 .. Max_Nof_Heartbeat_Slaves) :=
         (others => ACO.States.Unknown_State);
   end record;

   type Object_Dict_Access is access all Object_Dict'Class;

   procedure Set_Node_State
     (This       : in out Object_Dict;
      Node_State : in     ACO.States.State);

   function Get_Node_State (This : Object_Dict) return ACO.States.State;

   function Get_Heartbeat_Producer_Period (This : Object_Dict) return Natural;

   function Get_Communication_Cycle_Period (This : Object_Dict) return Natural;

   procedure Set_Communication_Cycle_Period
     (This   : in out Object_Dict;
      Period : in     Natural);

   function Get_Sync_Counter_Overflow (This : Object_Dict) return Sync_Counter;

   procedure Set_Sync_Counter_Overflow
     (This           : in out Object_Dict;
      Overflow_Value : in     Sync_Counter);

   procedure Set_Heartbeat_Producer_Period
     (This   : in out Object_Dict;
      Period : in     Natural);

   function Get_Heartbeat_Consumer_Period
     (This    : Object_Dict;
      Node_Id : ACO.Messages.Node_Nr)
      return Natural;

end ACO.OD;
