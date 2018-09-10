with ACO.States;
with ACO.Messages;
with ACO.Events;
with ACO.OD_Types;

package ACO.OD is

   pragma Preelaborate;

   use ACO.OD_Types;

   subtype Comm_Profile_Index is Object_Index range 16#1000# .. 16#1FFF#;

   Comm_Cycle_Period_Index     : constant := 16#1006#;
   Heartbeat_Producer_Index    : constant := 16#1017#;
   Heartbeat_Consumer_Index    : constant := 16#1016#;
   Sync_Counter_Overflow_Index : constant := 16#1019#;
   SDO_Server_Base_Index       : constant := 16#1200#;
   SDO_Client_Base_Index       : constant := 16#1280#;

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

   function Is_Entry_Writable
      (This  : Object_Dictionary'Class;
       Index : Entry_Index)
       return Boolean;

   function Is_Entry_Readable
      (This  : Object_Dictionary'Class;
       Index : Entry_Index)
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

   function Get_Node_State
      (This : Object_Dictionary)
       return ACO.States.State;

   procedure Set_Heartbeat_Consumer_Period
      (This    : in out Object_Dictionary;
       Node_Id : in     ACO.Messages.Node_Nr;
       Period  : in     Natural);

   function Get_Heartbeat_Consumer_Period
      (This    : Object_Dictionary;
       Node_Id : ACO.Messages.Node_Nr)
       return Natural;

   procedure Set_Heartbeat_Producer_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural);

   function Get_Heartbeat_Producer_Period
      (This : Object_Dictionary)
       return Natural;

   procedure Set_Communication_Cycle_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural);

   function Get_Communication_Cycle_Period
      (This : Object_Dictionary)
       return Natural;

   procedure Set_Sync_Counter_Overflow
      (This    : in out Object_Dictionary;
       Period  : in     Natural);

   function Get_Sync_Counter_Overflow
      (This : Object_Dictionary)
       return Natural;

   function Get_SDO_Server_Rx_CAN_Ids
      (This : Object_Dictionary)
       return ACO.Messages.Id_Array;

   function Get_SDO_Client_Rx_CAN_Ids
      (This : Object_Dictionary)
       return ACO.Messages.Id_Array;

private

   type Object_Data_Base is abstract tagged limited null record;

   function Objects
      (This : Object_Data_Base)
       return Profile_Objects_Ref
   is
      (null);

   function Index_Map
      (This  : Object_Data_Base;
       Index : Object_Index)
       return Index_Type
   is
      (No_Index);

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

      procedure Set_Heartbeat_Consumer_Period
         (Node_Id  : in     ACO.Messages.Node_Nr;
          Period   : in     Natural;
          Subindex :    out Object_Subindex);

      function Get_Heartbeat_Consumer_Period
         (Node_Id : ACO.Messages.Node_Nr) return Natural;

      function Get_SDO_Server_Rx_CAN_Ids return ACO.Messages.Id_Array;

      function Get_SDO_Client_Rx_CAN_Ids return ACO.Messages.Id_Array;

   private
      Node_State   : ACO.States.State := ACO.States.Unknown_State;
   end Barrier_Type;

   type Object_Dictionary (Data : not null access Object_Data_Base'Class) is
      new Object_Dictionary_Base with
   record
      Protected_Data : Barrier_Type (Data);
   end record;

end ACO.OD;
