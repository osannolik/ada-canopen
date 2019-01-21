with ACO.States;
with ACO.Messages;
with ACO.Events;
with ACO.OD_Types;
with ACO.SDO_Sessions;

package ACO.OD is

   use ACO.OD_Types;

   subtype Comm_Profile_Index is Object_Index range 16#1000# .. 16#1FFF#;

   Comm_Cycle_Period_Index     : constant := 16#1006#;
   Heartbeat_Producer_Index    : constant := 16#1017#;
   Heartbeat_Consumer_Index    : constant := 16#1016#;
   Sync_Counter_Overflow_Index : constant := 16#1019#;
   SDO_Server_Base_Index       : constant := 16#1200#;
   SDO_Client_Base_Index       : constant := 16#1280#;

   type Object_Dictionary_Base is abstract tagged limited record
      Events : ACO.Events.Node_Event_Manager;
   end record;

   type Object_Dictionary is abstract new Object_Dictionary_Base with private;

   function Objects
      (This : Object_Dictionary)
       return Profile_Objects_Ref is abstract;

   function Index_Map
      (This  : Object_Dictionary;
       Index : Object_Index)
       return Index_Type is abstract;

   function Object_Exist
      (This  : Object_Dictionary'Class;
       Index : Object_Index)
       return Boolean;

   function Entry_Exist
      (This     : Object_Dictionary'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex)
       return Boolean;

   function Maximum_Nof_Entries
      (This  : Object_Dictionary;
       Index : Object_Index)
       return Natural
      with Pre => This.Object_Exist (Index);

   function Is_Entry_Compatible
      (This     : Object_Dictionary;
       An_Entry : Entry_Base'Class;
       Index    : Object_Index;
       Subindex : Object_Subindex)
       return Boolean
      with Pre => This.Entry_Exist (Index, Subindex);

   function Is_Entry_Writable
      (This  : Object_Dictionary;
       Index : Entry_Index)
       return Boolean;

   function Is_Entry_Readable
      (This  : Object_Dictionary;
       Index : Entry_Index)
       return Boolean;

   function Get_Entry
      (This     : Object_Dictionary;
       Index    : Object_Index;
       Subindex : Object_Subindex) return Entry_Base'Class
      with Pre => This.Entry_Exist (Index, Subindex);

   procedure Set_Entry
      (This      : in out Object_Dictionary;
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
       Period  : in     Natural)
      with Pre => This.Object_Exist (Heartbeat_Consumer_Index);

   function Get_Heartbeat_Consumer_Period
      (This    : Object_Dictionary;
       Node_Id : ACO.Messages.Node_Nr)
       return Natural;

   procedure Set_Heartbeat_Producer_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
      with Pre => This.Entry_Exist (Heartbeat_Producer_Index, 0);

   function Get_Heartbeat_Producer_Period
      (This : Object_Dictionary)
       return Natural;

   procedure Set_Communication_Cycle_Period
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
      with Pre => This.Entry_Exist (Comm_Cycle_Period_Index, 0);

   function Get_Communication_Cycle_Period
      (This : Object_Dictionary)
       return Natural
      with Pre => This.Entry_Exist (Comm_Cycle_Period_Index, 0);

   procedure Set_Sync_Counter_Overflow
      (This    : in out Object_Dictionary;
       Period  : in     Natural)
      with Pre => This.Entry_Exist (Sync_Counter_Overflow_Index, 0);

   function Get_Sync_Counter_Overflow
      (This : Object_Dictionary)
       return Natural
      with Pre => This.Entry_Exist (Sync_Counter_Overflow_Index, 0);

   function Get_SDO_Server_Parameters
      (This : Object_Dictionary)
       return ACO.SDO_Sessions.SDO_Parameter_Array
      with Pre => This.Object_Exist (SDO_Server_Base_Index);

   function Get_SDO_Client_Parameters
      (This : Object_Dictionary)
       return ACO.SDO_Sessions.SDO_Parameter_Array
      with Pre => This.Object_Exist (SDO_Client_Base_Index);

private

   type Object_Dictionary is abstract new Object_Dictionary_Base with record
      Node_State : ACO.States.State := ACO.States.Unknown_State;
   end record;

   function Object
      (This  : Object_Dictionary'Class;
       Index : Object_Index)
       return Object_Ref
      with Inline;

end ACO.OD;
