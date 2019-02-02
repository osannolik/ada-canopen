with Ada.Real_Time;
with ACO.Utils.Generic_Event;
with ACO.Messages;
with ACO.States;
with ACO.SDO_Sessions;
with ACO.OD_Types;
with ACO.Configuration;

package ACO.Events is

   use ACO.Configuration;

   package Node_State is new ACO.Utils.Generic_Event
      (Item_Type             => ACO.States.State_Transition,
       Max_Nof_Subscribers   => Max_Nof_Node_State_Change_Subscribers,
       Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   package Entry_Update is new ACO.Utils.Generic_Event
      (Item_Type             => ACO.OD_Types.Entry_Index,
       Max_Nof_Subscribers   => Max_Nof_Entry_Update_Subscribers,
       Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   package New_Message is new ACO.Utils.Generic_Event
      (Item_Type             => ACO.Messages.Message,
       Max_Nof_Subscribers   => 8,
       Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   package Periodic_Tick is new ACO.Utils.Generic_Event
      (Item_Type             => Ada.Real_Time.Time,
       Max_Nof_Subscribers   => 4,
       Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   type Heartbeat_Data is record
      Id    : ACO.Messages.Node_Nr;
      State : ACO.States.State;
   end record;

   package Node_Heartbeat is new ACO.Utils.Generic_Event
      (Item_Type             => Heartbeat_Data,
       Max_Nof_Subscribers   => 2,
       Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   package Heartbeat_Timeout is new ACO.Utils.Generic_Event
      (Item_Type             => ACO.Messages.Node_Nr,
       Max_Nof_Subscribers   => 2,
       Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   type Event_Manager is tagged limited record
      Received_Message : New_Message.Event_Publisher;
      Periodic_Action  : Periodic_Tick.Event_Publisher;
   end record;

   type SDO_Status_Data is record
      Endpoint_Id : ACO.SDO_Sessions.Endpoint_Nr;
      Result      : ACO.SDO_Sessions.SDO_Result;
   end record;

   package SDO_Status is new ACO.Utils.Generic_Event
      (Item_Type             => SDO_Status_Data,
       Max_Nof_Subscribers   => 8,
       Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   type Node_Event_Manager is tagged limited record
      Node_State_Modified : Node_State.Queued_Event_Publisher
         (Priority_Ceiling => Event_Queue_Ceiling);
      Slave_State_Change  : Node_State.Queued_Event_Publisher
         (Priority_Ceiling => Event_Queue_Ceiling);
      Entry_Updated       : Entry_Update.Queued_Event_Publisher
         (Priority_Ceiling => Event_Queue_Ceiling);
      Heartbeat_Received  : Node_Heartbeat.Queued_Event_Publisher
         (Priority_Ceiling => Event_Queue_Ceiling);
      Heartbeat_Timed_Out : Heartbeat_Timeout.Queued_Event_Publisher
         (Priority_Ceiling => Event_Queue_Ceiling);
      SDO_Status_Update   : SDO_Status.Queued_Event_Publisher
         (Priority_Ceiling => Event_Queue_Ceiling);
   end record;

   procedure Process
      (This : in out Node_Event_Manager);

end ACO.Events;
