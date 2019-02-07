with Ada.Real_Time;
with ACO.Utils.Generic_Event;
with ACO.Messages;
with ACO.States;
with ACO.SDO_Sessions;
with ACO.OD_Types;
with ACO.Configuration;

package ACO.Events is

   use ACO.Configuration;

   type Handler_Event_Type is
     (Tick,
      Received_Message);

   type Handler_Event_Data
      (Event : Handler_Event_Type := Tick)
   is record
      case Event is
         when Tick =>
            Current_Time : Ada.Real_Time.Time;

         when Received_Message =>
            Msg : ACO.Messages.Message;

      end case;
   end record;

   package HEP is new ACO.Utils.Generic_Event
     (Item_Type             => Handler_Event_Data,
      Max_Nof_Subscribers   => Max_Nof_Handler_Event_Subscribers,
      Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   type Handler_Event_Listener
      (Event : Handler_Event_Type)
   is abstract new HEP.Subscriber with null record;

   overriding
   procedure Update
      (This : access Handler_Event_Listener;
       Data : in     Handler_Event_Data);

   procedure On_Event
      (This : in out Handler_Event_Listener;
       Data : in     Handler_Event_Data) is abstract;

   type Handler_Event_Manager is tagged limited record
      Handler_Events : HEP.Event_Publisher;
   end record;


   type Event_Type is
     (State_Transition,
      Slave_State_Transition,
      OD_Entry_Update,
      Heartbeat_Received,
      Heartbeat_Timed_Out,
      SDO_Status_Update);

   type Heartbeat_Data is record
      Id    : ACO.Messages.Node_Nr;
      State : ACO.States.State;
   end record;

   type SDO_Status_Data is record
      Endpoint_Id : ACO.SDO_Sessions.Endpoint_Nr;
      Result      : ACO.SDO_Sessions.SDO_Result;
   end record;

   type Slave_State_Data is record
      State   : ACO.States.State_Transition;
      Node_Id : ACO.Messages.Node_Nr;
   end record;

   type Event_Data
      (Event : Event_Type := State_Transition)
   is record
      case Event is
         when State_Transition =>
            State : ACO.States.State_Transition;

         when Slave_State_Transition =>
            Slave : Slave_State_Data;

         when OD_Entry_Update =>
            Index : ACO.OD_Types.Entry_Index;

         when Heartbeat_Received =>
            Received_Heartbeat : Heartbeat_Data;

         when Heartbeat_Timed_Out =>
            Node_Id : ACO.Messages.Node_Nr;

         when SDO_Status_Update =>
            SDO_Status : SDO_Status_Data;

      end case;
   end record;

   package EP is new ACO.Utils.Generic_Event
     (Item_Type             => Event_Data,
      Max_Nof_Subscribers   => Max_Nof_Node_Event_Subscribers,
      Max_Nof_Queued_Events => Max_Nof_Event_Queue_Data_Items);

   type Event_Listener
      (Event : Event_Type)
   is abstract new EP.Subscriber with null record;

   overriding
   procedure Update
      (This : access Event_Listener;
       Data : in     Event_Data);

   procedure On_Event
      (This : in out Event_Listener;
       Data : in     Event_Data) is abstract;

   type Node_Event_Manager is tagged limited record
      Node_Events : EP.Queued_Event_Publisher
         (Priority_Ceiling => Event_Queue_Ceiling);
   end record;

   procedure Process
      (This : in out Node_Event_Manager);

end ACO.Events;
