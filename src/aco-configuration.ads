with System;

package ACO.Configuration is
   --  Defines static parameters for the CANopen stack

   pragma Pure;

   Max_Nof_Heartbeat_Slaves : constant := 8;

   Max_Nof_Handler_Event_Subscribers : constant := 16;
   Max_Nof_Node_Event_Subscribers    : constant := 16;

   Max_Nof_Event_Queue_Data_Items : constant := 16;
   Event_Queue_Ceiling : constant System.Priority := System.Max_Priority;

   Periodic_Task_Priority  : constant System.Priority := System.Max_Priority;

   Messages_Buffer_Size    : constant := 8;
   Messages_Buffer_Ceiling : constant System.Priority := System.Max_Priority;

   Max_Nof_Simultaneous_SDO_Sessions : constant := 4;
   Max_SDO_Transfer_Size             : constant := 32;

   SDO_Session_Timeout_Ms : constant := 3000;

end ACO.Configuration;
