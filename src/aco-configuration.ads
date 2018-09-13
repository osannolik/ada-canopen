with System;

package ACO.Configuration is
   --  Defines static parameters for the CANopen stack

   pragma Pure;

   Max_Nof_Heartbeat_Slaves : constant := 8;

   Max_Nof_Entry_Update_Subscribers      : constant := 8;
   Max_Nof_Node_State_Change_Subscribers : constant := 8;

   Max_Nof_Event_Queue_Data_Items : constant := 4;
   Event_Queue_Priority  : constant System.Priority := System.Max_Priority;

   Periodic_Task_Period_Ms : constant := 1;
   Periodic_Task_Priority  : constant System.Priority := System.Max_Priority;

   Receiver_Task_Priority  : constant System.Priority := System.Default_Priority;

   Received_Messages_Buffer_Size     : constant := 8;
   Received_Messages_Buffer_Priority : constant System.Priority :=
      System.Max_Priority;

   Max_Nof_Simultaneous_SDO_Sessions : constant := 4;

end ACO.Configuration;
