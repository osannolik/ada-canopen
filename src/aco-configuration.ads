with System;

package ACO.Configuration is
   --  Defines static parameters for the CANopen stack

   pragma Pure;

   Max_Nof_Heartbeat_Slaves : constant := 8;

   Max_Nof_Entry_Update_Subscribers      : constant := 8;
   Max_Nof_Node_State_Change_Subscribers : constant := 8;

   Periodic_Task_Period_Ms : constant := 1;
   Periodic_Task_Priority  : constant System.Priority := System.Max_Priority;

   Receiver_Task_Priority  : constant System.Priority := System.Default_Priority;

   Received_Messages_Buffer_Size     : constant := 8;
   Received_Messages_Buffer_Priority : constant System.Priority :=
      System.Max_Priority;

end ACO.Configuration;
