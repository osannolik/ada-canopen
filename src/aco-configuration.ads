package ACO.Configuration is
   --  Defines static parameters for the CANopen stack

   pragma Pure;

   Max_Nof_Heartbeat_Slaves : constant := 8;

   Max_Nof_Entry_Update_Subscribers      : constant := 8;
   Max_Nof_Node_State_Change_Subscribers : constant := 8;

   Received_Messages_Buffer_Size : constant := 8;

end ACO.Configuration;
