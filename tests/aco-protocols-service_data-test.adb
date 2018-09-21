with AUnit.Assertions; use AUnit.Assertions;

with ACO.OD.Example;
with ACO.OD_Types.Entries;
with ACO.States;
with ACO.Drivers.Dummy;
with Ada.Real_Time;

package body ACO.Protocols.Service_Data.Test is
   pragma Assertion_Policy (Check);

   use ACO.Drivers.Dummy;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Service Data Object Test");
   end Name;

   T_Now : Ada.Real_Time.Time;

   procedure Let_Time_Pass
      (S       : in out SDO;
       Time_Ms : in     Natural)
   is
      use Ada.Real_Time;
   begin
      S.Od.Events.Process;

      for DT in 1 .. Time_Ms loop
         T_Now := T_Now + Milliseconds (1);
         S.Periodic_Actions (T_Now);
      end loop;
   end Let_Time_Pass;

   function For_All_Sessions_Check_State
      (S       : SDO;
       Service : ACO.SDO_Sessions.Services)
       return Boolean
   is --  All sessions has Service and if None then it may not buffer any data
      (for all I in ACO.SDO_Sessions.Valid_Endpoint_Nr'Range =>
          (S.Sessions.Get (I).Service = Service) and
             (S.Sessions.Get (I).Service /= None
              or S.Sessions.Length_Buffer (I) = 0));

   procedure Expedited_Download_Test
   is
      use ACO.SDO_Commands;
      use ACO.OD_Types.Entries;

      OD_Data : aliased ACO.OD.Example.Dictionary_Data;
      OD      : aliased ACO.OD.Object_Dictionary (OD_Data'Access);
      Driver  : aliased ACO.Drivers.Dummy.Dummy_Driver;
      S       : SDO (OD'Access, Driver'Access);
   begin
      S.Od.Set_Heartbeat_Producer_Period (500);
      S.Od.Set_Node_State (ACO.States.Pre_Operational);

      declare
         Value : constant := 1000;
         E     : constant Entry_Base'Class := Entry_U16'(Create (RW, Value));
         Msg : Message;
      begin
         --  As client, request to download entry to 0x1017 on node 1.
         --  Since entry size is <= 4 the download will be expedited.
         S.Write_Remote_Entry
            (Node     => 1,
             Index    => ACO.OD.Heartbeat_Producer_Index,
             Subindex => 0,
             An_Entry => E);

         --  As server, receive and process download init request
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As client, receive and process download init response
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  Nothing more should have been sent
         Assert (Dummy_Driver (S.Driver.all).Nof_Sent = 0,
                 "Client should not have sent a massage");

         --  All sessions should be closed (service = None)
         Assert (For_All_Sessions_Check_State (S, None),
                 "A session has not been ended properly");

         --  Check that the value has been updated
         Assert (Value = S.Od.Get_Heartbeat_Producer_Period,
                 "Value in server OD did not update correctly");
      end;
   end Expedited_Download_Test;

   procedure Segmented_Download_Test
   is
      use ACO.SDO_Commands;
      use ACO.OD_Types.Entries;
      use ACO.OD.Example;

      OD_Data : aliased ACO.OD.Example.Dictionary_Data;
      OD      : aliased ACO.OD.Object_Dictionary (OD_Data'Access);
      Driver  : aliased ACO.Drivers.Dummy.Dummy_Driver;
      S       : SDO (OD'Access, Driver'Access);
   begin
      S.Od.Set_Node_State (ACO.States.Pre_Operational);

      declare
         Value : constant Device_Name_String := "A Super Duper";
         E     : constant Entry_Base'Class := Device_Name_Entry'(Create (RW, Value));
         Msg : Message;
      begin
         --  As client, request to download entry to 0x1017 on node 1.
         --  Since entry size is <= 4 the download will be expedited.
         S.Write_Remote_Entry
            (Node     => 1,
             Index    => 16#1008#,
             Subindex => 0,
             An_Entry => E);

         --  As server, receive and process download init request
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As client, receive and process download init response
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As server, receive and process data segment of length 7
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As client, receive and process data segment response
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As server, receive and process data segment of length 6
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As client, receive and process data segment response
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  Nothing more should have been sent
         Assert (Dummy_Driver (S.Driver.all).Nof_Sent = 0,
                 "Client should not have sent a massage");

         --  All sessions should be closed (service = None)
         Assert (For_All_Sessions_Check_State (S, None),
                 "A session has not been ended properly");

         --  Check that the value has been updated
         declare
            New_Value : constant Entry_Base'Class := S.Od.Get_Entry (16#1008#, 0);
         begin
            Assert (Device_Name_Entry (New_Value).Read = Value,
                    "Value in server OD did not update correctly");
         end;
      end;
   end Segmented_Download_Test;

   function To_Error (Msg : Message) return Error_Type is
      use ACO.SDO_Commands;
      use type ACO.SDO_Commands.Abort_Code_Type;
      Cmd : constant Abort_Cmd := Convert (Msg);
   begin
      for E in Error_Type'Range loop
         if Abort_Code (E) = Code (Cmd) then
            return E;
         end if;
      end loop;
      return Unknown;
   end To_Error;

   procedure Download_Timeout_Test
   is
      use ACO.SDO_Commands;
      use ACO.OD_Types.Entries;
      use ACO.OD.Example;

      OD_Data : aliased ACO.OD.Example.Dictionary_Data;
      OD      : aliased ACO.OD.Object_Dictionary (OD_Data'Access);
      Driver  : aliased ACO.Drivers.Dummy.Dummy_Driver;
      S       : SDO (OD'Access, Driver'Access);
      Timeout : constant := ACO.Configuration.SDO_Session_Timeout_Ms;
   begin
      T_Now := Ada.Real_Time.Clock;

      S.Od.Set_Node_State (ACO.States.Pre_Operational);

      declare
         Value : constant Device_Name_String := "A Super Duper";
         E     : constant Entry_Base'Class := Device_Name_Entry'(Create (RW, Value));
         Msg : Message;
      begin
         -----------------------------------------------------------------------
         --  # 1. First message lost or server not responding
         -----------------------------------------------------------------------
         S.Write_Remote_Entry
            (Node     => 1,
             Index    => 16#1008#,
             Subindex => 0,
             An_Entry => E);

         --  The message that got away...
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);

         Let_Time_Pass (S, Timeout + 10);

         --  Client should have sent an abort message
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);
         Assert (To_Error (Msg) = SDO_Protocol_Timed_Out,
                 "Incorrect abort code");

         --  Nothing more should have been sent
         Assert (Dummy_Driver (S.Driver.all).Nof_Sent = 0,
                 "Client should not have sent a massage");

         --  All sessions should be closed (service = None)
         Assert (For_All_Sessions_Check_State (S, None),
                 "A session has not been ended properly");

         -----------------------------------------------------------------------
         --  # 2. Server response message lost or client not responding
         -----------------------------------------------------------------------
         S.Write_Remote_Entry
            (Node     => 1,
             Index    => 16#1008#,
             Subindex => 0,
             An_Entry => E);

         --  As server, receive and process download init request
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  The message that got away...
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);

         Let_Time_Pass (S, Timeout + 10);

         --  Client and Server should have sent an abort message
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);
         Assert (To_Error (Msg) = SDO_Protocol_Timed_Out,
                 "Incorrect abort code");
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);
         Assert (To_Error (Msg) = SDO_Protocol_Timed_Out,
                 "Incorrect abort code");

         --  Nothing more should have been sent
         Assert (Dummy_Driver (S.Driver.all).Nof_Sent = 0,
                 "Client should not have sent a massage");

         --  All sessions should be closed (service = None)
         Assert (For_All_Sessions_Check_State (S, None),
                 "A session has not been ended properly");

         -----------------------------------------------------------------------
         --  # 3. Client segment data message lost or server not responding
         -----------------------------------------------------------------------
         S.Write_Remote_Entry
            (Node     => 1,
             Index    => 16#1008#,
             Subindex => 0,
             An_Entry => E);

         --  As server, receive and process download init request
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As client, receive and process download init response
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  The message that got away...
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);

         Let_Time_Pass (S, Timeout + 10);

         --  Client and Server should have sent an abort message
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);
         Assert (To_Error (Msg) = SDO_Protocol_Timed_Out,
                 "Incorrect abort code");
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);
         Assert (To_Error (Msg) = SDO_Protocol_Timed_Out,
                 "Incorrect abort code");

         --  Nothing more should have been sent
         Assert (Dummy_Driver (S.Driver.all).Nof_Sent = 0,
                 "Client should not have sent a massage");

         --  All sessions should be closed (service = None)
         Assert (For_All_Sessions_Check_State (S, None),
                 "A session has not been ended properly");

         -----------------------------------------------------------------------
         --  # 4. Server segment response message lost or client not responding
         -----------------------------------------------------------------------
         S.Write_Remote_Entry
            (Node     => 1,
             Index    => 16#1008#,
             Subindex => 0,
             An_Entry => E);

         --  As server, receive and process download init request
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As client, receive and process download init response
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  As server, receive and process data segment of length 7
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);

         --  The message that got away...
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);

         Let_Time_Pass (S, Timeout + 10);

         --  Client and Server should have sent an abort message
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);
         Assert (To_Error (Msg) = SDO_Protocol_Timed_Out,
                 "Incorrect abort code");
         Dummy_Driver (S.Driver.all).Get_First_Sent (Msg);
         S.Message_Received (Msg);
         Assert (To_Error (Msg) = SDO_Protocol_Timed_Out,
                 "Incorrect abort code");

         --  Nothing more should have been sent
         Assert (Dummy_Driver (S.Driver.all).Nof_Sent = 0,
                 "Client should not have sent a massage");

         --  All sessions should be closed (service = None)
         Assert (For_All_Sessions_Check_State (S, None),
                 "A session has not been ended properly");
      end;
   end Download_Timeout_Test;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Expedited_Download_Test;
      Segmented_Download_Test;
      Download_Timeout_Test;
   end Run_Test;

end ACO.Protocols.Service_Data.Test;
