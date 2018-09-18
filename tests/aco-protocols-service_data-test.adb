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

   procedure Let_Time_Pass
      (S       : in out SDO;
       Time_Ms : in     Natural)
   is
      use Ada.Real_Time;
      Next_Release : Time := Clock;
   begin
      S.Od.Events.Process;

      for T in 1 .. Time_Ms loop
         Next_Release := Next_Release + Milliseconds (1);
         delay until Next_Release;
         S.Periodic_Actions;
      end loop;
   end Let_Time_Pass;
   pragma Unreferenced (Let_Time_Pass);

   function For_All_Sessions_Check_State
      (S       : SDO;
       Service : ACO.SDO_Sessions.Services)
       return Boolean
   is
      (for all I in ACO.SDO_Sessions.Valid_Endpoint_Nr'Range =>
          S.Sessions.Get (I).Service = Service);

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
             Index    => 16#1017#,
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
                 "Expedited Download: value in server OD did not update correctly");
      end;
   end Expedited_Download_Test;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Expedited_Download_Test;
   end Run_Test;

end ACO.Protocols.Service_Data.Test;
