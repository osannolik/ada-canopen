with Ada.Real_Time; use Ada.Real_Time;
with ACO.Drivers.Socket;
with ACO.Messages;
with ACO.Nodes;
with ACO.States;
with ACO.OD;
with ACO.Log;
with Ada.Text_IO.Text_Streams;
with ACO.OD_Types;
with ACO.OD.Example;


with Ada.Text_IO;
with Ada.Exceptions;
with ACO.OD_Types.Entries;

package body App is
   use Ada.Text_IO;

   O_Data : aliased ACO.OD.Example.Dictionary_Data;

   O : aliased ACO.OD.Object_Dictionary (O_Data'Access);

   D : aliased ACO.Drivers.Socket.CAN_Driver;

   N : aliased ACO.Nodes.Node (Id => 1, Od => O'Access, Driver => D'Access);

   T : ACO.Nodes.Receiver_Task (N'Access);

   W : ACO.Nodes.Periodic_Task (N'Access);

   procedure Run
   is
      Next_Release : Time := Clock;

      --  Msg_Rx : ACO.Messages.Message;
      Msg_Tx : ACO.Messages.Message :=
         ACO.Messages.Create (CAN_Id => 16#1#,
                              RTR    => False,
                              Data   => (1, 2, 3, 4));
      X : ACO.Messages.Data_Type := 10;

      use type ACO.Messages.Data_Type;

   begin

      declare

--           Obj_Ref : ACO.OD_Types.Object_Ref := O.Get_Object_Ref (16#1008#);
--
--           Obj : ACO.OD_Types.Object_Base'Class := O.Get_Object (16#1008#);
--
--           Bulle_Ref : ACO.OD_Types.Entry_Ref := O.Get_Entry_Ref (16#1008#, 0);

         Bulle : ACO.OD_Types.Entry_Base'Class := O.Get_Entry (16#1008#, 0);

         --  V : constant ACO.OD_Types.Entries.F32 := ACO.OD_Types.Entries.F32 (Bulle);

         V2 : ACO.OD_Types.Byte_Array := Bulle.Read;

         S : constant ACO.OD_Types.Entries.Visible_String :=
            ACO.OD.Example.Device_Name_Entry'Class (Bulle).Read;

         --         pragma Unreferenced (Obj, Obj_Ref, Bulle_Ref, Bulle, V2, S);

         U : constant ACO.OD_Types.Entries.Entry_U32 :=
            ACO.OD_Types.Entries.Create (ACO.OD_Types.WO, 1337);

         UPre : ACO.OD_Types.Entry_Base'Class := O.Get_Entry (16#1005#, 0);

         CHBT : constant Natural := O.Get_Heartbeat_Consumer_Period (1);

         pragma Unreferenced (UPre, CHBT);--, S);

         Hbt_Per : Natural;
      begin

         Hbt_Per := O.Get_Heartbeat_Consumer_Period (4);

         Hbt_Per := 2 * Hbt_Per;

         O.Set_Heartbeat_Consumer_Period (4, Hbt_Per);

         Hbt_Per := O.Get_Heartbeat_Consumer_Period (4);

         V2 (V2'Last) := V2 (V2'First);

         Bulle.Write (V2);

         O.Set_Entry (Bulle, 16#1008#, 0);

         O.Set_Entry (U, 16#1005#, 0);
         UPre := O.Get_Entry (16#1005#, 0);

         Ada.Text_IO.Put_Line (S);
      end;



      ACO.Log.Set_Stream (Text_Streams.Stream (Current_Output));
      ACO.Log.Set_Level (ACO.Log.Debug);

      D.Initialize;

      N.Set_State (ACO.States.Initializing);

      loop
         --  D.Send_Message (Msg_Tx);

         if X >= 20 then
            X := 10;
         else
            X := X + 1;
         end if;

         Msg_Tx.Data (Msg_Tx.Data'First) := X;

         Next_Release := Next_Release + Milliseconds (500);
         delay until Next_Release;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

   end Run;

end App;
