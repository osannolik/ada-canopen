with Ada.Real_Time;
with ACO.Drivers.Socket;
with ACO.CANopen;
with ACO.Nodes.Locals;
with ACO.Nodes.Remotes;
with ACO.OD.Example;
with ACO.Log;
with Ada.Text_IO.Text_Streams;

with Ada.Text_IO;
with Ada.Exceptions;

with ACO.OD_Types;
with ACO.OD_Types.Entries;
with ACO.SDO_Sessions;

package body App is

   O : aliased ACO.OD.Example.Dictionary;

   D : aliased ACO.Drivers.Socket.CAN_Driver;

   H : aliased ACO.CANopen.Handler (Driver => D'Access);

   W : ACO.CANopen.Periodic_Task (H'Access, Period_Ms => 10);

   procedure Run
      (Node : in out ACO.Nodes.Node_Base'Class)
   is
      use Ada.Text_IO;
      use type Ada.Real_Time.Time;

      Next_Release : Ada.Real_Time.Time;
   begin
      ACO.Log.Set_Stream (Text_Streams.Stream (Current_Output));
      ACO.Log.Set_Level (ACO.Log.Debug);

      D.Initialize;

      Node.Start;

      Next_Release := Ada.Real_Time.Clock;

      loop
         Next_Release := Next_Release + Ada.Real_Time.Milliseconds (10);
         delay until Next_Release;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Run;

   procedure Run_Local
   is
      Node : ACO.Nodes.Locals.Local
         (Id => 1, Handler => H'Access, Od => O'Access);
   begin
      Run (Node);
   end Run_Local;

   procedure Run_Remote
   is
      Node : aliased ACO.Nodes.Remotes.Remote
         (Id => 1, Handler => H'Access, Od => O'Access);

      use Ada.Text_IO;
      use type Ada.Real_Time.Time;
      use ACO.OD_Types;
      use ACO.OD_Types.Entries;

      Next_Release : Ada.Real_Time.Time;
      Value : U16 := 0;
   begin
      ACO.Log.Set_Stream (Text_Streams.Stream (Current_Output));
      ACO.Log.Set_Level (ACO.Log.Debug);

      D.Initialize;

      Node.Start;

      --  Poor man's "wait until heartbeat received"
      delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (3000);

      Next_Release := Ada.Real_Time.Clock;

      loop
         Next_Release := Next_Release + Ada.Real_Time.Milliseconds (1000);
         delay until Next_Release;

         declare
            function Read_U16 is new ACO.Nodes.Remotes.Generic_Read
               (Entry_T => Entry_U16);
            To_Entry : constant Entry_U16 :=
               Read_U16 (Node, ACO.OD.Heartbeat_Producer_Index, 0);
         begin
            Ada.Text_IO.Put_Line
               ("Generic function read value = " & U16' (To_Entry.Read)'Img);
         end;

         --  Alternative way of reading...
         declare
            To_Entry : Entry_U16;
            Result   : ACO.Nodes.Remotes.SDO_Result;
         begin
            Node.Read
               (Index    => ACO.OD.Heartbeat_Producer_Index,
                Subindex => 0,
                Result   => Result,
                To_Entry => To_Entry);

            Ada.Text_IO.Put_Line ("Remote node read result = " & Result'Img);

            if ACO.SDO_Sessions.Is_Complete (Result) then
               Ada.Text_IO.Put_Line ("Value =" & U16' (To_Entry.Read)'Img);
            end if;
         end;

         declare
            A_Value  : constant U16 := 500 + Value;
            An_Entry : constant Entry_U16 := Create (RW, A_Value);
         begin
            Node.Write
               (Index    => ACO.OD.Heartbeat_Producer_Index,
                Subindex => 0,
                An_Entry => An_Entry);

            Ada.Text_IO.Put_Line ("Remote node write value =" & A_Value'Img);
         end;

         Value := (Value + 1) mod 500;

      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Run_Remote;

end App;
