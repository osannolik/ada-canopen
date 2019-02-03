with Ada.Real_Time;
with ACO.Drivers.Stm32f40x;
with STM32.Device;
with ACO.Nodes;
with ACO.OD.Example;
with ACO.CANopen;
with ACO.Nodes.Remotes;
with ACO.OD_Types;
with ACO.OD_Types.Entries;
with ACO.SDO_Sessions;

package body App is

   D : aliased ACO.Drivers.Stm32f40x.CAN_Driver (STM32.Device.CAN_1'Access);

   H : aliased ACO.CANopen.Handler (Driver => D'Access);

   T : ACO.CANopen.Periodic_Task (H'Access, 10);

   O : aliased ACO.OD.Example.Dictionary;

   procedure Run
   is
      use Ada.Real_Time;

      N : aliased ACO.Nodes.Remotes.Remote (Id => 1, Handler => H'Access, Od => O'Access);

      Next_Release : Time := Clock;
   begin

      D.Initialize;

      N.Start;

      loop
         --  H.Periodic_Actions (T_Now => Next_Release);

         declare
            use ACO.OD_Types.Entries;

            An_Entry : constant Entry_U8 :=
              Create (Accessability => ACO.OD_Types.RW,
                      Data          => 0);
            Req : ACO.Nodes.Remotes.SDO_Write_Request (N'Access);
            Result : ACO.Nodes.Remotes.SDO_Result;
         begin
            N.Write (Request  => Req,
                     Index    => 16#1000#,
                     Subindex => 1,
                     An_Entry => An_Entry);

            Req.Suspend_Until_Result (Result);

            case Req.Status is
               when ACO.SDO_Sessions.Complete =>
                  null;
               when ACO.SDO_Sessions.Error =>
                  null;
               when ACO.SDO_Sessions.Pending =>
                  null;
            end case;
         end;

         Next_Release := Next_Release + Milliseconds (10);
         delay until Next_Release;
      end loop;
   end Run;

end App;
