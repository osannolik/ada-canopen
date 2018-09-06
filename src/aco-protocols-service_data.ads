with ACO.Messages;
with ACO.OD;
with ACO.Drivers;
with ACO.States;

private with System;
private with ACO.Log;
private with ACO.Utils.Generic_Alarms;
private with ACO.SDO_Sessions;
private with Interfaces;
private with ACO.OD_Types;
private with ACO.Utils.Byte_Order;

package ACO.Protocols.Service_Data is

   use ACO.Messages;

   SDO_S2C_Id : constant Function_Code := 16#B#;
   SDO_C2S_Id : constant Function_Code := 16#C#;

   type SDO
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class)
   is new Protocol with private;

   procedure Message_Received
     (This : in out SDO;
      Msg  : in     Message);

   procedure Periodic_Actions
     (This : in out SDO);

private
   use ACO.SDO_Sessions;

   type Unsigned_3 is mod 2 ** 3 with Size => 3;
   type Unsigned_2 is mod 2 ** 2 with Size => 2;

   package Commands is
      use Interfaces;
      use ACO.OD_Types;
      use ACO.Utils.Byte_Order;

      Download_Initiate_Req  : constant := 1;
      Download_Initiate_Conf : constant := 3;
      Download_Segment_Req   : constant := 0;
      Download_Segment_Conf  : constant := 1;
      Upload_Initiate_Req    : constant := 2;
      Upload_Initiate_Conf   : constant := 2;
      Upload_Segment_Req     : constant := 3;
      Upload_Segment_Conf    : constant := 0;

      function Get_CS (Msg : Message) return Unsigned_3 is
         (Unsigned_3 (Shift_Right (Msg.Data (0), 5) and 2#111#));

      function Get_Index (Msg : Message) return Entry_Index is
         ((Index    => Swap_Bus (Octets_2 (Msg.Data (1 .. 2))),
           Subindex => Msg.Data (3)));

--        function Index_To_Bus (Index : Object_Index) return Data_Array is
--           (Data_Array (Octets_2' (Swap_Bus (Unsigned_16 (Index)))));

      type Download_Initiate_Cmd (As_Raw : Boolean := False) is record
         case As_Raw is
            when True =>
               Raw               : Data_Array (0 .. 7);
            when False =>
               Command           : Unsigned_3;
               Nof_No_Data       : Unsigned_2;
               Is_Expedited      : Boolean;
               Is_Size_Indicated : Boolean;
               Index             : Unsigned_16;
               Subindex          : Unsigned_8;
               Data              : Data_Array (0 .. 3);
         end case;
      end record
         with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

      for Download_Initiate_Cmd use record
         Raw               at 0 range 0 .. 63;
         Data              at 0 range 32 .. 63;
         Subindex          at 0 range 24 .. 31;
         Index             at 0 range 8 .. 23;
         Command           at 0 range 5 .. 7;
         Nof_No_Data       at 0 range 2 .. 3;
         Is_Expedited      at 0 range 1 .. 1;
         Is_Size_Indicated at 0 range 0 .. 0;
      end record;

      function To_Download_Initiate_Cmd
         (Msg : Message) return Download_Initiate_Cmd
      is
         ((As_Raw => True, Raw => Msg.Data));

--        function Is_Valid_Command (Msg : Message) return Boolean is
--           ((Msg.Length = NMT_Command'Size / 8) and then
--            Is_Valid_Cmd_Spec (To_NMT_Command (Msg).Command_Specifier));

   end Commands;

   overriding
   procedure Initialize (This : in out SDO);

   overriding
   procedure Finalize (This : in out SDO);

   package Alarms is new ACO.Utils.Generic_Alarms (1);

   type SDO
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol (Od) with
   record
      Event_Manager : Alarms.Alarm_Manager;
   end record;

   overriding
   procedure On_State_Change
     (This     : in out SDO;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure SDO_Log
     (This    : in out SDO;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

   function Get_Endpoint
      (Id         : Id_Type;
       Rx_CAN_Ids : Id_Array)
       return Endpoint_Nr;

   procedure Message_Received_For_Server
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Nr);

   procedure Message_Received_For_Client
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Nr);

   procedure Server_Download_Init
      (This : in out SDO;
       Msg  : in     Message);

end ACO.Protocols.Service_Data;
