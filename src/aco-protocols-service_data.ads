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
private with ACO.Configuration;

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

   subtype Abort_Code_Type is Interfaces.Unsigned_32;

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
      Abort_Req              : constant := 4;

      function Get_CS (Msg : Message) return Unsigned_3 is
         (Unsigned_3 (Shift_Right (Msg.Data (0), 5) and 2#111#));

      function Get_Index (Msg : Message) return Entry_Index is
         ((Object => Swap_Bus (Octets_2 (Msg.Data (1 .. 2))),
           Sub    => Msg.Data (3)));

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

      function Get_Data_Size (Cmd : Download_Initiate_Cmd) return Natural is
         (Natural (Swap_Bus (Octets_4 (Cmd.Data))));

      function To_Download_Initiate_Cmd
         (Msg : Message) return Download_Initiate_Cmd
      is
         ((As_Raw => True, Raw => Msg.Data));

      type Download_Segment_Cmd (As_Raw : Boolean := False) is record
         case As_Raw is
            when True =>
               Raw         : Data_Array (0 .. 7);
            when False =>
               Command     : Unsigned_3;
               Toggle      : Boolean;
               Nof_No_Data : Unsigned_3;
               Is_Complete : Boolean;
               Data        : Data_Array (0 .. 6);
         end case;
      end record
         with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

      for Download_Segment_Cmd use record
         Raw         at 0 range 0 .. 63;
         Data        at 0 range 8 .. 63;
         Command     at 0 range 5 .. 7;
         Toggle      at 0 range 4 .. 4;
         Nof_No_Data at 0 range 1 .. 3;
         Is_Complete at 0 range 0 .. 0;
      end record;

      function To_Download_Segment_Cmd
         (Msg : Message) return Download_Segment_Cmd
      is
         ((As_Raw => True, Raw => Msg.Data));


      type Download_Initiate_Resp (As_Raw : Boolean := False) is record
         case As_Raw is
            when True =>
               Raw      : Data_Array (0 .. 7) := (others => 0);
            when False =>
               Command  : Unsigned_3;
               Index    : Unsigned_16;
               Subindex : Unsigned_8;
         end case;
      end record
         with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

      for Download_Initiate_Resp use record
         Raw      at 0 range 0 .. 63;
         Subindex at 0 range 24 .. 31;
         Index    at 0 range 8 .. 23;
         Command  at 0 range 5 .. 7;
      end record;

      function Create_Response (Index : Entry_Index)
                                return Download_Initiate_Resp
      is
         ((As_Raw   => False,
           Command  => Download_Initiate_Conf,
           Index    => Swap_Bus (Index.Object),
           Subindex => Index.Sub));

      type Download_Segment_Resp (As_Raw : Boolean := False) is record
         case As_Raw is
            when True =>
               Raw     : Data_Array (0 .. 7) := (others => 0);
            when False =>
               Command : Unsigned_3;
               Toggle  : Boolean;
         end case;
      end record
         with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

      for Download_Segment_Resp use record
         Raw     at 0 range 0 .. 63;
         Command at 0 range 5 .. 7;
         Toggle  at 0 range 4 .. 4;
      end record;

      function Create_Response (Toggle : Boolean)
                                return Download_Segment_Resp
      is
         ((As_Raw  => False,
           Command => Download_Segment_Conf,
           Toggle  => Toggle));


      type Abort_Cmd (As_Raw : Boolean := False) is record
         case As_Raw is
            when True =>
               Raw      : Data_Array (0 .. 7) := (others => 0);
            when False =>
               Command  : Unsigned_3;
               Index    : Unsigned_16;
               Subindex : Unsigned_8;
               Code     : Unsigned_32;
         end case;
      end record
         with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

      for Abort_Cmd use record
         Raw      at 0 range 0 .. 63;
         Code     at 0 range 32 .. 63;
         Subindex at 0 range 24 .. 31;
         Index    at 0 range 8 .. 23;
         Command  at 0 range 5 .. 7;
      end record;

      function Create_Abort (Index : Entry_Index;
                             Code  : Abort_Code_Type)
                             return Abort_Cmd
      is
         ((As_Raw   => False,
           Command  => Abort_Req,
           Index    => Swap_Bus (Index.Object),
           Subindex => Index.Sub,
           Code     => Swap_Bus (Code)));

   end Commands;

   type Error_Type is
      (Nothing,
       General_Error,
       Invalid_Value_For_Parameter,
       Toggle_Bit_Not_Altered,
       SDO_Protocol_Timed_Out,
       Command_Specifier_Not_Valid_Or_Unknown,
       Object_Does_Not_Exist_In_The_Object_Dictionary,
       Attempt_To_Write_A_Read_Only_Object,
       Failed_To_Transfer_Or_Store_Data,
       Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);

   Abort_Code : constant array (Error_Type) of Abort_Code_Type :=
      (Nothing                                               => 16#0000_0000#,
       General_Error                                         => 16#0800_0000#,
       Invalid_Value_For_Parameter                           => 16#0609_0030#,
       Toggle_Bit_Not_Altered                                => 16#0503_0000#,
       SDO_Protocol_Timed_Out                                => 16#0504_0000#,
       Command_Specifier_Not_Valid_Or_Unknown                => 16#0504_0001#,
       Object_Does_Not_Exist_In_The_Object_Dictionary        => 16#0602_0000#,
       Attempt_To_Write_A_Read_Only_Object                   => 16#0601_0002#,
       Failed_To_Transfer_Or_Store_Data                      => 16#0800_0020#,
       Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control => 16#0800_0021#);

   overriding
   procedure Initialize (This : in out SDO);

   overriding
   procedure Finalize (This : in out SDO);

   package Alarms is new ACO.Utils.Generic_Alarms
      (Configuration.Max_Nof_Simultaneous_SDO_Sessions);

   type Alarm (SDO_Ref : access SDO'Class := null) is new Alarms.Alarm_Type with
      record
         Id : Endpoint_Nr := No_Endpoint_Id;
      end record;

   overriding
   procedure Signal (This : access Alarm);

   type Alarm_Array is array (Valid_Endpoint_Nr'Range) of aliased Alarm;

   type SDO
      (Od     : not null access ACO.OD.Object_Dictionary'Class;
       Driver : not null access ACO.Drivers.Driver'Class) is new Protocol (Od) with
   record
      Sessions      : Session_Manager;
      Event_Manager : Alarms.Alarm_Manager;
      Alarms        : Alarm_Array := (others => (SDO'Access, No_Endpoint_Id));
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

   procedure Start_Alarm
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type);

   procedure Stop_Alarm
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type);

   procedure Message_Received_For_Server
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Server_Download_Init
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Server_Download_Segment
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Message_Received_For_Client
      (This     : in out SDO;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Write
      (This    : in out SDO;
       Index   : in     ACO.OD_Types.Entry_Index;
       Data    : in     Data_Array;
       Error   :    out Error_Type);

   procedure Send_SDO_Response
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type;
       Raw_Data : in     Msg_Data);

   procedure Send_Abort
      (This     : in out SDO;
       Endpoint : in     Endpoint_Type;
       Error    : in     Error_Type;
       Index    : in     ACO.OD_Types.Entry_Index := (0,0));

end ACO.Protocols.Service_Data;
