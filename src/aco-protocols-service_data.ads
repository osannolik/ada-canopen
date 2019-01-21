with Ada.Real_Time;
with ACO.CANopen;
with ACO.Messages;
with ACO.OD;
with ACO.SDO_Sessions;

private with ACO.Log;
private with ACO.Utils.Generic_Alarms;
private with ACO.Configuration;
private with ACO.SDO_Commands;
private with ACO.OD_Types;
private with ACO.States;

package ACO.Protocols.Service_Data is

   SDO_S2C_Id : constant ACO.Messages.Function_Code := 16#B#;
   SDO_C2S_Id : constant ACO.Messages.Function_Code := 16#C#;

   type SDO
      (Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new Protocol with private;

   function Tx_CAN_Id
      (This      : SDO;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type is abstract;

   function Rx_CAN_Id
      (This      : SDO;
       Parameter : ACO.SDO_Sessions.SDO_Parameters)
       return ACO.Messages.Id_Type is abstract;

   function Get_Endpoint
      (This      : SDO;
       Rx_CAN_Id : ACO.Messages.Id_Type)
       return ACO.SDO_Sessions.Endpoint_Type is abstract;

   overriding
   function Is_Valid
      (This : in out SDO;
       Msg  : in     ACO.Messages.Message)
       return Boolean;

   procedure Message_Received
     (This : in out SDO'Class;
      Msg  : in     ACO.Messages.Message)
      with Pre => This.Is_Valid (Msg);

   procedure Periodic_Actions
      (This  : in out SDO;
       T_Now : in     Ada.Real_Time.Time);

   function Get_Status
      (This : SDO;
       Id   : ACO.SDO_Sessions.Valid_Endpoint_Nr)
       return ACO.SDO_Sessions.SDO_Status;

   function Is_Complete
      (This : SDO;
       Id   : ACO.SDO_Sessions.Valid_Endpoint_Nr)
       return Boolean;

   procedure Clear
      (This : in out SDO;
       Id   : in     ACO.SDO_Sessions.Valid_Endpoint_Nr);

private


   type Error_Type is
      (Nothing,
       Unknown,
       General_Error,
       Invalid_Value_For_Parameter,
       Toggle_Bit_Not_Altered,
       SDO_Protocol_Timed_Out,
       Command_Specifier_Not_Valid_Or_Unknown,
       Object_Does_Not_Exist_In_The_Object_Dictionary,
       Attempt_To_Read_A_Write_Only_Object,
       Attempt_To_Write_A_Read_Only_Object,
       Failed_To_Transfer_Or_Store_Data,
       Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control);

   Abort_Code : constant array (Error_Type) of ACO.SDO_Commands.Abort_Code_Type :=
      (Nothing                                               => 16#0000_0000#,
       Unknown                                               => 16#0000_0000#,
       General_Error                                         => 16#0800_0000#,
       Invalid_Value_For_Parameter                           => 16#0609_0030#,
       Toggle_Bit_Not_Altered                                => 16#0503_0000#,
       SDO_Protocol_Timed_Out                                => 16#0504_0000#,
       Command_Specifier_Not_Valid_Or_Unknown                => 16#0504_0001#,
       Object_Does_Not_Exist_In_The_Object_Dictionary        => 16#0602_0000#,
       Attempt_To_Read_A_Write_Only_Object                   => 16#0601_0001#,
       Attempt_To_Write_A_Read_Only_Object                   => 16#0601_0002#,
       Failed_To_Transfer_Or_Store_Data                      => 16#0800_0020#,
       Failed_To_Transfer_Or_Store_Data_Due_To_Local_Control => 16#0800_0021#);


   package Alarms is new ACO.Utils.Generic_Alarms
      (Configuration.Max_Nof_Simultaneous_SDO_Sessions);

   type Alarm
      (SDO_Ref : access SDO'Class := null)
   is new Alarms.Alarm_Type with record
      Id : ACO.SDO_Sessions.Endpoint_Nr := ACO.SDO_Sessions.No_Endpoint_Id;
   end record;

   overriding
   procedure Signal
      (This  : access Alarm;
       T_Now : in     Ada.Real_Time.Time);

   type Alarm_Array is array (ACO.SDO_Sessions.Valid_Endpoint_Nr'Range)
      of aliased Alarm;

   type SDO
      (Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
       --Wrap    : not null access SDO_Wrapper_Base'Class)
   is abstract new Protocol (Od) with record
      Sessions : ACO.SDO_Sessions.Session_Manager;
      Timers   : Alarms.Alarm_Manager;
      Alarms   : Alarm_Array :=
         (others => (SDO'Access, ACO.SDO_Sessions.No_Endpoint_Id));
   end record;

   overriding
   procedure On_State_Change
     (This     : in out SDO;
      Previous : in     ACO.States.State;
      Current  : in     ACO.States.State);

   procedure Handle_Message
      (This     : in out SDO;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type) is null;

   procedure SDO_Log
     (This    : in out SDO;
      Level   : in     ACO.Log.Log_Level;
      Message : in     String);

   procedure Start_Alarm
      (This : in out SDO;
       Id   : in     ACO.SDO_Sessions.Valid_Endpoint_Nr);

   procedure Stop_Alarm
      (This : in out SDO;
       Id   : in     ACO.SDO_Sessions.Valid_Endpoint_Nr);

   procedure Abort_All
      (This     : in out SDO;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Write
      (This    : in out SDO;
       Index   : in     ACO.OD_Types.Entry_Index;
       Data    : in     ACO.Messages.Data_Array;
       Error   :    out Error_Type);

   procedure Send_SDO
      (This     : in out SDO'Class;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type;
       Raw_Data : in     ACO.Messages.Msg_Data);

   procedure Send_Abort
      (This     : in out SDO;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type;
       Error    : in     Error_Type;
       Index    : in     ACO.OD_Types.Entry_Index := (0,0));

end ACO.Protocols.Service_Data;
