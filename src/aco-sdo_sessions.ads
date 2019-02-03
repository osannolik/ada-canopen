with ACO.Messages;
with ACO.OD_Types;
with ACO.Configuration;

private with ACO.Utils.DS.Generic_Queue;

package ACO.SDO_Sessions is

   type Session_Manager is tagged limited private;

   type Services is
      (None,
       Download,
       Upload,
       Block_Download,
       Block_Upload);

   subtype Endpoint_Nr is Integer range -1 .. Integer'Last;

   No_Endpoint_Id : constant Endpoint_Nr := Endpoint_Nr'First;

   subtype Valid_Endpoint_Nr is Endpoint_Nr range
      No_Endpoint_Id + 1 ..  ACO.Configuration.Max_Nof_Simultaneous_SDO_Sessions;

   type SDO_Parameters is record
      CAN_Id_C2S : ACO.Messages.Id_Type := 0;
      CAN_Id_S2C : ACO.Messages.Id_Type := 0;
      Node       : ACO.Messages.Node_Nr := ACO.Messages.Not_A_Slave;
   end record;

   No_SDO_Parameters : constant SDO_Parameters :=
     (CAN_Id_C2S => 0,
      CAN_Id_S2C => 0,
      Node       => ACO.Messages.Not_A_Slave);

   type SDO_Parameter_Array is array (Natural range <>) of SDO_Parameters;

   type Endpoint_Type is record
      Id         : Endpoint_Nr   := No_Endpoint_Id;
      Parameters : SDO_Parameters;
   end record;

   No_Endpoint : constant Endpoint_Type :=
     (Id         => No_Endpoint_Id,
      Parameters => No_SDO_Parameters);

   function Image (Endpoint : Endpoint_Type) return String;

   type SDO_Status is
      (Pending,
       Complete,
       Error);

   Is_Complete : constant array (SDO_Status) of Boolean :=
      (Pending | Error => False,
       Complete        => True);

   subtype SDO_Result is SDO_Status range Complete .. Error;

   type SDO_Session (Service : Services := None) is record
      Endpoint : Endpoint_Type := No_Endpoint;
      Index    : ACO.OD_Types.Entry_Index := (0, 0);
      Toggle   : Boolean := False;
   end record;

   function Create_Download
      (Endpoint : Endpoint_Type;
       Index    : ACO.OD_Types.Entry_Index)
       return SDO_Session;

   function Create_Upload
      (Endpoint : Endpoint_Type;
       Index    : ACO.OD_Types.Entry_Index)
       return SDO_Session;

   function Get
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return SDO_Session;

   procedure Put
      (This    : in out Session_Manager;
       Session : in     SDO_Session);

   function Service
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return Services;

   procedure Clear
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr);

   procedure Clear_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr);

   procedure Put_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr;
       Data : in     ACO.Messages.Data_Array);

   function Length_Buffer
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return Natural;

   procedure Get_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr;
       Data :    out ACO.Messages.Data_Array)
      with Pre => Data'Length <= This.Length_Buffer (Id);

   function Peek_Buffer
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return ACO.Messages.Data_Array;

private

   package Q is new ACO.Utils.DS.Generic_Queue
      (Item_Type => ACO.Messages.Data_Type);

   type Session_Array is array (Endpoint_Nr range <>) of SDO_Session;

   type Buffer_Array is array (Endpoint_Nr range <>) of
      Q.Queue (Max_Nof_Items => ACO.Configuration.Max_SDO_Transfer_Size);

   type Session_Manager is tagged limited record
      List    : Session_Array (Valid_Endpoint_Nr'Range);
      Buffers : Buffer_Array (Valid_Endpoint_Nr'Range);
   end record;

end ACO.SDO_Sessions;
