with ACO.Messages;
with ACO.OD_Types;
with ACO.Configuration;

private with ACO.Utils.Generic_Ring_Buffer;

package ACO.SDO_Sessions is

   pragma Preelaborate;

   use ACO.Messages;
   use ACO.Configuration;

   type Session_Manager is tagged limited private;

   type Services is
      (None,
       Download,
       Upload,
       Block_Download,
       Block_Upload);

   type Endpoint_Role is (Client, Server);

   subtype Endpoint_Nr is Integer range -1 .. Integer'Last;

   No_Endpoint_Id : constant Endpoint_Nr := Endpoint_Nr'First;

   subtype Valid_Endpoint_Nr is Endpoint_Nr range
      Endpoint_Nr'First + 1 ..  Max_Nof_Simultaneous_SDO_Sessions;

   type SDO_Parameters is record
      CAN_Id_C2S : Id_Type := 0;
      CAN_Id_S2C : Id_Type := 0;
      Node       : Node_Nr;
   end record;

   type SDO_Parameter_Array is array (Natural range <>) of SDO_Parameters;

   type Endpoint_Type is record
      Id         : Endpoint_Nr   := No_Endpoint_Id;
      Role       : Endpoint_Role := Client;
      Parameters : SDO_Parameters;
   end record;

   No_Endpoint : Endpoint_Type;

   function Tx_CAN_Id (Endpoint : Endpoint_Type) return Id_Type is
      (case Endpoint.Role is
          when Server => Endpoint.Parameters.CAN_Id_S2C,
          when Client => Endpoint.Parameters.CAN_Id_C2S);

   function Image (Endpoint : Endpoint_Type) return String;

   function Get_Endpoint
      (Rx_CAN_Id         : Id_Type;
       Client_Parameters : SDO_Parameter_Array;
       Server_Parameters : SDO_Parameter_Array)
       return Endpoint_Type;

   function Get_Endpoint
      (Server_Node       : Node_Nr;
       Client_Parameters : SDO_Parameter_Array;
       Server_Parameters : SDO_Parameter_Array)
       return Endpoint_Type;

   type SDO_Status is
      (Pending,
       Complete,
       Error);

   type SDO_Session (Service : Services := None) is record
      Endpoint : Endpoint_Type := No_Endpoint;
      Index    : ACO.OD_Types.Entry_Index;
      Toggle   : Boolean := False;
      Status   : SDO_Status := Pending;
   end record;

   function Is_Complete (Session : SDO_Session) return Boolean is
      (Session.Status = Complete);

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
       Data : in     Data_Array);

   function Length_Buffer
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return Natural;

   procedure Get_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr;
       Data :    out Data_Array)
      with Pre => Data'Length <= This.Length_Buffer (Id);

   function Peek_Buffer
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return Data_Array;

private

   package RB is new ACO.Utils.Generic_Ring_Buffer
      (Item_Type     => ACO.Messages.Data_Type,
       Max_Nof_Items => Max_Data_SDO_Transfer_Size);

   type Session_Array is array (Endpoint_Nr range <>) of SDO_Session;

   type Buffer_Array is array (Endpoint_Nr range <>) of RB.Ring_Buffer;

   type Session_Manager is tagged limited record
      List    : Session_Array (Valid_Endpoint_Nr'Range);
      Buffers : Buffer_Array (Valid_Endpoint_Nr'Range);
   end record;

   function Get_Matching_Endpoint
      (Match_Condition   : not null access
          function (P : SDO_Parameters; Is_Server : Boolean) return Boolean;
       Client_Parameters : SDO_Parameter_Array;
       Server_Parameters : SDO_Parameter_Array)
       return Endpoint_Type;

end ACO.SDO_Sessions;
