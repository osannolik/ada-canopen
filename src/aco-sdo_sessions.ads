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
      Endpoint_Nr'First ..  Max_Nof_Simultaneous_SDO_Sessions;

   type SDO_CAN_Id is record
      C2S : Id_Type := 0;
      S2C : Id_Type := 0;
   end record;

   type SDO_CAN_Id_Array is array (Natural range <>) of SDO_CAN_Id;

   type Endpoint_Type is record
      Id     : Endpoint_Nr   := No_Endpoint_Id;
      Role   : Endpoint_Role := Client;
      CAN_Id : SDO_CAN_Id;
   end record;

   No_Endpoint : Endpoint_Type;

   function Tx_CAN_Id (Endpoint : Endpoint_Type) return Id_Type is
      (case Endpoint.Role is
          when Server => Endpoint.CAN_Id.S2C,
          when Client => Endpoint.CAN_Id.C2S);

   function Get_Endpoint
      (CAN_Id         : Id_Type;
       Client_CAN_Ids : SDO_CAN_Id_Array;
       Server_CAN_Ids : SDO_CAN_Id_Array)
       return Endpoint_Type;


   type SDO_Session (Service : Services := None) is record
      Endpoint : Endpoint_Type := No_Endpoint;

      case Service is
         when None | Upload | Block_Download | Block_Upload =>
            null;

         when Download =>
            Index     : ACO.OD_Types.Entry_Index;
            Nof_Bytes : Natural := 0;
            Count     : Natural := 0;
            Toggle    : Boolean := False;
      end case;
   end record;

   function Create_Download
      (Endpoint  : Endpoint_Type;
       Index     : ACO.OD_Types.Entry_Index;
       Nof_Bytes : Natural)
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

end ACO.SDO_Sessions;
