with ACO.Messages;

private with ACO.Configuration;
private with ACO.Utils.Generic_Simple_List;

package ACO.SDO_Sessions is

   pragma Preelaborate;

   use ACO.Messages;

   type Transfer_State is
      (Initiated, Downloading, Uploading, Block_Downloading, Block_Uploading);

   type Endpoint_Role is (Client, Server);

   subtype Endpoint_Nr is Integer range -1 .. Integer'Last;

   No_Endpoint_Id : constant Endpoint_Nr := Endpoint_Nr'First;

   type SDO_CAN_Id is record
      C2S : Id_Type;
      S2C : Id_Type;
   end record;

   type SDO_CAN_Id_Array is array (Natural range <>) of SDO_CAN_Id;

   type Endpoint_Type is record
      Id     : Endpoint_Nr := No_Endpoint_Id;
      Role   : Endpoint_Role;
      CAN_Id : SDO_CAN_Id;
   end record;

   No_Endpoint : constant Endpoint_Type :=
      (Id => No_Endpoint_Id, Role => Client, CAN_Id => (0, 0));

   type SDO_Session is record
      Endpoint  : Endpoint_Type;
      State     : Transfer_State;
      Nof_Bytes : Natural;
      Count     : Natural;
   end record;

   function "=" (R, L : SDO_Session) return Boolean is
      (R.Endpoint.Id = L.Endpoint.Id);


   function Create (Endpoint : Endpoint_Type) return SDO_Session;

   type Session_List is tagged limited private;

   function Get_Endpoint
      (CAN_Id         : Id_Type;
       Client_CAN_Ids : SDO_CAN_Id_Array;
       Server_CAN_Ids : SDO_CAN_Id_Array)
       return Endpoint_Type;

   procedure Put
      (This    : in out Session_List;
       Session : in     SDO_Session);

--     function Get
--        (This : Session_List;
--         Id   : Endpoint_Nr)
--         return SDO_Session;

   function Is_Full (This : Session_List) return Boolean;

   function In_List
      (This     : Session_List;
       Endpoint : Endpoint_Type)
       return Boolean;

   function Tx_CAN_Id (Endpoint : Endpoint_Type) return Id_Type is
      (case Endpoint.Role is
          when Server => Endpoint.CAN_Id.S2C,
          when Client => Endpoint.CAN_Id.C2S);

private

   package SL is new ACO.Utils.Generic_Simple_List
      (Element_Type     => SDO_Session,
       "="              => "=",
       Max_Nof_Elements => ACO.Configuration.Max_Nof_Simultaneous_SDO_Sessions);

   type Session_List is tagged limited record
      List : SL.List_Type;
   end record;

end ACO.SDO_Sessions;
