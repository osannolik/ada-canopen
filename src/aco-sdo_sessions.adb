package body ACO.SDO_Sessions is

   function Get_Endpoint
      (Rx_CAN_Id         : Id_Type;
       Client_Parameters : SDO_Parameter_Array;
       Server_Parameters : SDO_Parameter_Array)
       return Endpoint_Type
   is
      function Is_Rx_CAN_Id
         (P : SDO_Parameters; Is_Server : Boolean) return Boolean
      is
         (if Is_Server then P.CAN_Id_C2S = Rx_CAN_Id else P.CAN_Id_S2C = Rx_CAN_Id);
   begin
      return Get_Matching_Endpoint
         (Is_Rx_CAN_Id'Access, Client_Parameters, Server_Parameters);
   end Get_Endpoint;

   function Get_Endpoint
      (Server_Node       : Node_Nr;
       Client_Parameters : SDO_Parameter_Array;
       Server_Parameters : SDO_Parameter_Array)
       return Endpoint_Type
   is
      function Is_Server_Node_Of_Client
         (P : SDO_Parameters; Is_Server : Boolean) return Boolean
      is
         (if Is_Server then False else P.Node = Server_Node);
   begin
      return Get_Matching_Endpoint
         (Is_Server_Node_Of_Client'Access, Client_Parameters, Server_Parameters);
   end Get_Endpoint;

   function Get_Matching_Endpoint
      (Match_Condition   : not null access
          function (P : SDO_Parameters; Is_Server : Boolean) return Boolean;
       Client_Parameters : SDO_Parameter_Array;
       Server_Parameters : SDO_Parameter_Array)
       return Endpoint_Type
   is
      I : Endpoint_Nr := Valid_Endpoint_Nr'First;
   begin
      for P of Client_Parameters loop
         if Match_Condition (P, False) then
            return (Id => I, Role => Client, Parameters => P);
         end if;
         I := Endpoint_Nr'Succ (I);
      end loop;

      for P of Server_Parameters loop
         if Match_Condition (P, True) then
            return (Id => I, Role => Server, Parameters => P);
         end if;
         I := Endpoint_Nr'Succ (I);
      end loop;

      return No_Endpoint;
   end Get_Matching_Endpoint;

   function Create_Download
      (Endpoint : Endpoint_Type;
       Index    : ACO.OD_Types.Entry_Index)
       return SDO_Session
   is
      ((Service  => Download,
        Endpoint => Endpoint,
        Index    => Index,
        Toggle   => False,
        Status   => Pending));

   function Create_Upload
      (Endpoint : Endpoint_Type;
       Index    : ACO.OD_Types.Entry_Index)
       return SDO_Session
   is
      ((Service  => Upload,
        Endpoint => Endpoint,
        Index    => Index,
        Toggle   => False,
        Status   => Pending));

   procedure Put
      (This    : in out Session_Manager;
       Session : in     SDO_Session)
   is
   begin
      This.List (Session.Endpoint.Id) := Session;
   end Put;

   function Get
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return SDO_Session
   is
      (This.List (Id));

   function Service
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return Services
   is
      (This.List (Id).Service);

   procedure Clear
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr)
   is
   begin
      This.List (Id) := (None, No_Endpoint, (0,0), False, Pending);
      This.Buffers (Id).Flush;
   end Clear;

   procedure Clear_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr)
   is
   begin
      This.Buffers (Id).Flush;
   end Clear_Buffer;

   procedure Put_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr;
       Data : in     Data_Array)
   is
   begin
      This.Buffers (Id).Put (RB.Item_Array (Data));
   end Put_Buffer;

   procedure Get_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr;
       Data :    out Data_Array)
   is
   begin
      This.Buffers (Id).Get (RB.Item_Array (Data));
   end Get_Buffer;

   function Length_Buffer
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return Natural
   is
   begin
      return This.Buffers (Id).Length;
   end Length_Buffer;

   function Peek_Buffer
      (This : Session_Manager;
       Id   : Valid_Endpoint_Nr)
       return Data_Array
   is
   begin
      if This.Buffers (Id).Is_Empty then
         return Empty_Data;
      else
         return Data_Array (RB.Item_Array'(This.Buffers (Id).Peek));
      end if;
   end Peek_Buffer;

   function Image (Endpoint : Endpoint_Type) return String
   is
   begin
      return "(" & Endpoint.Role'Img & "," & Endpoint.Id'Img & ")";
   end Image;

end ACO.SDO_Sessions;
