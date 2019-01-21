package body ACO.SDO_Sessions is

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
       Data : in     ACO.Messages.Data_Array)
   is
   begin
      This.Buffers (Id).Put (Q.Item_Array (Data));
   end Put_Buffer;

   procedure Get_Buffer
      (This : in out Session_Manager;
       Id   : in     Valid_Endpoint_Nr;
       Data :    out ACO.Messages.Data_Array)
   is
   begin
      This.Buffers (Id).Get (Q.Item_Array (Data));
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
       return ACO.Messages.Data_Array
   is
   begin
      if This.Buffers (Id).Is_Empty then
         return ACO.Messages.Empty_Data;
      else
         return ACO.Messages.Data_Array (Q.Item_Array'(This.Buffers (Id).Peek));
      end if;
   end Peek_Buffer;

   function Image (Endpoint : Endpoint_Type) return String
   is
   begin
      return "(" & Endpoint.Role'Img & "," & Endpoint.Id'Img & ")";
   end Image;

end ACO.SDO_Sessions;
