package ACO.Protocols.Service_Data.Servers is

   type Server
      (Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new SDO with private;

private

   type Server
      (Handler : not null access ACO.CANopen.Handler;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is abstract new SDO (Handler, Od) with null record;

   procedure Handle_Message
      (This     : in out Server;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Upload_Init
      (This     : in out Server;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Upload_Segment
      (This     : in out Server;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Download_Init
      (This     : in out Server;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Download_Segment
      (This     : in out Server;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

end ACO.Protocols.Service_Data.Servers;
