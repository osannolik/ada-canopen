package ACO.Protocols.Service_Data.Servers is

   type Server
      (Handler : not null access ACO.CANopen.Handler'Class;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new SDO with private;

private

   type Server
      (Handler : not null access ACO.CANopen.Handler'Class;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new SDO (Handler, Od) with record
      null;
   end record;

   overriding
   procedure Handle_Message
      (This     : in out Server;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Upload_Init
      (This     : in out Server;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Upload_Segment
      (This     : in out Server;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Download_Init
      (This     : in out Server;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

   procedure Download_Segment
      (This     : in out Server;
       Msg      : in     Message;
       Endpoint : in     Endpoint_Type);

end ACO.Protocols.Service_Data.Servers;
