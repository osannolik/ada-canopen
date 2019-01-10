with ACO.OD_Types;

package ACO.Protocols.Service_Data.Clients is

   type Client
      (Handler : not null access ACO.CANopen.Handler'Class;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new SDO with private;

   procedure Write_Remote_Entry
      (This        : in out Client;
       Node        : in     ACO.Messages.Node_Nr;
       Index       : in     ACO.OD_Types.Object_Index;
       Subindex    : in     ACO.OD_Types.Object_Subindex;
       An_Entry    : in     ACO.OD_Types.Entry_Base'Class;
       Endpoint_Id :    out ACO.SDO_Sessions.Endpoint_Nr);

   procedure Read_Remote_Entry
      (This        : in out Client;
       Node        : in     ACO.Messages.Node_Nr;
       Index       : in     ACO.OD_Types.Object_Index;
       Subindex    : in     ACO.OD_Types.Object_Subindex;
       Endpoint_Id :    out ACO.SDO_Sessions.Endpoint_Nr);

   procedure Get_Read_Entry
      (This        : in out Client;
       Endpoint_Id : in     ACO.SDO_Sessions.Valid_Endpoint_Nr;
       Read_Entry  : in out ACO.OD_Types.Entry_Base'Class)
      with Pre => This.Is_Complete (Endpoint_Id);

private

   type Client
      (Handler : not null access ACO.CANopen.Handler'Class;
       Od      : not null access ACO.OD.Object_Dictionary'Class)
   is new SDO (Handler, Od) with record
      null;
   end record;

   overriding
   procedure Handle_Message
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Download_Init
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Download_Segment
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Upload_Init
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Upload_Segment
      (This     : in out Client;
       Msg      : in     ACO.Messages.Message;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type);

   procedure Send_Buffered
      (This     : in out Client;
       Endpoint : in     ACO.SDO_Sessions.Endpoint_Type;
       Toggle   : in     Boolean);

end ACO.Protocols.Service_Data.Clients;
