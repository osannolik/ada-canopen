with Ada.Streams;
with ACO.Drivers;
with ACO.Messages;

private with ACO.Loggers;
private with ACO.Protocols.Network_Management;
private with ACO.Protocols.Synchronization;
private with ACO.Protocols.Emergency;

package ACO.Nodes is
   use ACO.Messages;

   type Node is tagged limited private;

   procedure Initialize
     (This          : in out Node;
      Driver        : in     ACO.Drivers.Driver_Access;
      Logger_Stream : access Ada.Streams.Root_Stream_Type'Class := null);

   procedure Dispatch
     (This : in out Node;
      Msg  : in     Message);

   task type Receiver_Task (This : not null access Node'Class);

private

   type Node is tagged limited record
      Driver    : ACO.Drivers.Driver_Access;
      Log       : ACO.Loggers.Logger;
      Log_Level : ACO.Loggers.Log_Level := ACO.Loggers.Debug;
      NMT       : ACO.Protocols.Network_Management.NMT;
      SYNC      : ACO.Protocols.Synchronization.Sync;
      EMCY      : ACO.Protocols.Emergency.Emcy;
   end record;

end ACO.Nodes;
