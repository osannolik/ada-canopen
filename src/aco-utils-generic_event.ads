with ACO.Configuration;
with ACO.Utils.Generic_Pubsub;

private with ACO.Utils.Generic_Protected_Buffer;

generic
   type Item_Type is private;
   Max_Nof_Subscribers : Positive;

package ACO.Utils.Generic_Event is

   pragma Preelaborate;

   use ACO.Configuration;

   package PS is new ACO.Utils.Generic_Pubsub (Item_Type, Max_Nof_Subscribers);

   subtype Subscriber is PS.Sub;

   type Subscriber_Access is access all Subscriber'Class;

   type Event_Publisher is limited new PS.Pub with private;

   function Events_Waiting
      (This : Event_Publisher)
       return Natural;

   procedure Put
      (This : in out Event_Publisher;
       Data : in     Item_Type)
      with Pre => This.Events_Waiting < Max_Nof_Event_Queue_Data_Items;

   procedure Process
      (This : in out Event_Publisher);

private

   package B is new ACO.Utils.Generic_Protected_Buffer
      (Item_Type     => Item_Type,
       Max_Nof_Items => Max_Nof_Event_Queue_Data_Items,
       Ceiling       => Event_Queue_Priority);

   type Event_Publisher is limited new PS.Pub with record
      Queue : B.Protected_Buffer;
   end record;

end ACO.Utils.Generic_Event;
