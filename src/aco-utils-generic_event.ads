with System;
with ACO.Utils.Generic_Pubsub;

private with ACO.Utils.DS.Generic_Protected_Queue;

generic
   type Item_Type is private;
   Max_Nof_Subscribers   : Positive;
   Max_Nof_Queued_Events : Positive;

package ACO.Utils.Generic_Event is

   pragma Preelaborate;

   package PS is new ACO.Utils.Generic_Pubsub
      (Item_Type           => Item_Type,
       Max_Nof_Subscribers => Max_Nof_Subscribers);

   subtype Subscriber is PS.Sub;

   type Subscriber_Access is access all Subscriber'Class;

   type Event_Publisher is limited new PS.Pub with null record;

   type Queued_Event_Publisher
      (Priority_Ceiling : System.Priority)
   is new Event_Publisher with private;

   function Events_Waiting
      (This : Queued_Event_Publisher)
       return Natural;

   procedure Put
      (This : in out Queued_Event_Publisher;
       Data : in     Item_Type)
      with Pre => This.Events_Waiting < Max_Nof_Queued_Events;

   procedure Process
      (This : in out Queued_Event_Publisher);

private

   package PQ is new ACO.Utils.DS.Generic_Protected_Queue
     (Item_Type         => Item_Type,
      Maximum_Nof_Items => Max_Nof_Queued_Events);

   type Queued_Event_Publisher
      (Priority_Ceiling : System.Priority)
   is new Event_Publisher with record
      Queue : PQ.Protected_Queue (Priority_Ceiling);
   end record;

end ACO.Utils.Generic_Event;
