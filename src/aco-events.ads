with ACO.Utils.Generic_Pubsub;
with ACO.States;
with ACO.OD_Types;

package ACO.Events is

   pragma Preelaborate;

   package Node_State_Pubsub is new ACO.Utils.Generic_Pubsub
      (Item_Type           => ACO.States.State_Transition,
       Max_Nof_Subscribers => 5);

   package Natural_Pubsub is new ACO.Utils.Generic_Pubsub
      (Item_Type           => Natural,
       Max_Nof_Subscribers => 5);

   package Entry_Update_Pack is new ACO.Utils.Generic_Pubsub
      (Item_Type           => ACO.OD_Types.Entry_Index,
       Max_Nof_Subscribers => 5);


   type Node_State_Change_Publisher is
      new Node_State_Pubsub.Pub with null record;

   type Heartbeat_Consumer_Change_Publisher is
      new Natural_Pubsub.Pub with null record;

   type Heartbeat_Producer_Change_Publisher is
      new Natural_Pubsub.Pub with null record;

   type Sync_Producer_Change_Publisher is
      new Natural_Pubsub.Pub with null record;

   type Entry_Update_Publisher is
      new Entry_Update_Pack.Pub with null record;


   type Event_Manager is tagged limited record
      Entry_Updated             : Entry_Update_Publisher;
      Node_State_Change         : Node_State_Change_Publisher;
      Heartbeat_Consumer_Change : Heartbeat_Consumer_Change_Publisher;
      Heartbeat_Producer_Change : Heartbeat_Producer_Change_Publisher;
      Sync_Producer_Change      : Sync_Producer_Change_Publisher;
   end record;

end ACO.Events;
