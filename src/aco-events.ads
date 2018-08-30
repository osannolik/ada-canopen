with ACO.Utils.Generic_Pubsub;
with ACO.States;
with ACO.OD_Types;
with ACO.Configuration;

package ACO.Events is

   pragma Preelaborate;

   package Node_State_Pubsub is new ACO.Utils.Generic_Pubsub
      (Item_Type           => ACO.States.State_Transition,
       Max_Nof_Subscribers => ACO.Configuration.Max_Nof_Node_State_Change_Subscribers);

   package Entry_Update_Pack is new ACO.Utils.Generic_Pubsub
      (Item_Type           => ACO.OD_Types.Entry_Index,
       Max_Nof_Subscribers => ACO.Configuration.Max_Nof_Entry_Update_Subscribers);


   type Node_State_Change_Publisher is
      new Node_State_Pubsub.Pub with null record;

   type Entry_Update_Publisher is
      new Entry_Update_Pack.Pub with null record;


   type Event_Manager is tagged limited record
      Entry_Updated     : Entry_Update_Publisher;
      Node_State_Change : Node_State_Change_Publisher;
      Slave_State_Change : Node_State_Change_Publisher;
   end record;

end ACO.Events;
