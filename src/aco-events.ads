with ACO.Utils.Generic_Event;
with ACO.States;
with ACO.OD_Types;
with ACO.Configuration;

package ACO.Events is

   pragma Preelaborate;

   use ACO.Configuration;

   package Node_State is new ACO.Utils.Generic_Event
      (Item_Type           => ACO.States.State_Transition,
       Max_Nof_Subscribers => Max_Nof_Node_State_Change_Subscribers);

   package Entry_Update is new ACO.Utils.Generic_Event
      (Item_Type           => ACO.OD_Types.Entry_Index,
       Max_Nof_Subscribers => Max_Nof_Entry_Update_Subscribers);

   type Event_Manager is tagged limited record
      Node_State_Modified : Node_State.Event_Publisher;
      Slave_State_Change  : Node_State.Event_Publisher;
      Entry_Updated       : Entry_Update.Event_Publisher;
   end record;

   procedure Process
      (This : in out Event_Manager);

end ACO.Events;
