with ACO.Utils.DS.Generic_Protected_Queue;
with ACO.Configuration;

package ACO.Messages.Buffer is new ACO.Utils.DS.Generic_Protected_Queue
  (Item_Type         => Message,
   Maximum_Nof_Items => ACO.Configuration.Messages_Buffer_Size);
