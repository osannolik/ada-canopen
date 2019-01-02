with ACO.Configuration; use ACO.Configuration;
with ACO.Utils.DS.Generic_Protected_Queue;

package ACO.Messages.Buffer is new ACO.Utils.DS.Generic_Protected_Queue
   (Item_Type     => Message,
    Max_Nof_Items => Received_Messages_Buffer_Size,
    Ceiling       => Received_Messages_Buffer_Priority);
