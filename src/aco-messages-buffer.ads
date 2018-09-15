with ACO.Configuration; use ACO.Configuration;
with ACO.Utils.Generic_Protected_Buffer;

package ACO.Messages.Buffer is new ACO.Utils.Generic_Protected_Buffer
   (Item_Type     => Message,
    Max_Nof_Items => Received_Messages_Buffer_Size,
    Ceiling       => Received_Messages_Buffer_Priority);
