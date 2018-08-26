with ACO.Configuration;
with ACO.Utils.Generic_Buffer;

package ACO.Messages.Buffer is new ACO.Utils.Generic_Buffer
   (Item_Type     => Message,
    Max_Nof_Items => ACO.Configuration.Received_Messages_Buffer_Size);
