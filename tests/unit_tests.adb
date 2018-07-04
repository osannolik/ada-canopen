with Generic_Sorted_List_Test;
with ACO.Protocols.Network_Management.Test;
with ACO.Protocols.Error_Control.Test;
with ACO.Protocols.Synchronization.Test;

package body Unit_Tests is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (new Generic_Sorted_List_Test.Test);
      Ret.Add_Test (new ACO.Protocols.Network_Management.Test.Test);
      Ret.Add_Test (new ACO.Protocols.Error_Control.Test.Test);
      Ret.Add_Test (new ACO.Protocols.Synchronization.Test.Test);
      return Ret;
   end Suite;

end Unit_Tests;
