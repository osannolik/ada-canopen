with Generic_Table_Test;
with Generic_Collection_Test;
with Remote_Node_Test;

package body Unit_Tests is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (new Generic_Table_Test.Test);
      Ret.Add_Test (new Generic_Collection_Test.Test);
      Ret.Add_Test (new Remote_Node_Test.Test);
      return Ret;
   end Suite;

end Unit_Tests;
