with Generic_Sorted_List_Test;

package body Utils_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (new Generic_Sorted_List_Test.Test);
      return Ret;
   end Suite;

end Utils_Suite;
