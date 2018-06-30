with AUnit;
with AUnit.Simple_Test_Cases;

package ACO.Protocols.Network_Management.Test is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure Run_Test (T : in out Test);

end ACO.Protocols.Network_Management.Test;
