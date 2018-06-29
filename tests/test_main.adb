with AUnit.Reporter.Text;
with AUnit.Run;
with Unit_Tests;

procedure Test_Main is
   procedure Runner is new AUnit.Run.Test_Runner (Unit_Tests.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_Main;
