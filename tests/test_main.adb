with AUnit.Reporter.Text;
with AUnit.Run;
with Utils_Suite;

procedure Test_Main is
   procedure Runner is new AUnit.Run.Test_Runner (Utils_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_Main;
