pragma Profile (Ravenscar);

with GNAT.Command_Line;
with App;

procedure Main
is
   function Run_Remote_Node
      return Boolean
   is
   begin
      loop
         case GNAT.Command_Line.Getopt ("r") is
            when 'r' =>
               return True;
            when others =>
               exit;
         end case;
      end loop;
      return False;
   end Run_Remote_Node;
begin
   if Run_Remote_Node then
      App.Run_Remote;
   else
      App.Run_Local;
   end if;
end Main;
