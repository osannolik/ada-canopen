with AUnit.Assertions; use AUnit.Assertions;

with ACO.OD.Example;

package body ACO.Protocols.Network_Management.Test is
   pragma Assertion_Policy (Check);

   use ACO.States;

   OD_Data : aliased ACO.OD.Example.Dictionary_Data;
   OD      : aliased ACO.OD.Object_Dictionary (OD_Data'Access);

   Expected_Transitions : constant array (State, State) of State :=
   --  Current    Request: Initializing,    Pre_Operational, Operational,     Stopped,         Unknown_State
      (Initializing    => (Pre_Operational, Pre_Operational, Pre_Operational, Pre_Operational, Pre_Operational),
       Pre_Operational => (Pre_Operational, Pre_Operational, Operational,     Stopped,         Unknown_State),
       Operational     => (Pre_Operational, Pre_Operational, Operational,     Stopped,         Unknown_State),
       Stopped         => (Pre_Operational, Pre_Operational, Operational,     Stopped,         Unknown_State),
       Unknown_State   => (Pre_Operational, Pre_Operational, Operational,     Stopped,         Unknown_State));

   Cmds : constant array (Natural range <>) of Commands.Cmd_Spec_Type :=
      (Commands.Start,
       Commands.Stop,
       Commands.Pre_Op,
       Commands.Reset_Node,
       Commands.Reset_Communication);

   procedure Set_Actual_State
     (N : in out NMT;
      S : in     ACO.States.State)
   is
   begin
      N.Od.Set_Node_State (S);
   end Set_Actual_State;

   function Is_Current_State
     (N : in out NMT;
      S : in     ACO.States.State)
      return Boolean
   is
      (N.Od.Get_Node_State = S);

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Network Management Test");
   end Name;

   function Make_Message (Id : Node_Nr; Cs : Commands.Cmd_Spec_Type)
                                return Message
   is
   begin
      return Create (CAN_Id => NMT_CAN_Id,
                     RTR    => False,
                     Data   => (Data_Type (Cs), Data_Type (Id)));
   end Make_Message;

   procedure Message_Command_Test is
      N : NMT (OD'Access);
      Id : constant Node_Nr := 1;
      Incorrect_Id : constant Node_Nr := Id + 1;

      use Commands;

      function To_Request (Cmd : Commands.Cmd_Spec_Type) return State
      is
         (case Cmd is
             when Start               => Operational,
             when Stop                => Stopped,
             when Pre_Op              => Pre_Operational,
             when Reset_Node |
                  Reset_Communication => Initializing,
             when others =>
                raise Constraint_Error with "Invalid command value");

      State_Request  : State;
      Expected_State : State;
   begin
      for From in State loop
         if From /= Initializing and From /= Unknown_State then
            for Cmd of Cmds loop
               State_Request := To_Request (Cmd);
               Expected_State := Expected_Transitions (From, State_Request);

               Set_Actual_State (N, From);
               N.Message_Received (Make_Message (Id, Cmd), Id);
               Assert
                  (Is_Current_State (N, Expected_State),
                   From'Img & "+" & State_Request'Img &
                      "=" & N.Od.Get_Node_State'Img & "/=" & Expected_State'Img);

               --  Again with Broadcast_Id
               Set_Actual_State (N, From);
               N.Message_Received (Make_Message (Broadcast_Id, Cmd), Id);

               Assert
                  (Is_Current_State (N, Expected_State),
                   From'Img & "+" & State_Request'Img &
                      "=" & N.Od.Get_Node_State'Img & "/=" & Expected_State'Img);

               --  Again with Incorrect_Id, expect no ch
               Set_Actual_State (N, From);
               N.Message_Received (Make_Message (Incorrect_Id, Cmd), Id);
               Expected_State := From;

               Assert
                  (Is_Current_State (N, Expected_State),
                   From'Img & "+" & State_Request'Img &
                      "=" & N.Od.Get_Node_State'Img & "/=" & Expected_State'Img);
            end loop;
         end if;
      end loop;
   end Message_Command_Test;

   procedure Message_Ignored_Test is
      N : NMT (OD'Access);
      Id : constant Node_Nr := 1;

      Invalid_Command_1 : constant Message :=
         Create (CAN_Id => NMT_CAN_Id,
                 RTR    => False,
                 Data   => Empty_Data);

      Invalid_Command_2 : constant Message := Make_Message (Id, 0);
   begin
      Set_Actual_State (N, Pre_Operational);
      N.Message_Received (Invalid_Command_1, Id);
      Assert (Is_Current_State (N, Pre_Operational),
              "State changed on invalid command message (empty data)");

      Set_Actual_State (N, Pre_Operational);
      N.Message_Received (Invalid_Command_2, Id);
      Assert (Is_Current_State (N, Pre_Operational),
              "State changed on invalid command message (not an id)");

      for Cmd of Cmds loop
         Set_Actual_State (N, Initializing);
         N.Message_Received (Make_Message (Id, Cmd), Id);
         Assert (Is_Current_State (N, Initializing),
                 "State changed due to message when in " & Initializing'Img);

         Set_Actual_State (N, Unknown_State);
         N.Message_Received (Make_Message (Id, Cmd), Id);
         Assert (Is_Current_State (N, Unknown_State),
                 "State changed due to message when in " & Unknown_State'Img);
      end loop;
   end Message_Ignored_Test;

   procedure Allowed_Transition_Test is
      N : NMT (OD'Access);
   begin
      for From in State loop
         for Request in State loop
            Set_Actual_State (N, From);
            N.Set_State (Request);

            Assert (Is_Current_State (N, Expected_Transitions (From, Request)),
                    "Transition " & From'Img & "=>" & Request'Img & " incorrect");
         end loop;
      end loop;
   end Allowed_Transition_Test;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Allowed_Transition_Test;
      Message_Command_Test;
      Message_Ignored_Test;
   end Run_Test;

end ACO.Protocols.Network_Management.Test;
