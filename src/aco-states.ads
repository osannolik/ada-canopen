with ACO.Messages;
with ACO.Configuration;

package ACO.States is

   pragma Preelaborate;

   use ACO.Configuration;

   --    Initialisation  = 0x00,
   --    Disconnected    = 0x01,
   --    Connecting      = 0x02,
   --    Preparing       = 0x02,
   --    Stopped         = 0x04,
   --    Operational     = 0x05,
   --    Pre_operational = 0x7F,
   --    Unknown_state   = 0x0F

   type State is
     (Initializing,
      Pre_Operational,
      Operational,
      Stopped,
      Unknown_State);

   type State_Transition is record
      Previous : State := Unknown_State;
      Current  : State := Unknown_State;
   end record;

   type State_Record is record
      Is_Used    : Boolean := False;
      Node_Id    : ACO.Messages.Node_Nr;
      Node_State : State := Unknown_State;
   end record;

   type State_Array is array (Positive range <>) of State_Record;

   type Node_States_List is tagged record
      Node_States : State_Array (1 .. Max_Nof_Heartbeat_Slaves);
   end record;

   function Is_Full (This : Node_States_List) return Boolean;

   function In_List
      (This    : Node_States_List;
       Node_Id : ACO.Messages.Node_Nr)
       return Boolean;

   procedure Clear
      (This : in out Node_States_List);

   procedure Add_Node
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr;
       Node_State : in     State)
      with Pre => not This.Is_Full;

   procedure Remove_Node
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr);

   procedure Set_Node_State
      (This       : in out Node_States_List;
       Node_Id    : in     ACO.Messages.Slave_Node_Nr;
       Node_State : in     State);

   function Get_Node_State
      (This    : Node_States_List;
       Node_Id : ACO.Messages.Slave_Node_Nr)
       return State;

end ACO.States;
