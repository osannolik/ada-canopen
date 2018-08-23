with Ada.Streams;
with Interfaces;
with System;

package ACO.Messages is

   pragma Preelaborate;

   use Interfaces;

   type Id_Type is mod 2**11;

   type Function_Code is mod 2**4;

   type Node_Nr is mod 2**7;

   subtype Slave_Node_Nr is Node_Nr range 1 .. Node_Nr'Last;

   Broadcast_Id : constant Node_Nr := 0;

   type CAN_Id_Type (As_Id : Boolean := False) is record
      case As_Id is
         when True =>
            Id : Id_Type;
         when False =>
            Code : Function_Code;
            Node : Node_Nr;
      end case;
   end record
     with Unchecked_Union, Size => 11, Bit_Order => System.Low_Order_First;

   for CAN_Id_Type use record
      Id   at 0 range 0 .. 10;
      Code at 0 range 7 .. 10;
      Node at 0 range 0 .. 6;
   end record;

   Max_Data_Length : constant := 8;

   subtype Data_Length is Natural range 0 .. Max_Data_Length;

   subtype Data_Type is Unsigned_8;

   type Data_Array is array (Natural range <>) of Data_Type;

   Empty_Data : Data_Array (1 .. 0);

   subtype Msg_Data is Data_Array (0 .. Max_Data_Length - 1);

   type Message is record
      CAN_Id : CAN_Id_Type;
      RTR    : Boolean;
      Length : Data_Length;
      Data   : Msg_Data;
   end record;

   function CAN_Id (Msg : Message) return Id_Type with Inline;

   function Func_Code (Msg : Message) return Function_Code with Inline;

   function Node_Id (Msg : Message) return Node_Nr with Inline;

   function Create (CAN_Id : Id_Type;
                    RTR    : Boolean;
                    DLC    : Data_Length;
                    Data   : Msg_Data)
                    return Message;

   function Create (CAN_Id : Id_Type;
                    RTR    : Boolean;
                    Data   : Data_Array)
                    return Message
      with Pre => Data'Length <= Msg_Data'Length;

   function Create (Code : Function_Code;
                    Node : Node_Nr;
                    RTR  : Boolean;
                    Data : Data_Array)
                    return Message
      with Pre => Data'Length <= Msg_Data'Length;

   procedure Print
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Msg    : in  Message);

   function Image (Msg : Message) return String;

private

   Fill_Data : constant Data_Type := 0;

end ACO.Messages;
