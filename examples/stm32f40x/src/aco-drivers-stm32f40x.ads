with Ada.Interrupts;
with STM32.CAN;
with System;

package ACO.Drivers.Stm32f40x is
   use STM32.CAN;

   type CAN_Driver
     (Device : not null access CAN_Controller)
   is new Driver with private;

   overriding
   procedure Receive_Message_Blocking
     (This : in out CAN_Driver;
      Msg  :    out ACO.Messages.Message);

   overriding
   procedure Send_Message
     (This : in out CAN_Driver;
      Msg  : in     ACO.Messages.Message);

   overriding
   procedure Initialize
     (This : in out CAN_Driver);

   overriding
   procedure Finalize
     (This : in out CAN_Driver) is null;

   overriding
   function Is_Message_Pending
     (This : CAN_Driver)
      return Boolean;

   overriding
   function Current_Time
     (This : CAN_Driver)
      return Ada.Real_Time.Time;

   package CAN_ISR is
      function Tx_Interrupt_Id (Device : not null access CAN_Controller)
                                return Ada.Interrupts.Interrupt_ID;
      function Rx0_Interrupt_Id (Device : not null access CAN_Controller)
                                 return Ada.Interrupts.Interrupt_ID;
      function Rx1_Interrupt_Id (Device : not null access CAN_Controller)
                                 return Ada.Interrupts.Interrupt_ID;
      function SCE_Interrupt_Id (Device : not null access CAN_Controller)
                                 return Ada.Interrupts.Interrupt_ID;

      subtype Index is Natural range 0 .. 7;
      type Msg_Array is array (Index'Range) of CAN_Message;

      type Message_Buffer is record
         Buffer  : Msg_Array;
         Idx_New : Index := 0;
         Idx_Old : Index := 0;
      end record;

      protected type Controller (Device : not null access CAN_Controller) is

         pragma Interrupt_Priority (System.Interrupt_Priority'Last);

         procedure Transmit_Message
            (Message : in     CAN_Message;
             Success :    out Boolean);

         entry Receive_Message
            (Message : out CAN_Message);

         procedure Enable_Receiver
            (Fifo : in Fifo_Nr);

         function Is_Message_Pending
           return Boolean;

      private

         Tx_Buffer : Message_Buffer;
         Rx_Buffer : Message_Buffer;

         Is_Rx_Pending : Boolean := False;

         procedure Send;

         procedure IRQ_Handler;
         pragma Attach_Handler (IRQ_Handler, Tx_Interrupt_Id (Device));
         pragma Attach_Handler (IRQ_Handler, Rx0_Interrupt_Id (Device));
         pragma Attach_Handler (IRQ_Handler, Rx1_Interrupt_Id (Device));
         pragma Attach_Handler (IRQ_Handler, SCE_Interrupt_Id (Device));

      end Controller;

   private
      Max_Nof_Messages : constant Natural := Index'Last;

      function Nof_Messages (This : Message_Buffer) return Natural;

      procedure Get_Next_Message
         (This    : in out Message_Buffer;
          Message :    out CAN_Message)
         with Pre => Nof_Messages (This) > 0;

      procedure Put_Message
         (This    : in out Message_Buffer;
          Message : in     CAN_Message)
         with Pre => Nof_Messages (This) < Max_Nof_Messages;
   end CAN_ISR;

private

   type CAN_Driver
     (Device : not null access CAN_Controller)
   is new Driver with record
      Controller : CAN_ISR.Controller (Device);
   end record;

   function Convert (D : STM32.CAN.Message_Data)
                     return ACO.Messages.Data_Array
      with Inline;

   function Convert (D : ACO.Messages.Data_Array)
                     return STM32.CAN.Message_Data
      with Inline;

end ACO.Drivers.Stm32f40x;
