with HAL;
with STM32.GPIO;
with STM32.Device;
with Ada.Interrupts.Names;
with Ada.Unchecked_Conversion;

package body ACO.Drivers.Stm32f40x is

   function Convert (D : STM32.CAN.Message_Data)
                     return ACO.Messages.Data_Array
   is
      subtype From is STM32.CAN.Message_Data (D'Range);
      subtype To is ACO.Messages.Data_Array (D'Range);
      function To_Data_Array is new Ada.Unchecked_Conversion (From, To);
   begin
      return To_Data_Array (D);
   end Convert;

   function Convert (D : ACO.Messages.Data_Array)
                     return STM32.CAN.Message_Data
   is
      subtype From is ACO.Messages.Data_Array (D'Range);
      subtype To is STM32.CAN.Message_Data (D'Range);
      function To_Message_Data is new Ada.Unchecked_Conversion (From, To);
   begin
      return To_Message_Data (D);
   end Convert;

   overriding
   procedure Await_Message
     (This : in out CAN_Driver;
      Msg  :    out Message)
   is
      Rx_Msg : CAN_Message;
   begin
      --  Suspend until new CAN message is received
      This.Controller.Receive_Message (Rx_Msg);

      Msg := Create (CAN_Id => Id_Type (Rx_Msg.Std_ID),
                     RTR    => Rx_Msg.Rtr,
                     Data   => Convert (Rx_Msg.Data));
   end Await_Message;

   overriding
   procedure Send_Message
     (This : in out CAN_Driver;
      Msg  : in     Message)
   is
      Success : Boolean;
      Tx_Msg : constant CAN_Message :=
         (Std_ID => Standard_Id (CAN_Id (Msg)),
          Ext_ID => 0,
          Ide    => False,
          Rtr    => Msg.RTR,
          Dlc    => Data_Length_Type (Msg.Length),
          Data   => Convert (Msg.Data));
   begin
      This.Controller.Transmit_Message
         (Message => Tx_Msg,
          Success => Success);
   end Send_Message;

   overriding
   procedure Initialize (This : in out CAN_Driver)
   is
      use STM32.GPIO;
      use STM32.Device;

      Tx_Pin : GPIO_Point renames PD1;
      Rx_Pin : GPIO_Point renames PD0;

      Fifo_X     : constant Fifo_Nr := FIFO_0;

      --  For now, set to 125 kbit/s
      --  see http://www.bittiming.can-wiki.info/#bxCAN
      Bit_Timing : constant Bit_Timing_Config :=
         (Resynch_Jump_Width => 1,
          Time_Segment_1     => 13,
          Time_Segment_2     => 2,
          Quanta_Prescaler   => 21);

      Mask_Allow_All : constant Filter_32 :=
         (Std_ID => 0,
          Ext_ID => 0,
          Ide    => False,
          Rtr    => False);

      Bank_Config : constant CAN_Filter_Bank :=
         (Bank_Nr         => 0,
          Activated       => True,
          Fifo_Assignment => Fifo_X,
          Filters         => (Mask32, (Mask_Allow_All, Mask_Allow_All)));
   begin
      Enable_Clock (Points => (Tx_Pin, Rx_Pin));
      Configure_IO (Rx_Pin, (Mode_AF, Pull_Up,  Push_Pull, Speed_50MHz, GPIO_AF_CAN1_9));
      Configure_IO (Tx_Pin, (Mode_AF, Floating, Push_Pull, Speed_50MHz, GPIO_AF_CAN1_9));

      Enable_Clock (This.Device.all);

      Reset (This.Device.all);

      Configure
         (This                => This.Device.all,
          Mode                => Normal,
          Time_Triggered      => False,
          Auto_Bus_Off        => False,
          Auto_Wakeup         => False,
          Auto_Retransmission => False,
          Rx_FIFO_Locked      => False,
          Tx_FIFO_Prio        => False,
          Timing_Config       => Bit_Timing);

      Set_Slave_Start_Bank (14);

      Configure_Filter
         (This        => This.Device.all,
          Bank_Config => Bank_Config);

      This.Controller.Enable_Receiver (Fifo_X);
   end Initialize;

   package body CAN_ISR is
      function Tx_Interrupt_Id
         (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
      is
      begin
         if Device = STM32.Device.CAN_1'Access then
            return Ada.Interrupts.Names.CAN1_TX_Interrupt;
         elsif Device = STM32.Device.CAN_2'Access then
            return Ada.Interrupts.Names.CAN2_TX_Interrupt;
         else
            raise Constraint_Error;
         end if;
      end Tx_Interrupt_Id;

      function Rx0_Interrupt_Id
         (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
      is
      begin
         if Device = STM32.Device.CAN_1'Access then
            return Ada.Interrupts.Names.CAN1_RX0_Interrupt;
         elsif Device = STM32.Device.CAN_2'Access then
            return Ada.Interrupts.Names.CAN2_RX0_Interrupt;
         else
            raise Constraint_Error;
         end if;
      end Rx0_Interrupt_Id;

      function Rx1_Interrupt_Id
         (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
      is
      begin
         if Device = STM32.Device.CAN_1'Access then
            return Ada.Interrupts.Names.CAN1_RX1_Interrupt;
         elsif Device = STM32.Device.CAN_2'Access then
            return Ada.Interrupts.Names.CAN2_RX1_Interrupt;
         else
            raise Constraint_Error;
         end if;
      end Rx1_Interrupt_Id;

      function SCE_Interrupt_Id
         (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
      is
      begin
         if Device = STM32.Device.CAN_1'Access then
            return Ada.Interrupts.Names.CAN1_SCE_Interrupt;
         elsif Device = STM32.Device.CAN_2'Access then
            return Ada.Interrupts.Names.CAN2_SCE_Interrupt;
         else
            raise Constraint_Error;
         end if;
      end SCE_Interrupt_Id;

      function Nof_Messages (This : Message_Buffer) return Natural
      is
      begin
         if This.Idx_New >= This.Idx_Old then
            return This.Idx_New - This.Idx_Old;
         else
            return Index'Last - This.Idx_Old + 1 + This.Idx_New - Index'First;
         end if;
      end Nof_Messages;

      procedure Get_Next_Message
         (This    : in out Message_Buffer;
          Message :    out CAN_Message)
      is
      begin
         Message := This.Buffer (This.Idx_Old);
         This.Idx_Old := (This.Idx_Old + 1) mod (Index'Last + 1);
      end Get_Next_Message;

      procedure Put_Message
         (This    : in out Message_Buffer;
          Message : in     CAN_Message)
      is
         Idx_Next : constant Index := (This.Idx_New + 1) mod (Index'Last + 1);
      begin
         This.Buffer (This.Idx_New) := Message;
         This.Idx_New := Idx_Next;
      end Put_Message;

      protected body Controller is

         procedure Transmit_Message
            (Message : in     CAN_Message;
             Success :    out Boolean)
         is
         begin
            Success := Nof_Messages (Tx_Buffer) < Max_Nof_Messages;
            if Success then
               Put_Message (Tx_Buffer, Message);
               Send;
            end if;
         end Transmit_Message;

         entry Receive_Message
            (Message : out CAN_Message) when New_Message
         is
         begin
            Message := Rx_Msg;
            New_Message := False;
         end Receive_Message;

         procedure Enable_Receiver
            (Fifo : in Fifo_Nr)
         is
            CAN : CAN_Controller renames Device.all;
         begin
            case Fifo is
               when FIFO_0 =>
                  Enable_Interrupts (CAN, FIFO_0_Message_Pending);
               when FIFO_1 =>
                  Enable_Interrupts (CAN, FIFO_1_Message_Pending);
            end case;
         end Enable_Receiver;

         procedure Send is
            CAN : CAN_Controller renames Device.all;
         begin
            while Nof_Messages (Tx_Buffer) > 0 loop
               declare
                  Message : CAN_Message;
                  Empty_Found : Boolean;
                  Mailbox : Mailbox_Type;
               begin
                  Get_Empty_Mailbox (CAN, Mailbox, Empty_Found);

                  exit when not Empty_Found;

                  Get_Next_Message (Tx_Buffer, Message);
                  Write_Tx_Message (CAN, Message, Mailbox);
                  Enable_Interrupts (CAN, Transmit_Mailbox_Empty);
                  Transmission_Request (CAN, Mailbox);
               end;
            end loop;
         end Send;

         procedure IRQ_Handler is
            use HAL;
            CAN : CAN_Controller renames Device.all;
         begin
            if Interrupt_Enabled (CAN, Transmit_Mailbox_Empty) then
               if Transmission_Completed (CAN, Mailbox_0) or else
                  Transmission_Completed (CAN, Mailbox_1) or else
                  Transmission_Completed (CAN, Mailbox_2)
               then
                  --  Either successful or aborted or failed
                  Disable_Interrupts (CAN, Transmit_Mailbox_Empty);
                  Send;
               end if;
            end if;

            if Interrupt_Enabled (CAN, FIFO_0_Message_Pending) and then
               Nof_Msg_In_Fifo (CAN, FIFO_0) > 0
            then
               --  Get message from fifo 0
               Rx_Msg := Read_Rx_Message (CAN, FIFO_0);
               Release_Fifo (CAN, FIFO_0);
               New_Message := True;

            elsif Interrupt_Enabled (CAN, FIFO_1_Message_Pending) and then
               Nof_Msg_In_Fifo (CAN, FIFO_1) > 0
            then
               --  Get message from fifo 1
               Rx_Msg := Read_Rx_Message (CAN, FIFO_1);
               Release_Fifo (CAN, FIFO_1);
               New_Message := True;
            end if;
         end IRQ_Handler;

      end Controller;
   end CAN_ISR;

end ACO.Drivers.Stm32f40x;
