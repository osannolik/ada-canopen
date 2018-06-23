package body ACO.Messages is

   function CAN_Id (Msg : Message) return Id_Type is
      (Msg.CAN_Id.Id);

   function Func_Code (Msg : Message) return Function_Code is
      (Msg.CAN_Id.Code);

   function Node_Id (Msg : Message) return Node_Nr is
      (Msg.CAN_Id.Node);

   function Create (CAN_Id : Id_Type;
                    RTR    : Boolean;
                    DLC    : Data_Length;
                    Data   : Msg_Data)
                    return Message
   is
   begin
      return (CAN_Id => (True, CAN_Id),
              RTR    => RTR,
              Length => DLC,
              Data   => Data);
   end Create;

   function Create (CAN_Id : Id_Type;
                    RTR    : Boolean;
                    Data   : Data_Array)
                    return Message
   is
      Len  : constant Natural := Msg_Data'Length - Data'Length;
      Fill : constant Data_Array
         (Msg_Data'First .. Msg_Data'First + Len - 1) := (others => Fill_Data);
   begin
      return (CAN_Id => (True, CAN_Id),
              RTR    => RTR,
              Length => Data'Length,
              Data   => Data & Fill);
   end Create;

   function Create (Code : Function_Code;
                    Node : Node_Nr;
                    RTR  : Boolean;
                    Data : Data_Array)
                    return Message
   is
      Tmp : constant CAN_Id_Type := (False, Code, Node);
   begin
      return Create (CAN_Id => Tmp.Id,
                     RTR    => RTR,
                     Data   => Data);
   end Create;

   function Image (Msg : in Message) return String is
      function Trim (S : String) return String is
         (if S'Length > 1 and then S (S'First) = ' ' then
                S (S'First + 1 .. S'Last) else S);

      --  Yuck, but avoids Unbounded_String
      Data : constant String := "[" &
         Trim (Msg.Data (0)'Img) & "," & Msg.Data (1)'Img & "," &
         Msg.Data (2)'Img & "," & Msg.Data (3)'Img & "," &
         Msg.Data (4)'Img & "," & Msg.Data (5)'Img & "," &
         Msg.Data (6)'Img & "," & Msg.Data (7)'Img & "]";
   begin
      return "<message" &
             " code=""" & Trim (Msg.CAN_Id.Code'Img) & """" &
             " node=""" & Trim (Msg.CAN_Id.Node'Img) & """" &
             " rtr="""  & Trim (Msg.RTR'Img)         & """" &
             " dlc="""  & Trim (Msg.Length'Img)      & """" &
             ">" & Data & "</message>";
   end Image;

   procedure Print
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Msg    : in  Message)
   is
      use Ada.Streams;

      --  Plain text
      S : constant String := Image (Msg);
      Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (S'Length));
   begin
      for I in Buffer'Range loop
         Buffer (I) := Stream_Element (Character'Pos (S (Natural (I))));
      end loop;

      Stream.Write (Buffer);
   end Print;

end ACO.Messages;
