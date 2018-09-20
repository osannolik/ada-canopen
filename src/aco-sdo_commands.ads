with ACO.Messages;
with ACO.OD_Types;
with ACO.Utils.Byte_Order;
with Interfaces;
with System;

package ACO.SDO_Commands is
   use Interfaces;
   use ACO.Messages;
   use ACO.OD_Types;
   use ACO.Utils.Byte_Order;

   type Unsigned_3 is mod 2 ** 3 with Size => 3;
   type Unsigned_2 is mod 2 ** 2 with Size => 2;

   subtype Abort_Code_Type is Interfaces.Unsigned_32;

   Download_Initiate_Req  : constant := 1;
   Download_Initiate_Conf : constant := 3;
   Download_Segment_Req   : constant := 0;
   Download_Segment_Conf  : constant := 1;
   Upload_Initiate_Req    : constant := 2;
   Upload_Initiate_Conf   : constant := 2;
   Upload_Segment_Req     : constant := 3;
   Upload_Segment_Conf    : constant := 0;
   Abort_Req              : constant := 4;

   function Get_CS (Msg : Message) return Unsigned_3 is
      (Unsigned_3 (Shift_Right (Msg.Data (0), 5) and 2#111#));

   function Get_Index (Msg : Message) return Entry_Index is
      ((Object => Swap_Bus (Octets_2 (Msg.Data (1 .. 2))),
        Sub    => Msg.Data (3)));

   --        function Index_To_Bus (Index : Object_Index) return Data_Array is
   --           (Data_Array (Octets_2' (Swap_Bus (Unsigned_16 (Index)))));

   type Download_Initiate_Cmd (As_Raw : Boolean := False) is record
      case As_Raw is
         when True =>
            Raw               : Data_Array (0 .. 7);
         when False =>
            Command           : Unsigned_3;
            Nof_No_Data       : Unsigned_2;
            Is_Expedited      : Boolean;
            Is_Size_Indicated : Boolean;
            Index             : Unsigned_16;
            Subindex          : Unsigned_8;
            Data              : Data_Array (0 .. 3);
      end case;
   end record
      with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

   for Download_Initiate_Cmd use record
      Raw               at 0 range 0 .. 63;
      Data              at 0 range 32 .. 63;
      Subindex          at 0 range 24 .. 31;
      Index             at 0 range 8 .. 23;
      Command           at 0 range 5 .. 7;
      Nof_No_Data       at 0 range 2 .. 3;
      Is_Expedited      at 0 range 1 .. 1;
      Is_Size_Indicated at 0 range 0 .. 0;
   end record;

   function Get_Data_Size (Cmd : Download_Initiate_Cmd) return Natural is
      (if Cmd.Is_Expedited then 4 - Natural (Cmd.Nof_No_Data) else
             Natural (Swap_Bus (Octets_4 (Cmd.Data))));

   function Convert
      (Msg : Message) return Download_Initiate_Cmd
   is
      ((As_Raw => True, Raw => Msg.Data));

   subtype Expedited_Data is Data_Array (0 .. 3);

   function Create
      (Index : Entry_Index;
       Data  : Data_Array)
       return Download_Initiate_Cmd
      with Pre => Data'Length <= Expedited_Data'Length;

   function Create
      (Index : Entry_Index;
       Size  : Natural)
       return Download_Initiate_Cmd;

   subtype Segment_Data is Data_Array (0 .. 6);

   type Download_Segment_Cmd (As_Raw : Boolean := False) is record
      case As_Raw is
         when True =>
            Raw         : Data_Array (0 .. 7);
         when False =>
            Command     : Unsigned_3;
            Toggle      : Boolean;
            Nof_No_Data : Unsigned_3;
            Is_Complete : Boolean;
            Data        : Segment_Data;
      end case;
   end record
      with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

   for Download_Segment_Cmd use record
      Raw         at 0 range 0 .. 63;
      Data        at 0 range 8 .. 63;
      Command     at 0 range 5 .. 7;
      Toggle      at 0 range 4 .. 4;
      Nof_No_Data at 0 range 1 .. 3;
      Is_Complete at 0 range 0 .. 0;
   end record;

   function Convert
      (Msg : Message) return Download_Segment_Cmd
   is
      ((As_Raw => True, Raw => Msg.Data));

   function Create
      (Toggle      : Boolean;
       Is_Complete : Boolean;
       Data        : Data_Array)
       return Download_Segment_Cmd
      with Pre => Data'Length <= Segment_Data'Length;

   type Download_Initiate_Resp (As_Raw : Boolean := False) is record
      case As_Raw is
         when True =>
            Raw      : Data_Array (0 .. 7) := (others => 0);
         when False =>
            Command  : Unsigned_3;
            Index    : Unsigned_16;
            Subindex : Unsigned_8;
      end case;
   end record
      with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

   for Download_Initiate_Resp use record
      Raw      at 0 range 0 .. 63;
      Subindex at 0 range 24 .. 31;
      Index    at 0 range 8 .. 23;
      Command  at 0 range 5 .. 7;
   end record;

   function Convert
      (Msg : Message) return Download_Initiate_Resp
   is
      ((As_Raw => True, Raw => Msg.Data));

   function Create
      (Index : Entry_Index)
       return Download_Initiate_Resp;

   type Download_Segment_Resp (As_Raw : Boolean := False) is record
      case As_Raw is
         when True =>
            Raw     : Data_Array (0 .. 7) := (others => 0);
         when False =>
            Command : Unsigned_3;
            Toggle  : Boolean;
      end case;
   end record
      with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

   for Download_Segment_Resp use record
      Raw     at 0 range 0 .. 63;
      Command at 0 range 5 .. 7;
      Toggle  at 0 range 4 .. 4;
   end record;

   function Convert
      (Msg : Message) return Download_Segment_Resp
   is
      ((As_Raw => True, Raw => Msg.Data));

   function Create
      (Toggle : Boolean)
       return Download_Segment_Resp;

   type Abort_Cmd (As_Raw : Boolean := False) is record
      case As_Raw is
         when True =>
            Raw      : Data_Array (0 .. 7) := (others => 0);
         when False =>
            Command  : Unsigned_3;
            Index    : Unsigned_16;
            Subindex : Unsigned_8;
            Code     : Unsigned_32;
      end case;
   end record
      with Unchecked_Union, Size => 64, Bit_Order => System.Low_Order_First;

   for Abort_Cmd use record
      Raw      at 0 range 0 .. 63;
      Code     at 0 range 32 .. 63;
      Subindex at 0 range 24 .. 31;
      Index    at 0 range 8 .. 23;
      Command  at 0 range 5 .. 7;
   end record;

   function Convert
      (Msg : Message) return Abort_Cmd
   is
      ((As_Raw => True, Raw => Msg.Data));

   function Create
      (Index : Entry_Index;
       Code  : Abort_Code_Type)
       return Abort_Cmd;

   function Code (Cmd : Abort_Cmd) return Abort_Code_Type
   is
      (Abort_Code_Type (Unsigned_32' (Swap_Bus (Cmd.Code))));

end ACO.SDO_Commands;
