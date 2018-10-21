package body ACO.SDO_Commands is

   function Create
      (Index : Entry_Index;
       Data  : Data_Array)
       return Download_Initiate_Cmd
   is
      Tmp : Expedited_Data := (others => 0);
   begin
      Tmp (Tmp'First .. Tmp'First + Data'Length - 1) := Data;
      return (As_Raw            => False,
              Command           => Download_Initiate_Req,
              Nof_No_Data       => Unsigned_2 (Expedited_Data'Length - Data'Length),
              Is_Expedited      => True,
              Is_Size_Indicated => True,
              Index             => Swap_Bus (Index.Object),
              Subindex          => Index.Sub,
              Data              => Tmp);
   end Create;

   function Create
      (Index : Entry_Index;
       Size  : Natural)
       return Download_Initiate_Cmd
   is
      ((As_Raw            => False,
        Command           => Download_Initiate_Req,
        Nof_No_Data       => 0,
        Is_Expedited      => False,
        Is_Size_Indicated => True,
        Index             => Swap_Bus (Index.Object),
        Subindex          => Index.Sub,
        Data              =>
           Data_Array (Octets_4'(Swap_Bus (Unsigned_32 (Size))))));

   function Create
      (Toggle      : Boolean;
       Is_Complete : Boolean;
       Data        : Data_Array)
       return Download_Segment_Cmd
   is
      Tmp : Segment_Data := (others => 0);
   begin
      Tmp (Tmp'First .. Tmp'First + Data'Length - 1) := Data;
      return (As_Raw      => False,
              Command     => Download_Segment_Req,
              Toggle      => Toggle,
              Nof_No_Data => Unsigned_3 (Segment_Data'Length - Data'Length),
              Is_Complete => Is_Complete,
              Data        => Tmp);
   end Create;

   function Create
      (Index : Entry_Index)
       return Download_Initiate_Resp
   is
      Tmp : Download_Initiate_Resp := (As_Raw => True, Raw => (others => 0));
   begin
      Tmp.Command := Download_Initiate_Conf;
      Tmp.Index := Swap_Bus (Index.Object);
      Tmp.Subindex := Index.Sub;
      return Tmp;
   end Create;

   function Create
      (Toggle : Boolean)
       return Download_Segment_Resp
   is
      ((As_Raw  => False,
        Command => Download_Segment_Conf,
        Toggle  => Toggle));

   function Create
      (Index : Entry_Index;
       Code  : Abort_Code_Type)
       return Abort_Cmd
   is
      ((As_Raw   => False,
        Command  => Abort_Req,
        Index    => Swap_Bus (Index.Object),
        Subindex => Index.Sub,
        Code     => Swap_Bus (Code)));

   function Create
      (Index : Entry_Index)
       return Upload_Initiate_Cmd
   is
      ((As_Raw   => False,
        Command  => Upload_Initiate_Req,
        Index    => Swap_Bus (Index.Object),
        Subindex => Index.Sub));

   function Create
      (Index : Entry_Index;
       Data  : Data_Array)
       return Upload_Initiate_Resp
   is
      Tmp : Expedited_Data := (others => 0);
   begin
      Tmp (Tmp'First .. Tmp'First + Data'Length - 1) := Data;
      return (As_Raw            => False,
              Command           => Upload_Initiate_Conf,
              Nof_No_Data       => Unsigned_2 (Expedited_Data'Length - Data'Length),
              Is_Expedited      => True,
              Is_Size_Indicated => True,
              Index             => Swap_Bus (Index.Object),
              Subindex          => Index.Sub,
              Data              => Tmp);
   end Create;

   function Create
      (Index : Entry_Index;
       Size  : Natural)
       return Upload_Initiate_Resp
   is
      ((As_Raw            => False,
        Command           => Upload_Initiate_Conf,
        Nof_No_Data       => 0,
        Is_Expedited      => False,
        Is_Size_Indicated => True,
        Index             => Swap_Bus (Index.Object),
        Subindex          => Index.Sub,
        Data              =>
           Data_Array (Octets_4'(Swap_Bus (Unsigned_32 (Size))))));

   function Create
      (Toggle : Boolean)
       return Upload_Segment_Cmd
   is
      ((As_Raw  => False,
        Command => Upload_Segment_Req,
        Toggle  => Toggle));

   function Create
      (Toggle      : Boolean;
       Is_Complete : Boolean;
       Data        : Data_Array)
       return Upload_Segment_Resp
   is
      Tmp : Segment_Data := (others => 0);
   begin
      Tmp (Tmp'First .. Tmp'First + Data'Length - 1) := Data;
      return (As_Raw      => False,
              Command     => Upload_Segment_Conf,
              Toggle      => Toggle,
              Nof_No_Data => Unsigned_3 (Segment_Data'Length - Data'Length),
              Is_Complete => Is_Complete,
              Data        => Tmp);
   end Create;

end ACO.SDO_Commands;
