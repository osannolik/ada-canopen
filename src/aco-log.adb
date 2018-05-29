package body ACO.Log is

   function Build_String
     (Level   : Log_Level;
      Message : String)
      return String
   is
   begin
      return "[" & Level'Img & "] " & Message;
   end Build_String;

   procedure Put
     (Level   : in Log_Level;
      Message : in String)
   is
   begin
      if Level /= Off and then
         Level >= Logger_Level and then
         Logger_Stream /= null
      then
         String'Write (Logger_Stream, Build_String (Level, Message));
      end if;
   end Put;

   procedure Put_Line
     (Level   : in Log_Level;
      Message : in String)
   is
   begin
      Put (Level, Message & ASCII.LF);
   end Put_Line;

   procedure Set_Level
     (Level : in Log_Level)
   is
   begin
      Logger_Level := Level;
   end Set_Level;

   procedure Set_Stream
     (Stream : access Root_Stream_Type'Class)
   is
   begin
      Logger_Stream := Stream;
   end Set_Stream;

   function Get_Stream return Stream_Access
   is
   begin
      return Stream_Access (Logger_Stream);
   end Get_Stream;

end ACO.Log;
