package body ACO.Loggers is

   function Build_String
     (Level   : Log_Level;
      Message : String)
      return String
   is
   begin
      return "[" & Level'Img & "] " & Message;
   end Build_String;

   procedure Put
     (This    : in out Logger;
      Level   : in     Log_Level;
      Message : in     String)
   is
   begin
      if Level /= Off and then
         Level >= This.Level and then
         This.Stream /= null
      then
         String'Write (This.Stream, Build_String (Level, Message));
      end if;
   end Put;

   procedure Put_Line
     (This    : in out Logger;
      Level   : in     Log_Level;
      Message : in     String)
   is
   begin
      Put (This, Level, Message & ASCII.LF);
   end Put_Line;

   procedure Set_Level
     (This  : in out Logger;
      Level : in     Log_Level)
   is
   begin
      This.Level := Level;
   end Set_Level;

   procedure Set_Stream
     (This   : in out Logger;
      Stream : access Root_Stream_Type'Class)
   is
   begin
      This.Stream := Stream;
   end Set_Stream;

   function Get_Stream (This : Logger) return Stream_Access
   is
   begin
      return Stream_Access (This.Stream);
   end Get_Stream;

end ACO.Loggers;
