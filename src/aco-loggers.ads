with Ada.Streams;

package ACO.Loggers is
   pragma Preelaborate;

   use Ada.Streams;

   type Logger is tagged limited private;

   type Logger_Access is access all Logger'Class;

   type Log_Level is
     (Debug,
      Info,
      Warning,
      Error,
      Off);

   procedure Put
     (This    : in out Logger;
      Level   : in     Log_Level;
      Message : in     String);

   procedure Put_Line
     (This    : in out Logger;
      Level   : in     Log_Level;
      Message : in     String);

   procedure Set_Level
     (This  : in out Logger;
      Level : in     Log_Level);

   procedure Set_Stream
     (This   : in out Logger;
      Stream : access Root_Stream_Type'Class);

   type Stream_Access is access all Root_Stream_Type'Class;

   function Get_Stream (This : Logger) return Stream_Access;

private

   type Logger is tagged limited record
      Level : Log_Level := Off;
      Stream : access Root_Stream_Type'Class := null;
   end record;

   function Build_String
     (Level   : Log_Level;
      Message : String)
      return String;

end ACO.Loggers;
