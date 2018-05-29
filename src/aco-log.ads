with Ada.Streams;

package ACO.Log is
   pragma Preelaborate;

   use Ada.Streams;

   type Log_Level is
     (Debug,
      Info,
      Warning,
      Error,
      Off);

   procedure Put
     (Level   : in Log_Level;
      Message : in String);

   procedure Put_Line
     (Level   : in Log_Level;
      Message : in String);

   procedure Set_Level
     (Level : in Log_Level);

   procedure Set_Stream
     (Stream : access Root_Stream_Type'Class);

   type Stream_Access is access all Root_Stream_Type'Class;

   function Get_Stream return Stream_Access;

private

   Logger_Level : Log_Level := Debug;

   Logger_Stream : access Root_Stream_Type'Class := null;

   function Build_String
     (Level   : Log_Level;
      Message : String)
      return String;

end ACO.Log;
