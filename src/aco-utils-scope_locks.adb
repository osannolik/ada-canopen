package body ACO.Utils.Scope_Locks is

   protected body Mutex
   is
      entry Seize when Is_Free
      is
      begin
         Is_Free := False;
      end Seize;

      procedure Release
      is
      begin
         Is_Free := True;
      end Release;

   end Mutex;

   overriding
   procedure Initialize
      (This : in out Scope_Lock)
   is
   begin
      This.Lock.Seize;
   end Initialize;

   overriding
   procedure Finalize
      (This : in out Scope_Lock)
   is
   begin
      This.Lock.Release;
   end Finalize;

end ACO.Utils.Scope_Locks;
