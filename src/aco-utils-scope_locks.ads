with System;
with Ada.Finalization;

package ACO.Utils.Scope_Locks is

   pragma Preelaborate;

   protected type Mutex
      (Ceiling : System.Priority)
   is
      entry Seize;

      procedure Release;

   private
      pragma Priority (Ceiling);

      Is_Free : Boolean := True;
   end Mutex;

   type Scope_Lock (Lock : not null access Mutex) is
      new Ada.Finalization.Limited_Controlled with null record;
   --  Declaring a Scope_Lock within the declarative region of a subprogram is
   --  sufficient to acquire the referenced lock and thereby providing mutual
   --  exclusion of calls to the subprogram, due to it being a controlled type.

private

   overriding
   procedure Initialize
      (This : in out Scope_Lock);

   overriding
   procedure Finalize
      (This : in out Scope_Lock);

end ACO.Utils.Scope_Locks;
