private with System;
private with ACO.Utils.DS.Generic_Collection;
private with ACO.Utils.Scope_Locks;

generic
   type Item_Type is private;
   Max_Nof_Subscribers : Positive;

package ACO.Utils.Generic_Pubsub is

   pragma Preelaborate;

   type Sub is abstract tagged null record;

   type Sub_Access is access all Sub'Class;

   procedure Update
     (This : access Sub;
      Data : in     Item_Type) is abstract;

   type Pub is abstract tagged limited private;

   procedure Update
     (This : in out Pub;
      Data : in     Item_Type);

   function Nof_Subscribers (This : in out Pub) return Natural;

   procedure Attach
     (This       : in out Pub;
      Subscriber : in     Sub_Access)
      with Pre => This.Nof_Subscribers < Max_Nof_Subscribers;

   procedure Detach
     (This       : in out Pub;
      Subscriber : in     Sub_Access);

private

   package C is new ACO.Utils.DS.Generic_Collection
      (Item_Type => Sub_Access,
       "="       => "=");

   type Pub is tagged limited record
      Subscribers : C.Collection (Max_Size => Max_Nof_Subscribers);
      Mutex       : aliased ACO.Utils.Scope_Locks.Mutex (System.Priority'Last);
   end record;

   type Sub_Array is array (Positive range <>) of Sub_Access;

   function Get_Subscribers
      (This : in out Pub)
       return Sub_Array;

end ACO.Utils.Generic_Pubsub;
