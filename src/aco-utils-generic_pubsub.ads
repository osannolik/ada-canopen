generic
   type Item_Type is private;
   Max_Nof_Subscribers : Positive;

package ACO.Utils.Generic_Pubsub is

   pragma Preelaborate;

   type Sub is abstract tagged null record;

   type Ref is access all Sub'Class;

   procedure Update
     (This : access Sub;
      Data : in     Item_Type) is abstract;

   type Pub is abstract tagged private;

   procedure Update
     (This : in out Pub;
      Data : in     Item_Type);

   procedure Attach
     (This       : in out Pub;
      Subscriber : access Sub'Class);

   Already_Maximum_Nof_Subscribers : Exception;

   procedure Detach
     (This       : in out Pub;
      Subscriber : access Sub'Class);

private

   type Subscriber_List is array (1 .. Max_Nof_Subscribers) of Ref;

   type Pub is abstract tagged record
      Subscribers : Subscriber_List := (others => null);
   end record;

end ACO.Utils.Generic_Pubsub;
