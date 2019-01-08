with System;

private with Ada.Synchronous_Task_Control;
private with ACO.Utils.DS.Generic_Queue;

generic
   type Item_Type is private;

package ACO.Utils.DS.Generic_Protected_Queue is
   --  A protected queue

   pragma Preelaborate;

   type Protected_Queue
      (Max_Nof_Items : Positive;
       Ceiling       : System.Priority)
   is tagged limited private;

   procedure Put_Blocking
      (This : in out Protected_Queue;
       Item : in     Item_Type);

   procedure Put
      (This    : in out Protected_Queue;
       Item    : in     Item_Type;
       Success :    out Boolean);

   procedure Get_Blocking
      (This : in out Protected_Queue;
       Item :    out Item_Type);

   procedure Get
      (This : in out Protected_Queue;
       Item :    out Item_Type)
      with Pre => not This.Is_Empty;

   function Count
      (This : Protected_Queue)
       return Natural;

   function Is_Empty
      (This : Protected_Queue)
       return Boolean;

   function Is_Full
      (This : Protected_Queue)
       return Boolean;

private

   package Q is new ACO.Utils.DS.Generic_Queue (Item_Type);

   protected type Buffer_Type
      (Max_Nof_Items : Positive;
       Ceiling       : System.Priority)
   is

      procedure Put
         (Item    : in     Item_Type;
          Success :    out Boolean);

      procedure Get
         (Item    : out Item_Type;
          Success : out Boolean);

      function Nof_Items return Natural;

   private
      pragma Priority (Ceiling);

      Queue : Q.Queue (Max_Nof_Items => Max_Nof_Items);
   end Buffer_Type;

   type Protected_Queue
      (Max_Nof_Items : Positive;
       Ceiling       : System.Priority)
   is tagged limited record
      Buffer    : Buffer_Type (Max_Nof_Items, Ceiling);
      Non_Full  : Ada.Synchronous_Task_Control.Suspension_Object;
      Non_Empty : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end ACO.Utils.DS.Generic_Protected_Queue;
