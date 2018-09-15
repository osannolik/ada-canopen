with System;

private with Ada.Synchronous_Task_Control;
private with ACO.Utils.Generic_Ring_Buffer;

generic
   type Item_Type is private;
   Max_Nof_Items : Positive;
   Ceiling : System.Priority;

package ACO.Utils.Generic_Protected_Buffer is
   --  A protected ring buffer

   pragma Preelaborate;

   type Protected_Buffer is tagged limited private;

   procedure Put_Blocking
      (This : in out Protected_Buffer;
       Item : in     Item_Type);

   procedure Get_Blocking
      (This : in out Protected_Buffer;
       Item :    out Item_Type);

   procedure Get
      (This : in out Protected_Buffer;
       Item :    out Item_Type)
      with Pre => not This.Is_Empty;

   function Count
      (This : Protected_Buffer)
       return Natural;

   function Is_Empty
      (This : Protected_Buffer)
       return Boolean;

   function Is_Full
      (This : Protected_Buffer)
       return Boolean;

private

   package RB is new ACO.Utils.Generic_Ring_Buffer (Item_Type, Max_Nof_Items);

   protected type Buffer_Type is

      procedure Put
         (Item    : in     Item_Type;
          Success :    out Boolean);

      procedure Get
         (Item    : out Item_Type;
          Success : out Boolean);

      function Nof_Items return Natural;

   private
      pragma Priority (Ceiling);

      Ring : RB.Ring_Buffer;
   end Buffer_Type;

   type Protected_Buffer is tagged limited record
      Buffer    : Buffer_Type;
      Non_Full  : Ada.Synchronous_Task_Control.Suspension_Object;
      Non_Empty : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end ACO.Utils.Generic_Protected_Buffer;
