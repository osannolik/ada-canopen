private with Ada.Synchronous_Task_Control;

generic
   type Item_Type is private;
   Max_Nof_Items : Positive;

package ACO.Utils.Generic_Buffer is

   type Protected_Buffer is tagged limited private;

   procedure Put_Blocking
      (This : in out Protected_Buffer;
       Item : in     Item_Type);

   procedure Get_Blocking
      (This : in out Protected_Buffer;
       Item :    out Item_Type);

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

   subtype Index_Type is Positive;

   type Item_Array is array (Index_Type range <>) of Item_Type;

   protected type Buffer_Type is

      procedure Put
         (Item    : in     Item_Type;
          Success :    out Boolean);

      procedure Get
         (Item    : out Item_Type;
          Success : out Boolean);

      function Nof_Items return Natural;

   private
      Is_Full  : Boolean := False;
      Is_Empty : Boolean := True;

      Items      : Item_Array (1 .. Max_Nof_Items);
      Next_Index : Index_Type := 1;
      Old_Index  : Index_Type := 1;

      Count      : Natural    := 0;
   end Buffer_Type;

   type Protected_Buffer is tagged limited record
      Buffer    : Buffer_Type;
      Non_Full  : Ada.Synchronous_Task_Control.Suspension_Object;
      Non_Empty : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end ACO.Utils.Generic_Buffer;
