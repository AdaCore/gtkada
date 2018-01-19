------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body Task_Worker is

   use type Ada.Containers.Count_Type;

   -------------------
   --  Working_Task --
   -------------------

   task type Working_Task (Number : Natural);
   type Task_Access is access Working_Task;

   Task_Number : Natural := 1;
   --  A global counter for tasks

   task body Working_Task is
      Data : Work_Item;
      Num  : constant String := Natural'Image (Number);
   begin
      --  Send some data on the queue
      Data.Some_Data := To_Unbounded_String ("hello from task" & Num);
      Queue.Enqueue (Data);

      --  Simulate a blocking work: assume the work takes 1 second to compute;
      --  the interesting part of this example is that the GUI will keep being
      --  operational while this task is blocked.
      delay 1.0;
      Data.Some_Data := To_Unbounded_String ("hello again from task" & Num);
      Queue.Enqueue (Data);

      --  Another blocking work 2 seconds this time
      delay 2.0;
      Data.Some_Data := To_Unbounded_String ("goodbye from task" & Num);
      Queue.Enqueue (Data);
   end Working_Task;

   --------------
   -- Run_Task --
   --------------

   procedure Run_Task is
      A : Task_Access;
   begin
      A := new Working_Task (Task_Number);
      --  Note: we're leaking accesses here.
      Task_Number := Task_Number + 1;
   end Run_Task;

end Task_Worker;
