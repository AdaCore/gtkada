-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib.Object;  use Glib.Object;
with System;       use System;

package body Glib.Types is

   ----------------
   -- Implements --
   ----------------

   package body Implements is

      ---------------
      -- To_Object --
      ---------------

      function To_Object (Interf : Interface_Type) return Object_Type is
         Stub : Object_Type_Record;
      begin
         return Object_Type
           (Get_User_Data (System.Address (Interf), Stub));
      end To_Object;

      ------------------
      -- To_Interface --
      ------------------

      function To_Interface
        (Object : access Object_Type_Record'Class) return Interface_Type is
      begin
         return Interface_Type (Get_Object (Object));
      end To_Interface;
   end Implements;

end Glib.Types;
