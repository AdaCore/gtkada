-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

package body Gdk.Input is

   ----------
   -- Init --
   ----------

   procedure Init is
      procedure Internal;
      pragma Import (C, Internal, "gdk_input_init");
   begin
      Internal;
   end Init;


   ----------
   -- Exit --
   ----------

   procedure Gdk_Exit is
      procedure Internal;
      pragma Import (C, Internal, "gdk_input_exit");
   begin
      Internal;
   end Gdk_Exit;


   --------------------------
   -- Set_Extension_Events --
   --------------------------

   procedure Set_Extension_Events (Window : in Gdk.Window.Gdk_Window;
                                   Mask   : in Gdk.Types.Gdk_Event_Mask;
                                   Mode   : in Gdk.Types.Gdk_Extension_Mode) is
      procedure Internal (Window : in System.Address;
                          Mask   : in Gdk.Types.Gdk_Event_Mask;
                          Mode   : in Gdk.Types.Gdk_Extension_Mode);
      pragma Import (C, Internal, "gdk_input_set_extension_events");
   begin
      Internal (Get_Object (Window), Mask, Mode);
   end Set_Extension_Events;

end Gdk.Input;
