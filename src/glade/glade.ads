-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                       Copyright (C) 2000                          --
--                           ACT-Europe                              --
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

--  <description>
--
--  This package is a binding to the libglade library that provides routines
--  to create widgets dynamically from an XML definition file.
--  @pxref{Package_Glade.XML}.
--
--  </description>

package Glade is

   pragma Linker_Options ("-lgtkada_glade");

   procedure Init;
   --  must be called before use of libglade

   procedure Gnome_Init;
   --  This is defined in libglade-gnome.
   --  It should be used instead of Glade_Init if you want to use the
   --  GNOME widget set with libglade.

   procedure Bonobo_Init;
   --  This is defined in libglade-bonobo
   --  It should be used instead of Glade_Init if you want to use the GNOME
   --  widget set with included Bonobo controls with libglade.

   procedure Load_Module (Module : String);
   --  Load the named dynamic module.
   --  Basically it is loaded, and the glade_init_module function is called.
   --  This function should do any library initialisation and call
   --  glade_register_widgets.

private
   pragma Import (C, Init, "glade_init");
   pragma Import (C, Gnome_Init, "glade_gnome_init");
   pragma Import (C, Bonobo_Init, "glade_bonobo_init");
end Glade;
