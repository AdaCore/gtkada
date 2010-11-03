-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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
--  This package provides a ready to use high level printing object.
--
--  To use this package,
--
--      ??? to be completed
--
--  </description>
--  <group>Miscellaneous</group>

with Glib; use Glib;
with Glib.Error;

with Gtk.Print_Context;   use Gtk.Print_Context;
with Gtk.Print_Operation; use Gtk.Print_Operation;
with Gtk.Window;

package Gtkada.Printing is

   type Gtkada_Print_Operation_Record is new
     Gtk_Print_Operation_Record with private;
   type Gtkada_Print_Operation is access all Gtkada_Print_Operation_Record;

   procedure Gtk_New (Op : out Gtkada_Print_Operation);
   procedure Initialize (Widget : access Gtkada_Print_Operation_Record'Class);
   --  Initialize the print operation

   type Draw_Page_Handler is access procedure
     (Op          : Gtkada_Print_Operation;
      Context     : Gtk_Print_Context;
      Page_Number : Gint);
   --  Called for every page that is printed. This handler must render the
   --  page Page_Number onto the cairo context obtained from Context using
   --  Gtk.Print_Context.Get_Cairo_Context.
   --  ??? Complete doc from gtkprintoperation.c

   procedure Install_Draw_Page_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Draw_Page_Handler);
   --  Install a Draw_Page_Handler.

   function Connect_And_Run
     (Op        : access Gtkada_Print_Operation_Record'Class;
      Action    : Gtk_Print_Operation_Action;
      Parent    : access Gtk.Window.Gtk_Window_Record'Class;
      Error     : Glib.Error.GError := null)
      return Gtk_Print_Operation_Result;
   --  Runs the print operation.
   --  See Gtk.Print_Operations.Run.

private

   type Gtkada_Print_Operation_Record is
     new Gtk_Print_Operation_Record
   with record
      Draw_Page : Draw_Page_Handler := null;
   end record;

end Gtkada.Printing;
