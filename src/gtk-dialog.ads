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

--  <description>
--  Dialog boxes are a convenient way to prompt the user for a small amount of
--  input, eg. to display a message, ask a question, or anything else that does
--  not require extensive effort on the user's part.
--
--  Gtkada treats a dialog as a window split horizontally. The top section is a
--  Gtk_Vbox, and is where widgets such as a Gtk_Label or a Gtk_Entry should be
--  packed. The second area is known as the action_area. This is generally used
--  for packing buttons into the dialog which may perform functions such as
--  cancel, ok, or apply. The two areas are separated by a Gtk_Hseparator.
--
--  If 'dialog' is a newly created dialog, the two primary areas of the window
--  can be accessed using Get_Vbox and Get_Action_Area as can be seen from the
--  example, below.
--
--  A 'modal' dialog (that is, one which freezes the rest of the application
--  from user input), can be created by calling Set_Modal on the dialog.
--
--  @pxref{Package_Gtkada.Dialogs} for a higher level dialog interface.
--  </description>
--  <c_version>1.2.8</c_version>

with Gtk.Box;
with Gtk.Window;

package Gtk.Dialog is

   type Gtk_Dialog_Record is new Gtk.Window.Gtk_Window_Record with private;
   type Gtk_Dialog is access all Gtk_Dialog_Record'Class;

   procedure Gtk_New (Dialog : out Gtk_Dialog);
   --  Create a new dialog.
   --  Widgets should not be packed into this widget directly, but into the
   --  vbox and action_area, as described above.

   procedure Initialize (Dialog : access Gtk_Dialog_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Dialog.

   function Get_Action_Area
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   --  Return the action area box associated with a Dialog.

   function Get_Vbox
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   --  Return the vertical box associated with a Dialog.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Dialog_Record is new Gtk.Window.Gtk_Window_Record with null record;

   pragma Import (C, Get_Type, "gtk_dialog_get_type");
end Gtk.Dialog;
