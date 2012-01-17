------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Gtk.File_Chooser;   use Gtk.File_Chooser;
with Gtk.Window;         use Gtk.Window;

with Glib.Type_Conversion_Hooks;

package body Gtk.File_Chooser_Dialog is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_File_Chooser_Dialog_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog            : out Gtk_File_Chooser_Dialog;
      Title             : String;
      Parent            : Gtk.Window.Gtk_Window;
      Action            : Gtk.File_Chooser.Gtk_File_Chooser_Action)
   is
   begin
      Dialog := new Gtk_File_Chooser_Dialog_Record;
      Initialize (Dialog, Title, Parent, Action);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog            : access Gtk_File_Chooser_Dialog_Record'Class;
      Title             : String;
      Parent            : Gtk.Window.Gtk_Window;
      Action            : Gtk.File_Chooser.Gtk_File_Chooser_Action)
   is
      function Internal
        (Title             : String;
         Parent            : System.Address;
         Action            : Gtk.File_Chooser.Gtk_File_Chooser_Action)
         return System.Address;
      pragma Import (C, Internal, "ada_gtk_file_chooser_dialog_new");

      P : System.Address := System.Null_Address;
   begin
      if Parent /= null then
         P := Get_Object (Parent);
      end if;

      Set_Object (Dialog, Internal (Title & ASCII.NUL, P, Action));
   end Initialize;

end Gtk.File_Chooser_Dialog;
