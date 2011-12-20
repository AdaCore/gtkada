------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

package body Gtk.Recent_Chooser_Dialog is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Recent_Chooser_Dialog;
      Title  : String;
      Parent : access Gtk.Window.Gtk_Window_Record'Class)
   is
   begin
      Widget := new Gtk_Recent_Chooser_Dialog_Record;
      Gtk.Recent_Chooser_Dialog.Initialize (Widget, Title, Parent);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Recent_Chooser_Dialog_Record'Class;
      Title  : String;
      Parent : access Gtk.Window.Gtk_Window_Record'Class)
   is
      function Internal
        (Title  : String;
         Parent : System.Address)
         return System.Address;
      pragma Import (C, Internal, "ada_gtk_recent_chooser_dialog_new");
   begin
      Set_Object (Widget, Internal (Title & ASCII.NUL, Get_Object (Parent)));
   end Initialize;

   -------------------------
   -- Gtk_New_For_Manager --
   -------------------------

   procedure Gtk_New_For_Manager
     (Widget  : out Gtk_Recent_Chooser_Dialog;
      Title   : String;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class;
      Manager : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class)
   is
   begin
      Widget := new Gtk_Recent_Chooser_Dialog_Record;
      Initialize_For_Manager (Widget, Title, Parent, Manager);
   end Gtk_New_For_Manager;

   ----------------------------
   -- Initialize_For_Manager --
   ----------------------------

   procedure Initialize_For_Manager
     (Widget  : access Gtk_Recent_Chooser_Dialog_Record'Class;
      Title   : String;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class;
      Manager : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class)
   is
      function Internal
        (Title   : String;
         Parent  : System.Address;
         Manager : System.Address)
         return System.Address;
      pragma Import
        (C, Internal, "ada_gtk_recent_chooser_dialog_new_for_manager");
   begin
      Set_Object
        (Widget,
         Internal
           (Title & ASCII.NUL, Get_Object (Parent), Get_Object (Manager)));
   end Initialize_For_Manager;

end Gtk.Recent_Chooser_Dialog;

--  Implemented by C helper, since these functions use varargs:
--  No binding: gtk_recent_chooser_dialog_new
--  No binding: gtk_recent_chooser_dialog_new_for_manager
