------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Gtk;                         use Gtk;
with Glib;                        use Glib;
with Gtk.Color_Chooser_Dialog;  use Gtk.Color_Chooser_Dialog;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Widget;                  use Gtk.Widget;

package body Create_Color_Chooser is

   type Gtk_Color_Dialog_Access is access all Gtk_Color_Chooser_Dialog;
   package Destroy_Dialog_Cb is new Handlers.User_Callback
     (Gtk_Color_Chooser_Dialog_Record, Gtk_Color_Dialog_Access);

   Dialog : aliased Gtk_Color_Chooser_Dialog;

   procedure Destroy_Dialog
     (Win : access Gtk_Color_Chooser_Dialog_Record'Class;
      Ptr : Gtk_Color_Dialog_Access);
   --  Called when the dialog is destroyed

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This widget provides an easy way to select new colors."
        & " This is a very specific widget, and most applications won't"
        & " need it. There are two versions, one with a dialog, and one"
        & " without.";
   end Help;

   --------------------
   -- Destroy_Dialog --
   --------------------

   procedure Destroy_Dialog
     (Win : access Gtk_Color_Chooser_Dialog_Record'Class;
      Ptr : Gtk_Color_Dialog_Access)
   is
      pragma Warnings (Off, Win);
   begin
      Ptr.all := null;
   end Destroy_Dialog;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk_Frame_Record'Class) is
      pragma Warnings (Off, Frame);
   begin
      if Dialog = null then
         Gtk_New (Dialog, Title => "Color Chooser Dialog", Parent => null);
         Set_Position (Dialog, Enums.Win_Pos_Mouse);

         Destroy_Dialog_Cb.Connect
           (Dialog, "destroy",
            Destroy_Dialog_Cb.To_Marshaller (Destroy_Dialog'Access),
            Dialog'Access);
         Show (Dialog);
      else
         Destroy (Dialog);
      end if;
   end Run;

end Create_Color_Chooser;
