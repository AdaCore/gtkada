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

with Ada.Strings.Fixed;

package body Common is

   --------------------
   -- Destroy_Window --
   --------------------

   procedure Destroy_Window
      (Win : access Gtk.Window.Gtk_Window_Record'Class;
       Ptr : Gtk_Window_Access)
   is
      pragma Unreferenced (Win);
   begin
      Ptr.all := null;
   end Destroy_Window;

   --------------------
   -- Destroy_Dialog --
   --------------------

   procedure Destroy_Dialog
      (Win : access Gtk.Dialog.Gtk_Dialog_Record'Class;
       Ptr : Gtk_Dialog_Access)
   is
      pragma Unreferenced (Win);
   begin
      Ptr.all := null;
   end Destroy_Dialog;

   --------------
   -- Image_Of --
   --------------

   function Image_Of (I : Gint) return String is
   begin
      return Ada.Strings.Fixed.Trim (Gint'Image (I), Ada.Strings.Left);
   end Image_Of;

end Common;
