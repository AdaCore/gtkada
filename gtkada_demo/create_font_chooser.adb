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

with Gtk.Font_Chooser_Widget;   use Gtk.Font_Chooser_Widget;
with Gtk;                       use Gtk;
with Pango.Enums;               use Pango.Enums;
with Pango.Font;                use Pango.Font;
with Pango.Font_Face;           use Pango.Font_Face;
with Pango.Font_Family;         use Pango.Font_Family;

package body Create_Font_Chooser is

   function On_Filter
      (Family : not null access Pango_Font_Family_Record'Class;
       Face   : not null access Pango_Font_Face_Record'Class)
      return Boolean;
   --  Whether to show a font in the dialog

   ---------------
   -- On_Filter --
   ---------------

   function On_Filter
      (Family : not null access Pango_Font_Family_Record'Class;
       Face   : not null access Pango_Font_Face_Record'Class)
      return Boolean
   is
   begin
      --  Do not show fonts that are synthesized by the engine, for
      --  instance to emulate bold. These are generally lower quality

      if Face.Is_Synthesized then
         return False;
      end if;

      --  Hide all italic fonts

      if Get_Style (Face.Describe) = Pango_Style_Italic then
         return False;
      end if;

      --  Only show monospace (fixed size) fonts

      if not Family.Is_Monospace then
         return False;
      end if;

      return True;
   end On_Filter;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A very specific widget to select a new font based on its"
        & " characteristics, like the family, weight, size, ...";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Font       : Gtk_Font_Chooser_Widget;
   begin
      Set_Label (Frame, "Gtk_Font_Chooser_Widget");

      Gtk_New (Font);
      Font.Set_Font ("courier 9");
      Font.Set_Filter_Func (On_Filter'Access);

      --  ??? Show filter

      Frame.Add (Font);
      Frame.Show_All;
   end Run;

end Create_Font_Chooser;
