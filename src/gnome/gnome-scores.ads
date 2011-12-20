------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Glib; use Glib;
with Gdk.Color;
with Gnome.Dialog;
with Gtk.Widget;
with Gtkada.Types; use Gtkada.Types;

package Gnome.Scores is

   type Gnome_Scores_Record is new
     Gnome.Dialog.Gnome_Dialog_Record with private;
   type Gnome_Scores is access all Gnome_Scores_Record'Class;

   type Time_T is new Long_Integer;

   procedure Gnome_New
     (Widget   : out Gnome_Scores;
      Names    : Chars_Ptr_Array;
      Scores   : out Gfloat;
      Times    : out Time_T;
      Clear    : Guint);

   procedure Initialize
     (Widget   : access Gnome_Scores_Record'Class;
      Names    : Chars_Ptr_Array;
      Scores   : out Gfloat;
      Times    : out Time_T;
      Clear    : Guint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Color
     (Gs  : access Gnome_Scores_Record;
      Pos : Guint;
      Col : Gdk.Color.Gdk_Color);

   procedure Set_Colors
     (Gs  : access Gnome_Scores_Record;
      Col : Gdk.Color.Gdk_Color);

   procedure Set_Current_Player
     (Gs : access Gnome_Scores_Record;
      J  : Gint);

   procedure Set_Def_Color
     (Gs  : access Gnome_Scores_Record;
      Col : Gdk.Color.Gdk_Color);

   procedure Set_Logo_Label
     (Gs    : access Gnome_Scores_Record;
      Txt   : String;
      Font  : String;
      Color : Gdk.Color.Gdk_Color);

   procedure Set_Logo_Label_Title
     (Gs  : access Gnome_Scores_Record;
      Txt : String);

   procedure Set_Logo_Pixmap
     (Gs   : access Gnome_Scores_Record;
      Logo : String);

   procedure Set_Logo_Widget
     (Gs : access Gnome_Scores_Record;
      W  : access Gtk.Widget.Gtk_Widget_Record'Class);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Scores_Record is new
     Gnome.Dialog.Gnome_Dialog_Record with null record;

   pragma Import (C, Get_Type, "gnome_scores_get_type");
end Gnome.Scores;
