------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

with Gtk.Box;           use Gtk.Box;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Label;         use Gtk.Label;
with Gtk.Revealer;      use Gtk.Revealer;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;        use Gtk.Widget;

package body Create_Read_More is

   Details : Gtk_Revealer;
   --  Holds the part of the text that is shown on demand

   procedure On_Toggled (Self : access Gtk_Toggle_Button_Record'Class);
   --  Show or hide Details, and relabel Self to say what it will do next

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Revealer@B is a natural way to keep the long part of"
        & " a text out of the way until it is asked for."
        & ASCII.LF
        & "The first paragraph is always visible. The rest sits inside a"
        & " @bGtk_Revealer@B, and the button underneath toggles it with"
        & " @bSet_Reveal_Child@B, sliding the text open or closed while the"
        & " widgets below it follow along.";
   end Help;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled (Self : access Gtk_Toggle_Button_Record'Class) is
   begin
      Details.Set_Reveal_Child (Self.Get_Active);
      Self.Set_Label (if Self.Get_Active then "Read less" else "Read more");
   end On_Toggled;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Page   : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 12);
      Intro  : constant Gtk_Label :=
        Gtk_Label_New
          ("GtkAda binds GTK, Gdk, Glib, Pango and Cairo for Ada. The"
           & " mapping from C to Ada is mostly mechanical, so that"
           & " gtk_window_present becomes Gtk.Window.Present.");
      More   : constant Gtk_Label :=
        Gtk_Label_New
          ("Most of the bindings are generated from the GObject"
           & " introspection data of each library, refined by small"
           & " override files that choose Ada names and types. Only a"
           & " handful of packages are written by hand, for the parts of"
           & " the libraries that introspection cannot describe.");
      Button : constant Gtk_Toggle_Button :=
        Gtk_Toggle_Button_New_With_Label ("Read more");
   begin
      Frame.Set_Label ("Read More");

      Page.Set_Margin_Top (12);
      Page.Set_Margin_Bottom (12);
      Page.Set_Margin_Start (12);
      Page.Set_Margin_End (12);
      Frame.Set_Child (Page);

      Intro.Set_Wrap (True);
      Intro.Set_Xalign (0.0);
      Intro.Set_Max_Width_Chars (60);
      Page.Append (Intro);

      More.Set_Wrap (True);
      More.Set_Xalign (0.0);
      More.Set_Max_Width_Chars (60);

      Details := Gtk_Revealer_New;
      Details.Set_Transition_Type (Slide_Down);
      Details.Set_Child (More);
      Page.Append (Details);

      Button.Set_Halign (Align_Start);
      Button.On_Toggled (On_Toggled'Access);
      Page.Append (Button);
   end Run;

end Create_Read_More;
