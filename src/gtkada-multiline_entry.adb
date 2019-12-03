------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with Glib.Error;

with Gtk.Css_Provider;    use Gtk.Css_Provider;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Style_Context;   use Gtk.Style_Context;
with Gtk.Style_Provider;
with Gtk.Text_Iter;       use Gtk.Text_Iter;

package body Gtkada.Multiline_Entry is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Mult_Entry : out Gtkada_Multiline_Entry)
   is
   begin
      Mult_Entry := new Gtkada_Multiline_Entry_Record;
      Gtkada.Multiline_Entry.Initialize (Mult_Entry);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
      Provider : Gtk_Css_Provider;
      Error    : aliased Glib.Error.GError;
      Success  : Boolean;
      Css      : constant String := ".entry.multiline {"
                   & ASCII.LF
                   & "padding: 8px;}"
                   & ASCII.LF
                   & ASCII.LF
                   & ".entry.multiline GtkTextView" & ASCII.LF
                   & "{background-color: transparent;}" & ASCII.LF;
      pragma Unreferenced (Success);
   begin
      Gtk.Frame.Initialize (Mult_Entry);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Mult_Entry.Add (Scrolled);

      Gtk_New (Mult_Entry.Text_View);
      Scrolled.Add (Mult_Entry.Text_View);

      --  Add the '.entry' class to the multiline entry widget so that it
      --  inherits the default style of entries.

      Get_Style_Context (Mult_Entry).Add_Class ("entry");

      --  Add a '.multiline' class so that users can distinguish normal entries
      --  from the multiline ones.

      Get_Style_Context (Mult_Entry).Add_Class ("multiline");

      --  Customize the CSS for multiline entriies so that the Gtk_Text_View's
      --  background is transparent.

      Gtk_New (Provider);

      Success := Load_From_Data
        (Provider,
         Data  => Css,
         Error => Error'Access);

      Gtk.Style_Context.Add_Provider_For_Screen
        (Get_Style_Context (Mult_Entry).Get_Screen, +Provider,
         Priority => Gtk.Style_Provider.Priority_Settings);
   end Initialize;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record)
      return UTF8_String
   is
      Buffer   : constant Gtk_Text_Buffer := Mult_Entry.Text_View.Get_Buffer;
      From, To : Gtk_Text_Iter;
   begin
      Buffer.Get_Start_Iter (From);
      Buffer.Get_End_Iter (To);

      return Buffer.Get_Text
        (Start   => From,
         The_End => To);
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record;
      Text       : UTF8_String)
   is
      Buffer : constant Gtk_Text_Buffer := Mult_Entry.Text_View.Get_Buffer;
   begin
      Buffer.Set_Text (Text);
   end Set_Text;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record)
      return Gtk_Text_Buffer is
   begin
      return Mult_Entry.Text_View.Get_Buffer;
   end Get_Buffer;

end Gtkada.Multiline_Entry;
