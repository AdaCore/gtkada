------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Glib;            use Glib;
with Gdk.RGBA;        use Gdk.RGBA;
with Gtk;             use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_Tag;    use Gtk.Text_Tag;
with Gtk.Text_Iter;   use Gtk.Text_Iter;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View;   use Gtk.Text_View;
with Gtk.Frame;       use Gtk.Frame;
with Gtk.Widget;      use Gtk.Widget;
with Pango.Font;      use Pango.Font;

package body Create_Text_View is

   procedure Insert_With_Tag
     (Buffer : access Gtk_Text_Buffer_Record'Class;
      Tag    : String;
      Text   : String);
   --  Insert some text with some special highlighting in the buffer.
   --  Note: in a real application, one would pass a Gtk_Tag instead of a tag
   --  name, to avoid a lookup in the tags table

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Text_View@B widget is a widget used to display any"
        & " text graphically. It provides support for changing fonts, colors,"
        & " background colors, as well as inserting pixmaps in the text."
        & ASCII.LF
        & "It is based on the model-view paradigm: the text itself is stored"
        & " in a non-graphical object, a @bGtk_Text_Buffer@B, which has"
        & " support for traversing the text through @bGtk_Text_Iter@B objects;"
        & " This buffer is then associated with one or many @bGtk_Text_View@B"
        & " which automatically reflect any change in the buffer."
        & ASCII.LF
        & "The text is fully editable";
   end Help;

   ---------------------
   -- Insert_With_Tag --
   ---------------------

   procedure Insert_With_Tag
     (Buffer : access Gtk_Text_Buffer_Record'Class;
      Tag    : String;
      Text   : String)
   is
      T : Gtk_Text_Tag;
      Iter, Start_Iter : Gtk_Text_Iter;
      Table : Gtk_Text_Tag_Table;
      Result : Boolean;
      pragma Warnings (Off, Result);
   begin
      Get_End_Iter (Buffer, Iter);

      if Tag = "" then
         Insert (Buffer, Iter, Text & ASCII.LF);

      else
         Table := Get_Tag_Table (Buffer);
         T := Lookup (Table, Tag);

         Insert (Buffer, Iter, Text & ASCII.LF);
         Start_Iter := Iter;
         Backward_Chars (Start_Iter, Text'Length + 1, Result);
         Apply_Tag (Buffer, T, Start_Iter, Iter);
      end if;
   end Insert_With_Tag;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Buffer   : Gtk_Text_Buffer;
      Tags     : Gtk_Text_Tag_Table;
      Tag      : Gtk_Text_Tag;
      Color    : Gdk_RGBA;
      View     : Gtk_Text_View;
      Scrolled : Gtk_Scrolled_Window;
      Success  : Boolean;

   begin
      Gtk_New (Tags);

      --  Create the tags that will be used to change the rendering of the
      --  text.

      Parse (Color, "red", Success);
      Gtk_New (Tag, "red");
      Add (Tags, Tag);
      Set_Property (Tag, Foreground_Rgba_Property, Color);

      Gtk_New (Tag, "courier");
      Add (Tags, Tag);
      Set_Property (Tag, Font_Desc_Property, From_String ("Courier bold"));

      --  Create the buffer and the views as appropriate

      Gtk_New (Buffer, Tags);

      Set_Label (Frame, "Text View");
      Gtk_New (View, Buffer);

      --  Insert some random text

      for Lien in 1 .. 50 loop
         for Count in 1 .. 10 loop
            Insert_With_Tag
              (Buffer, "", "A normal line with no special highlight");
         end loop;

         Insert_With_Tag
           (Buffer, "red", "Some text in red, notice the use of tags");
         Insert_With_Tag
           (Buffer, "courier", "The font can also be changed");
      end loop;

      --  Insert the view in the frame

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Always, Policy_Always);
      Add (Scrolled, View);

      Show_All (Scrolled);
      Add (Frame, Scrolled);
   end Run;

end Create_Text_View;
