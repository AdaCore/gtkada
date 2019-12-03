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

--  <description>
--
--  This package provides an implementation of multiline entries using
--  the Gtk_Text_View and the Gtk_Scrolled_Window widgets.
--
--  Its purpose is to provide a simpler API and a style similar to the
--  Gtk_Entry's one by default.
--
--  Gtkada_Multiline_Entry widgets can be referenced with the "entry" CSS main
--  node: they can also be distinguished from the normal entries using the
--  "mutiline" CSS node.
--
--  Example:
--
--  .entry.multiline {background-color: black;}
--
--  </description>
--  <group>Numeric/Text Data Entry</group>

with Glib;                use Glib;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_View;       use Gtk.Text_View;

package Gtkada.Multiline_Entry is

   type Gtkada_Multiline_Entry_Record is new Gtk_Frame_Record
   with private;
   type Gtkada_Multiline_Entry is
     access all Gtkada_Multiline_Entry_Record'Class;

   procedure Gtk_New
     (Mult_Entry : out Gtkada_Multiline_Entry);
   procedure Initialize
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record'Class);
   --  Creates a new multiline entry, with its associated scrolled window.

   function Get_Text
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record)
      return UTF8_String;
   --  Return the contents of the multiline entry widget, including newline
   --  characters if any.

   procedure Set_Text
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record;
      Text       : UTF8_String);
   --  Set the contents of the multiline entry widget.

   function Get_Buffer
     (Mult_Entry : not null access Gtkada_Multiline_Entry_Record)
      return Gtk_Text_Buffer;
   --  Return the buffer
private

   type Gtkada_Multiline_Entry_Record is new Gtk_Frame_Record
     with record
      Text_View : Gtk_Text_View;
   end record;

end Gtkada.Multiline_Entry;
