------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Supports compose sequences, dead keys and numeric Unicode input.
--
--  ## Compose sequences
--
--  `GtkIMContextSimple` reads compose sequences from the first of the
--  following files that is found: ~/.config/gtk-4.0/Compose, ~/.XCompose,
--  /usr/share/X11/locale/$locale/Compose (for locales that have a nontrivial
--  Compose file). A subset of the file syntax described in the Compose(5)
--  manual page is supported. Additionally, `include "%L"` loads GTK's built-in
--  table of compose sequences rather than the locale-specific one from X11.
--
--  If none of these files is found, `GtkIMContextSimple` uses a built-in
--  table of compose sequences that is derived from the X11 Compose files.
--
--  Note that compose sequences typically start with the Compose_key, which is
--  often not available as a dedicated key on keyboards. Keyboard layouts may
--  map this keysym to other keys, such as the right Control key.
--
--  ## Unicode characters
--
--  `GtkIMContextSimple` also supports numeric entry of Unicode characters by
--  typing <kbd>Ctrl</kbd>-<kbd>Shift</kbd>-<kbd>u</kbd>, followed by a
--  hexadecimal Unicode codepoint.
--
--  For example,
--
--  Ctrl-Shift-u 1 2 3 Enter
--
--  yields U+0123 LATIN SMALL LETTER G WITH CEDILLA, i.e. ģ.
--
--  ## Dead keys
--
--  `GtkIMContextSimple` supports dead keys. For example, typing
--
--  dead_acute a
--
--  yields U+00E! LATIN SMALL LETTER_A WITH ACUTE, i.e. á. Note that this
--  depends on the keyboard layout including dead keys.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gtk.IM_Context; use Gtk.IM_Context;

package Gtk.IM_Context_Simple is

   type Gtk_IM_Context_Simple_Record is new Gtk_IM_Context_Record with null record;
   type Gtk_IM_Context_Simple is access all Gtk_IM_Context_Simple_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_IM_Context_Simple);
   procedure Initialize
      (Self : not null access Gtk_IM_Context_Simple_Record'Class);
   --  Creates a new `GtkIMContextSimple`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_IM_Context_Simple_New return Gtk_IM_Context_Simple;
   --  Creates a new `GtkIMContextSimple`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_im_context_simple_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Compose_File
      (Self         : not null access Gtk_IM_Context_Simple_Record;
       Compose_File : UTF8_String);
   --  Adds an additional table from the X11 compose file.
   --  @param Compose_File The path of compose file

end Gtk.IM_Context_Simple;
