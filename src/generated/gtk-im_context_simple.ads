------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  GtkIMContextSimple is a simple input method context supporting table-based
--  input methods. It has a built-in table of compose sequences that is derived
--  from the X11 Compose files.
--
--  GtkIMContextSimple reads additional compose sequences from the first of
--  the following files that is found: ~/.config/gtk-3.0/Compose, ~/.XCompose,
--  /usr/share/X11/locale/$locale/Compose (for locales that have a nontrivial
--  Compose file). The syntax of these files is described in the Compose(5)
--  manual page.
--
--  GtkIMContextSimple also supports numeric entry of Unicode characters by
--  typing Ctrl-Shift-u, followed by a hexadecimal Unicode codepoint. For
--  example, Ctrl-Shift-u 1 2 3 Enter yields U+0123 LATIN SMALL LETTER G WITH
--  CEDILLA, i.e. ģ.
--
--  </description>

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
   --  Creates a new Gtk.IM_Context_Simple.Gtk_IM_Context_Simple.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_IM_Context_Simple_New return Gtk_IM_Context_Simple;
   --  Creates a new Gtk.IM_Context_Simple.Gtk_IM_Context_Simple.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_im_context_simple_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Compose_File
      (Self         : not null access Gtk_IM_Context_Simple_Record;
       Compose_File : UTF8_String);

   procedure Add_Table
      (Self        : not null access Gtk_IM_Context_Simple_Record;
       Data        : Gint_Array;
       Max_Seq_Len : Glib.Gint;
       N_Seqs      : Glib.Gint);
   --  Adds an additional table to search to the input context. Each row of
   --  the table consists of Max_Seq_Len key symbols followed by two Guint16
   --  interpreted as the high and low words of a gunicode value. Tables are
   --  searched starting from the last added.
   --  The table must be sorted in dictionary order on the numeric value of
   --  the key symbol fields. (Values beyond the length of the sequence should
   --  be zero.)
   --  "data": the table
   --  "max_seq_len": Maximum length of a sequence in the table (cannot be
   --  greater than GTK_MAX_COMPOSE_LEN)
   --  "n_seqs": number of sequences in the table

end Gtk.IM_Context_Simple;
