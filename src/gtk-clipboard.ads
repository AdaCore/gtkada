-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2002-2003 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <c_version>2.0.0</c_version>

with Gdk.Types;

package Gtk.Clipboard is

   type Gtk_Clipboard is new Glib.C_Proxy;

   function Get
     (Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
      return Gtk_Clipboard;
   --  Return the clipboard object for the given selection.
   --  Cut/copy/paste menu items and keyboard shortcuts should use
   --  the default clipboard, returned by passing Gdk_None for Selection.
   --  The currently-selected object or text should be provided on the
   --  clipboard identified by Selection_Primary. Cut/copy/paste menu items
   --  conceptually copy the contents of the Selection_Primary clipboard
   --  to the default clipboard, i.e. they copy the selection to what the
   --  user sees as the clipboard.
   --
   --  (Passing Gdk_None is the same as using
   --  gdk_atom_intern ("CLIPBOARD", False). See
   --  @uref{"http://www.freedesktop.org/standards/clipboards.txt"}
   --  for a detailed discussion of the "CLIPBOARD" vs. "PRIMARY" selections
   --  under the X window system. On Win32 the Selection_Primary
   --  clipboard is essentially ignored.)
   --
   --  It's possible to have arbitrary named clipboards; if you do invent
   --  new clipboards, you should prefix the selection name with an
   --  underscore (because the ICCCM requires that nonstandard atoms are
   --  underscore-prefixed), and namespace it as well. For example,
   --  if your application called "Foo" has a special-purpose
   --  clipboard, you might call it "_FOO_SPECIAL_CLIPBOARD".
   --
   --  Selection is a Gdk_Atom which identifies the clipboard to use.
   --
   --  If no clipboard already exists, a new one will be created. Once a
   --  clipboard object has been created, it is persistent for all time and
   --  cannot be freed.

   --  type Gtk_Clipboard_Get_Func is access procedure
   --    (Clipboard          : Gtk_Clipboard;
   --     Selection_Data     : Gtk_Selection_Data;
   --     Info               : Guint;
   --     User_Data_Or_Owner : System.Address);

   --  type Gtk_Clipboard_Clear_Func is access procedure
   --    (Clipboard          : Gtk_Clipboard;
   --     User_Data_Or_Owner : System.Address);

   --  function Set_With_Data
   --    (GtkClipboard          *clipboard;
   --     const GtkTargetEntry  *targets;
   --     guint                  n_targets;
   --     GtkClipboardGetFunc    get_func;
   --     GtkClipboardClearFunc  clear_func;
   --     gpointer               user_data) return Boolean;

   --  function Set_With_Owner
   --    (GtkClipboard          *clipboard;
   --     const GtkTargetEntry  *targets;
   --     guint                  n_targets;
   --     GtkClipboardGetFunc    get_func;
   --     GtkClipboardClearFunc  clear_func;
   --     GObject               *owner) return Boolean;

   --  function Get_Owner (Clipboard : Gtk_Clipboard)
   --    return Glib.Object.GObject;

   procedure Clear (Clipboard : Gtk_Clipboard);
   --  Clear the contents of the clipboard.
   --  Generally this should only be called between the time you call
   --  Set_With_Owner or Set_With_Data, and when the Clear_Func you supplied
   --  is called. Otherwise, the clipboard may be owned by someone else.

   procedure Set_Text
     (Clipboard : Gtk_Clipboard;
      Text      : UTF8_String);
   --  Set the contents of the clipboard.

   --  procedure Request_Contents
   --    (Clipboard : Gtk_Clipboard;
   --     Target    : Gdk_Atom;
   --     Callback  : GtkClipboardReceivedFunc;
   --     User_Data : System.Address);

   --  procedure Request_Text
   --    (Clipboard : Gtk_Clipboard;
   --     Callback  : GtkClipboardTextReceivedFunc;
   --     User_Data : System.Address);

   --  function Wait_For_Contents
   --    (Clipboard : Gtk_Clipboard;
   --     Target    : Gdk_Atom) return Gtk_Selection_Data;

   --  function Wait_For_Text (Clipboard : Gtk_Clipboard) return UTF8_String;

   --  function Wait_Is_Text_Available
   --    (Clipboard : Gtk_Clipboard) return Boolean;

private
   pragma Import (C, Get, "gtk_clipboard_get");
   pragma Import (C, Clear, "gtk_clipboard_clear");
end Gtk.Clipboard;
