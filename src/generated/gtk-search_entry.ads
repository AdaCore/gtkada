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
--  Gtk.Search_Entry.Gtk_Search_Entry is a subclass of Gtk.GEntry.Gtk_Entry
--  that has been tailored for use as a search entry.
--
--  It will show an inactive symbolic "find" icon when the search entry is
--  empty, and a symbolic "clear" icon when there is text. Clicking on the
--  "clear" icon will empty the search entry.
--
--  Note that the search/clear icon is shown using a secondary icon, and thus
--  does not work if you are using the secondary icon position for some other
--  purpose.
--
--  To make filtering appear more reactive, it is a good idea to not react to
--  every change in the entry text immediately, but only after a short delay.
--  To support this, Gtk.Search_Entry.Gtk_Search_Entry emits the
--  Gtk.Search_Entry.Gtk_Search_Entry::search-changed signal which can be used
--  instead of the Gtk.Editable.Gtk_Editable::changed signal.
--
--  The Gtk.Search_Entry.Gtk_Search_Entry::previous-match,
--  Gtk.Search_Entry.Gtk_Search_Entry::next-match and
--  Gtk.Search_Entry.Gtk_Search_Entry::stop-search signals can be used to
--  implement moving between search results and ending the search.
--
--  Often, GtkSearchEntry will be fed events by means of being placed inside a
--  Gtk.Search_Bar.Gtk_Search_Bar. If that is not the case, you can use
--  Gtk.Search_Entry.Handle_Event to pass events.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;         use Gdk.Event;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Types;        use Glib.Types;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Cell_Editable; use Gtk.Cell_Editable;
with Gtk.Editable;      use Gtk.Editable;
with Gtk.GEntry;        use Gtk.GEntry;

package Gtk.Search_Entry is

   type Gtk_Search_Entry_Record is new Gtk_Entry_Record with null record;
   type Gtk_Search_Entry is access all Gtk_Search_Entry_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Search_Entry);
   procedure Initialize
      (Self : not null access Gtk_Search_Entry_Record'Class);
   --  Creates a Gtk.Search_Entry.Gtk_Search_Entry, with a find icon when the
   --  search field is empty, and a clear icon when it isn't.
   --  Since: gtk+ 3.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Search_Entry_New return Gtk_Search_Entry;
   --  Creates a Gtk.Search_Entry.Gtk_Search_Entry, with a find icon when the
   --  search field is empty, and a clear icon when it isn't.
   --  Since: gtk+ 3.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_search_entry_get_type");

   -------------
   -- Methods --
   -------------

   function Handle_Event
      (Self  : not null access Gtk_Search_Entry_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean;
   --  This function should be called when the top-level window which contains
   --  the search entry received a key event. If the entry is part of a
   --  Gtk.Search_Bar.Gtk_Search_Bar, it is preferable to call
   --  Gtk.Search_Bar.Handle_Event instead, which will reveal the entry in
   --  addition to passing the event to this function.
   --  If the key event is handled by the search entry and starts or continues
   --  a search, GDK_EVENT_STOP will be returned. The caller should ensure that
   --  the entry is shown in this case, and not propagate the event further.
   --  Since: gtk+ 3.16
   --  "event": a key event

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Editing_Done
      (Cell_Editable : not null access Gtk_Search_Entry_Record);

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Search_Entry_Record);

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Search_Entry_Record;
       Event         : Gdk.Event.Gdk_Event);

   procedure Copy_Clipboard
      (Editable : not null access Gtk_Search_Entry_Record);

   procedure Cut_Clipboard
      (Editable : not null access Gtk_Search_Entry_Record);

   procedure Delete_Selection
      (Editable : not null access Gtk_Search_Entry_Record);

   procedure Delete_Text
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   function Get_Chars
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;

   function Get_Editable
      (Editable : not null access Gtk_Search_Entry_Record) return Boolean;

   procedure Set_Editable
      (Editable    : not null access Gtk_Search_Entry_Record;
       Is_Editable : Boolean);

   function Get_Position
      (Editable : not null access Gtk_Search_Entry_Record) return Glib.Gint;

   procedure Set_Position
      (Editable : not null access Gtk_Search_Entry_Record;
       Position : Glib.Gint);

   procedure Get_Selection_Bounds
      (Editable      : not null access Gtk_Search_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);

   procedure Insert_Text
      (Editable        : not null access Gtk_Search_Entry_Record;
       New_Text        : UTF8_String;
       New_Text_Length : Glib.Gint;
       Position        : in out Glib.Gint);

   procedure Paste_Clipboard
      (Editable : not null access Gtk_Search_Entry_Record);

   procedure Select_Region
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Search_Entry_Void is not null access procedure
     (Self : access Gtk_Search_Entry_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Next_Match : constant Glib.Signal_Name := "next-match";
   procedure On_Next_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False);
   procedure On_Next_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::next-match signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user initiates a move to the next match for
   --  the current search string.
   --
   --  Applications should connect to it, to implement moving between matches.
   --
   --  The default bindings for this signal is Ctrl-g.

   Signal_Previous_Match : constant Glib.Signal_Name := "previous-match";
   procedure On_Previous_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False);
   procedure On_Previous_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::previous-match signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user initiates a move to the previous match
   --  for the current search string.
   --
   --  Applications should connect to it, to implement moving between matches.
   --
   --  The default bindings for this signal is Ctrl-Shift-g.

   Signal_Search_Changed : constant Glib.Signal_Name := "search-changed";
   procedure On_Search_Changed
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False);
   procedure On_Search_Changed
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The Gtk.Search_Entry.Gtk_Search_Entry::search-changed signal is emitted
   --  with a short delay of 150 milliseconds after the last change to the
   --  entry text.

   Signal_Stop_Search : constant Glib.Signal_Name := "stop-search";
   procedure On_Stop_Search
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False);
   procedure On_Stop_Search
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::stop-search signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user stops a search via keyboard input.
   --
   --  Applications should connect to it, to implement hiding the search entry
   --  in this case.
   --
   --  The default bindings for this signal is Escape.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellEditable"
   --
   --  - "Editable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Search_Entry_Record, Gtk_Search_Entry);
   function "+"
     (Widget : access Gtk_Search_Entry_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Search_Entry
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Editable is new Glib.Types.Implements
     (Gtk.Cell_Editable.Gtk_Cell_Editable, Gtk_Search_Entry_Record, Gtk_Search_Entry);
   function "+"
     (Widget : access Gtk_Search_Entry_Record'Class)
   return Gtk.Cell_Editable.Gtk_Cell_Editable
   renames Implements_Gtk_Cell_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Editable.Gtk_Cell_Editable)
   return Gtk_Search_Entry
   renames Implements_Gtk_Cell_Editable.To_Object;

   package Implements_Gtk_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, Gtk_Search_Entry_Record, Gtk_Search_Entry);
   function "+"
     (Widget : access Gtk_Search_Entry_Record'Class)
   return Gtk.Editable.Gtk_Editable
   renames Implements_Gtk_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Editable.Gtk_Editable)
   return Gtk_Search_Entry
   renames Implements_Gtk_Editable.To_Object;

end Gtk.Search_Entry;
