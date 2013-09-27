------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;         use Gdk.Event;
with Glib;              use Glib;
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

   function Gtk_Search_Entry_New return Gtk_Search_Entry;
   --  Creates a Gtk.Search_Entry.Gtk_Search_Entry, with a find icon when the
   --  search field is empty, and a clear icon when it isn't.
   --  Since: gtk+ 3.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_search_entry_get_type");

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
       Start_Pos : Gint;
       End_Pos   : Gint := -1);

   function Get_Chars
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1) return UTF8_String;

   function Get_Editable
      (Editable : not null access Gtk_Search_Entry_Record) return Boolean;

   procedure Set_Editable
      (Editable    : not null access Gtk_Search_Entry_Record;
       Is_Editable : Boolean);

   function Get_Position
      (Editable : not null access Gtk_Search_Entry_Record) return Gint;

   procedure Set_Position
      (Editable : not null access Gtk_Search_Entry_Record;
       Position : Gint);

   procedure Get_Selection_Bounds
      (Editable      : not null access Gtk_Search_Entry_Record;
       Start_Pos     : out Gint;
       End_Pos       : out Gint;
       Has_Selection : out Boolean);

   procedure Insert_Text
      (Editable        : not null access Gtk_Search_Entry_Record;
       New_Text        : UTF8_String;
       New_Text_Length : Gint;
       Position        : in out Gint);

   procedure Paste_Clipboard
      (Editable : not null access Gtk_Search_Entry_Record);

   procedure Select_Region
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1);

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
