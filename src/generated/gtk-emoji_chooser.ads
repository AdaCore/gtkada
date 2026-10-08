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

--  Used by text widgets to let users insert Emoji characters.
--
--  <picture> <source srcset="emojichooser-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkEmojiChooser"
--  src="emojichooser.png"> </picture>
--  `GtkEmojiChooser` emits the [signalGtk.EmojiChooser::emoji-picked] signal
--  when an Emoji is selected.
--
--  # Shortcuts and Gestures
--
--  `GtkEmojiChooser` supports the following keyboard shortcuts:
--
--  - <kbd>Ctrl</kbd>+<kbd>N</kbd> scrolls th the next section. -
--  <kbd>Ctrl</kbd>+<kbd>P</kbd> scrolls th the previous section.
--
--  # Actions
--
--  `GtkEmojiChooser` defines a set of built-in actions:
--
--  - `scroll.section` scrolls to the next or previous section.
--
--  # CSS nodes
--
--  ``` popover ├── box.emoji-searchbar │ ╰── entry.search ╰──
--  box.emoji-toolbar ├── button.image-button.emoji-section ├── ... ╰──
--  button.image-button.emoji-section ```
--
--  Every `GtkEmojiChooser` consists of a main node called popover. The
--  contents of the popover are largely implementation defined and supposed to
--  inherit general styles. The top searchbar used to search emoji and gets the
--  .emoji-searchbar style class itself. The bottom toolbar used to switch
--  between different emoji categories consists of buttons with the
--  .emoji-section style class and gets the .emoji-toolbar style class itself.

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                   use Gdk;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Native;            use Gtk.Native;
with Gtk.Popover;           use Gtk.Popover;
with Gtk.Shortcut_Manager;  use Gtk.Shortcut_Manager;

package Gtk.Emoji_Chooser is

   type Gtk_Emoji_Chooser_Record is new Gtk_Popover_Record with null record;
   type Gtk_Emoji_Chooser is access all Gtk_Emoji_Chooser_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Emoji_Chooser);
   procedure Initialize
      (Self : not null access Gtk_Emoji_Chooser_Record'Class);
   --  Creates a new `GtkEmojiChooser`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Emoji_Chooser_New return Gtk_Emoji_Chooser;
   --  Creates a new `GtkEmojiChooser`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_emoji_chooser_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Emoji_Chooser_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Emoji_Chooser_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Emoji_Chooser_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Emoji_Chooser_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Emoji_Chooser_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Emoji_Chooser_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Emoji_Chooser_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Emoji_Chooser_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Emoji_Chooser_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Emoji_Chooser_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Emoji_Chooser_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Emoji_Chooser_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Emoji_Chooser_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Emoji_Chooser_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Emoji_Chooser_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Emoji_Chooser_Record)
       return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Emoji_Chooser_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Emoji_Chooser_Record);

   procedure Unrealize (Self : not null access Gtk_Emoji_Chooser_Record);

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Emoji_Chooser_UTF8_String_Void is not null access procedure
     (Self : access Gtk_Emoji_Chooser_Record'Class;
      Text : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Text : UTF8_String);

   Signal_Emoji_Picked : constant Glib.Signal_Name := "emoji-picked";
   procedure On_Emoji_Picked
      (Self  : not null access Gtk_Emoji_Chooser_Record;
       Call  : Cb_Gtk_Emoji_Chooser_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Emoji_Picked
      (Self  : not null access Gtk_Emoji_Chooser_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user selects an Emoji.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Native"
   --
   --  - "Gtk.ShortcutManager"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Emoji_Chooser_Record, Gtk_Emoji_Chooser);
   function "+"
     (Widget : access Gtk_Emoji_Chooser_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Emoji_Chooser
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Emoji_Chooser_Record, Gtk_Emoji_Chooser);
   function "+"
     (Widget : access Gtk_Emoji_Chooser_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Emoji_Chooser
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Emoji_Chooser_Record, Gtk_Emoji_Chooser);
   function "+"
     (Widget : access Gtk_Emoji_Chooser_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Emoji_Chooser
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Emoji_Chooser_Record, Gtk_Emoji_Chooser);
   function "+"
     (Widget : access Gtk_Emoji_Chooser_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Emoji_Chooser
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Emoji_Chooser_Record, Gtk_Emoji_Chooser);
   function "+"
     (Widget : access Gtk_Emoji_Chooser_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Emoji_Chooser
   renames Implements_Gtk_Shortcut_Manager.To_Object;

end Gtk.Emoji_Chooser;
