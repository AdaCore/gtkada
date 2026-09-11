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

--  A single-line text entry widget for use as a search entry.
--
--  The main API for interacting with a `GtkSearchEntry` as entry is the
--  `GtkEditable` interface.
--
--  <picture> <source srcset="search-entry-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkSearchEntry"
--  src="search-entry.png"> </picture>
--  It will show an inactive symbolic "find" icon when the search entry is
--  empty, and a symbolic "clear" icon when there is text. Clicking on the
--  "clear" icon will empty the search entry.
--
--  To make filtering appear more reactive, it is a good idea to not react to
--  every change in the entry text immediately, but only after a short delay.
--  To support this, `GtkSearchEntry` emits the
--  [signalGtk.SearchEntry::search-changed] signal which can be used instead of
--  the [signalGtk.Editable::changed] signal.
--
--  The [signalGtk.SearchEntry::previous-match],
--  [signalGtk.SearchEntry::next-match] and
--  [signalGtk.SearchEntry::stop-search] signals can be used to implement
--  moving between search results and ending the search.
--
--  Often, `GtkSearchEntry` will be fed events by means of being placed inside
--  a [classGtk.SearchBar]. If that is not the case, you can use
--  [methodGtk.SearchEntry.set_key_capture_widget] to let it capture key input
--  from another widget.
--
--  `GtkSearchEntry` provides only minimal API and should be used with the
--  [ifaceGtk.Editable] API.
--
--  ## Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.SearchEntry::activate] - [signalGtk.SearchEntry::next-match]
--  - [signalGtk.SearchEntry::previous-match] -
--  [signalGtk.SearchEntry::stop-search]
--
--  ## CSS Nodes
--
--  ``` entry.search ╰── text ```
--
--  `GtkSearchEntry` has a single CSS node with name entry that carries a
--  `.search` style class, and the text node is a child of that.
--
--  ## Accessibility
--
--  `GtkSearchEntry` uses the [enumGtk.AccessibleRole.search_box] role.
--
--  <group>Numeric/Text Data Entry</group>
--  <gtkada_demo>create_entry.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Widget;            use Gtk.Widget;
with Interfaces.C;          use Interfaces.C;

package Gtk.Search_Entry is

   type Gtk_Search_Entry_Record is new Gtk_Widget_Record with null record;
   type Gtk_Search_Entry is access all Gtk_Search_Entry_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Search_Entry);
   procedure Initialize
      (Self : not null access Gtk_Search_Entry_Record'Class);
   --  Creates a `GtkSearchEntry`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Search_Entry_New return Gtk_Search_Entry;
   --  Creates a `GtkSearchEntry`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_search_entry_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Input_Hints
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Enums.Gtk_Input_Hints;
   --  Gets the input purpose for Entry.
   --  Since: gtk+ 4.14
   --  @return The input hints

   procedure Set_Input_Hints
      (Self  : not null access Gtk_Search_Entry_Record;
       Hints : Gtk.Enums.Gtk_Input_Hints);
   --  Sets the input hints for Entry.
   --  Since: gtk+ 4.14
   --  @param Hints the new input hints

   function Get_Input_Purpose
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Enums.Gtk_Input_Purpose;
   --  Gets the input purpose of Entry.
   --  Since: gtk+ 4.14
   --  @return The input hints

   procedure Set_Input_Purpose
      (Self    : not null access Gtk_Search_Entry_Record;
       Purpose : Gtk.Enums.Gtk_Input_Purpose);
   --  Sets the input purpose of Entry.
   --  Since: gtk+ 4.14
   --  @param Purpose the new input purpose

   function Get_Key_Capture_Widget
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the widget that Entry is capturing key events from.
   --  @return The key capture widget.
   --  Return has transfer-ownership='none'

   procedure Set_Key_Capture_Widget
      (Self   : not null access Gtk_Search_Entry_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets Widget as the widget that Entry will capture key events from.
   --  Key events are consumed by the search entry to start or continue a
   --  search.
   --  If the entry is part of a `GtkSearchBar`, it is preferable to call
   --  [methodGtk.SearchBar.set_key_capture_widget] instead, which will reveal
   --  the entry in addition to triggering the search entry.
   --  Note that despite the name of this function, the events are only
   --  'captured' in the bubble phase, which means that editable child widgets
   --  of Widget will receive text input before it gets captured. If that is
   --  not desired, you can capture and forward the events yourself with
   --  [methodGtk.EventControllerKey.forward].
   --  @param Widget a `GtkWidget`

   function Get_Placeholder_Text
      (Self : not null access Gtk_Search_Entry_Record) return UTF8_String;
   --  Gets the placeholder text associated with Entry.
   --  Since: gtk+ 4.10
   --  @return The placeholder text.

   procedure Set_Placeholder_Text
      (Self : not null access Gtk_Search_Entry_Record;
       Text : UTF8_String := "");
   --  Sets the placeholder text associated with Entry.
   --  Since: gtk+ 4.10
   --  @param Text the text to set as a placeholder

   function Get_Search_Delay
      (Self : not null access Gtk_Search_Entry_Record) return Guint;
   --  Get the delay to be used between the last keypress and the
   --  [signalGtk.SearchEntry::search-changed] signal being emitted.
   --  Since: gtk+ 4.8
   --  @return a delay in milliseconds.

   procedure Set_Search_Delay
      (Self      : not null access Gtk_Search_Entry_Record;
       The_Delay : Guint);
   --  Set the delay to be used between the last keypress and the
   --  [signalGtk.SearchEntry::search-changed] signal being emitted.
   --  Since: gtk+ 4.8
   --  @param The_Delay a delay in milliseconds

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Search_Entry_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Search_Entry_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Search_Entry_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Search_Entry_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Search_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Search_Entry_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Search_Entry_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Search_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Search_Entry_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Search_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Search_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Delete_Selection
      (Self : not null access Gtk_Search_Entry_Record);

   procedure Delete_Text
      (Self      : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   procedure Finish_Delegate
      (Self : not null access Gtk_Search_Entry_Record);

   function Get_Alignment
      (Self : not null access Gtk_Search_Entry_Record)
       return Interfaces.C.C_float;

   procedure Set_Alignment
      (Self   : not null access Gtk_Search_Entry_Record;
       Xalign : Interfaces.C.C_float);

   function Get_Chars
      (Self      : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;

   function Get_Delegate
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Editable.Gtk_Editable;

   function Get_Editable
      (Self : not null access Gtk_Search_Entry_Record) return Boolean;

   procedure Set_Editable
      (Self        : not null access Gtk_Search_Entry_Record;
       Is_Editable : Boolean);

   function Get_Enable_Undo
      (Self : not null access Gtk_Search_Entry_Record) return Boolean;

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Search_Entry_Record;
       Enable_Undo : Boolean);

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Search_Entry_Record) return Glib.Gint;

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Search_Entry_Record;
       N_Chars : Glib.Gint);

   function Get_Position
      (Self : not null access Gtk_Search_Entry_Record) return Glib.Gint;

   procedure Set_Position
      (Self     : not null access Gtk_Search_Entry_Record;
       Position : Glib.Gint);

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Search_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);

   function Get_Text
      (Self : not null access Gtk_Search_Entry_Record) return UTF8_String;

   procedure Set_Text
      (Self : not null access Gtk_Search_Entry_Record;
       Text : UTF8_String);

   function Get_Width_Chars
      (Self : not null access Gtk_Search_Entry_Record) return Glib.Gint;

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Search_Entry_Record;
       N_Chars : Glib.Gint);

   procedure Init_Delegate (Self : not null access Gtk_Search_Entry_Record);

   procedure Insert_Text
      (Self     : not null access Gtk_Search_Entry_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint);

   procedure Select_Region
      (Self      : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activates_Default_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to activate the default widget when Enter is pressed.

   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints;
   --  The hints about input for the `GtkSearchEntry` used to alter the
   --  behaviour of input methods.

   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose;
   --  The purpose for the `GtkSearchEntry` input used to alter the behaviour
   --  of input methods.

   Key_Capture_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget that the entry will use to capture key events.
   --
   --  Key events are consumed by the search entry to start or continue a
   --  search.

   Placeholder_Text_Property : constant Glib.Properties.Property_String;
   --  The text that will be displayed in the `GtkSearchEntry` when it is
   --  empty and unfocused.

   Search_Delay_Property : constant Glib.Properties.Property_Uint;
   --  The delay in milliseconds from last keypress to the search changed
   --  signal.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Search_Entry_Void is not null access procedure
     (Self : access Gtk_Search_Entry_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the entry is activated.
   --
   --  The keybindings for this signal are all forms of the <kbd>Enter</kbd>
   --  key.

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
   --  Emitted when the user initiates a move to the next match for the
   --  current search string.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  Applications should connect to it, to implement moving between matches.
   --
   --  The default bindings for this signal is <kbd>Ctrl</kbd>+<kbd>g</kbd>.

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
   --  Emitted when the user initiates a move to the previous match for the
   --  current search string.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  Applications should connect to it, to implement moving between matches.
   --
   --  The default bindings for this signal is
   --  <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>g</kbd>.

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
   --  Emitted with a delay. The length of the delay can be changed with the
   --  [propertyGtk.SearchEntry:search-delay] property.

   Signal_Search_Started : constant Glib.Signal_Name := "search-started";
   procedure On_Search_Started
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False);
   procedure On_Search_Started
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user initiated a search on the entry.

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
   --  Emitted when the user stops a search via keyboard input.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  Applications should connect to it, to implement hiding the search entry
   --  in this case.
   --
   --  The default bindings for this signal is <kbd>Escape</kbd>.

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
   --  - "Gtk.Editable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Search_Entry_Record, Gtk_Search_Entry);
   function "+"
     (Widget : access Gtk_Search_Entry_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Search_Entry
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Search_Entry_Record, Gtk_Search_Entry);
   function "+"
     (Widget : access Gtk_Search_Entry_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Search_Entry
   renames Implements_Gtk_Constraint_Target.To_Object;

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

private
   Search_Delay_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("search-delay");
   Placeholder_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("placeholder-text");
   Key_Capture_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("key-capture-widget");
   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose :=
     Gtk.Enums.Build ("input-purpose");
   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints :=
     Gtk.Enums.Build ("input-hints");
   Activates_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activates-default");
end Gtk.Search_Entry;
