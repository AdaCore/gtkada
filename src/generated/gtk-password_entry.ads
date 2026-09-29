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

--  A single-line text entry widget for entering passwords and other secrets.
--
--  <picture> <source srcset="password-entry-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example
--  GtkPasswordEntry" src="password-entry.png"> </picture>
--  It does not show its contents in clear text, does not allow to copy it to
--  the clipboard, and it shows a warning when Caps Lock is engaged. If the
--  underlying platform allows it, `GtkPasswordEntry` will also place the text
--  in a non-pageable memory area, to avoid it being written out to disk by the
--  operating system.
--
--  Optionally, it can offer a way to reveal the contents in clear text.
--
--  `GtkPasswordEntry` provides only minimal API and should be used with the
--  [ifaceGtk.Editable] API.
--
--  # CSS Nodes
--
--  ``` entry.password ╰── text ├── image.caps-lock-indicator ┊ ```
--
--  `GtkPasswordEntry` has a single CSS node with name entry that carries a
--  .passwordstyle class. The text Css node below it has a child with name
--  image and style class .caps-lock-indicator for the Caps Lock icon, and
--  possibly other children.
--
--  # Accessibility
--
--  `GtkPasswordEntry` uses the [enumGtk.AccessibleRole.text_box] role.
--
--  <group>Numeric/Text Data Entry</group>
--  <gtkada_demo>create_password_entry.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Menu_Model;       use Glib.Menu_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Widget;            use Gtk.Widget;
with Interfaces.C;          use Interfaces.C;

package Gtk.Password_Entry is

   type Gtk_Password_Entry_Record is new Gtk_Widget_Record with null record;
   type Gtk_Password_Entry is access all Gtk_Password_Entry_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Password_Entry);
   procedure Initialize
      (Self : not null access Gtk_Password_Entry_Record'Class);
   --  Creates a `GtkPasswordEntry`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Password_Entry_New return Gtk_Password_Entry;
   --  Creates a `GtkPasswordEntry`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_password_entry_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Extra_Menu
      (Self : not null access Gtk_Password_Entry_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Gets the menu model set with Gtk.Password_Entry.Set_Extra_Menu.
   --  @return the menu model. Has transfer-ownership='none'.

   procedure Set_Extra_Menu
      (Self  : not null access Gtk_Password_Entry_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets a menu model to add when constructing the context menu for Entry.
   --  @param Model a `GMenuModel`

   function Get_Show_Peek_Icon
      (Self : not null access Gtk_Password_Entry_Record) return Boolean;
   --  Returns whether the entry is showing an icon to reveal the contents.
   --  @return True if an icon is shown

   procedure Set_Show_Peek_Icon
      (Self           : not null access Gtk_Password_Entry_Record;
       Show_Peek_Icon : Boolean);
   --  Sets whether the entry should have a clickable icon to reveal the
   --  contents.
   --  Setting this to False also hides the text again.
   --  @param Show_Peek_Icon whether to show the peek icon

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Password_Entry_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Password_Entry_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Password_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Password_Entry_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Password_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Password_Entry_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Password_Entry_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Password_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Password_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Password_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Password_Entry_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Password_Entry_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Password_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Password_Entry_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Password_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Password_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Delete_Selection
      (Self : not null access Gtk_Password_Entry_Record);

   procedure Delete_Text
      (Self      : not null access Gtk_Password_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   procedure Finish_Delegate
      (Self : not null access Gtk_Password_Entry_Record);

   function Get_Alignment
      (Self : not null access Gtk_Password_Entry_Record)
       return Interfaces.C.C_float;

   procedure Set_Alignment
      (Self   : not null access Gtk_Password_Entry_Record;
       Xalign : Interfaces.C.C_float);

   function Get_Chars
      (Self      : not null access Gtk_Password_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;

   function Get_Delegate
      (Self : not null access Gtk_Password_Entry_Record)
       return Gtk.Editable.Gtk_Editable;

   function Get_Editable
      (Self : not null access Gtk_Password_Entry_Record) return Boolean;

   procedure Set_Editable
      (Self        : not null access Gtk_Password_Entry_Record;
       Is_Editable : Boolean);

   function Get_Enable_Undo
      (Self : not null access Gtk_Password_Entry_Record) return Boolean;

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Password_Entry_Record;
       Enable_Undo : Boolean);

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Password_Entry_Record) return Glib.Gint;

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Password_Entry_Record;
       N_Chars : Glib.Gint);

   function Get_Position
      (Self : not null access Gtk_Password_Entry_Record) return Glib.Gint;

   procedure Set_Position
      (Self     : not null access Gtk_Password_Entry_Record;
       Position : Glib.Gint);

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Password_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);

   function Get_Text
      (Self : not null access Gtk_Password_Entry_Record) return UTF8_String;

   procedure Set_Text
      (Self : not null access Gtk_Password_Entry_Record;
       Text : UTF8_String);

   function Get_Width_Chars
      (Self : not null access Gtk_Password_Entry_Record) return Glib.Gint;

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Password_Entry_Record;
       N_Chars : Glib.Gint);

   procedure Init_Delegate
      (Self : not null access Gtk_Password_Entry_Record);

   procedure Insert_Text
      (Self     : not null access Gtk_Password_Entry_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint);

   procedure Select_Region
      (Self      : not null access Gtk_Password_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activates_Default_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to activate the default widget when Enter is pressed.

   Extra_Menu_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  A menu model whose contents will be appended to the context menu.

   Placeholder_Text_Property : constant Glib.Properties.Property_String;
   --  The text that will be displayed in the `GtkPasswordEntry` when it is
   --  empty and unfocused.

   Show_Peek_Icon_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show an icon for revealing the content.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Password_Entry_Void is not null access procedure
     (Self : access Gtk_Password_Entry_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Password_Entry_Record;
       Call  : Cb_Gtk_Password_Entry_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Password_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the entry is activated.
   --
   --  The keybindings for this signal are all forms of the Enter key.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Password_Entry_Record, Gtk_Password_Entry);
   function "+"
     (Widget : access Gtk_Password_Entry_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Password_Entry
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Password_Entry_Record, Gtk_Password_Entry);
   function "+"
     (Widget : access Gtk_Password_Entry_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Password_Entry
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Password_Entry_Record, Gtk_Password_Entry);
   function "+"
     (Widget : access Gtk_Password_Entry_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Password_Entry
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, Gtk_Password_Entry_Record, Gtk_Password_Entry);
   function "+"
     (Widget : access Gtk_Password_Entry_Record'Class)
   return Gtk.Editable.Gtk_Editable
   renames Implements_Gtk_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Editable.Gtk_Editable)
   return Gtk_Password_Entry
   renames Implements_Gtk_Editable.To_Object;

private
   Show_Peek_Icon_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-peek-icon");
   Placeholder_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("placeholder-text");
   Extra_Menu_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("extra-menu");
   Activates_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activates-default");
end Gtk.Password_Entry;
