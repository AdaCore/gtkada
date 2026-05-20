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

--  A `GtkWindow` subclass that integrates with `GtkApplication`.
--
--  Notably, `GtkApplicationWindow` can handle an application menubar.
--
--  This class implements the [ifaceGio.ActionGroup] and [ifaceGio.ActionMap]
--  interfaces, to let you add window-specific actions that will be exported by
--  the associated [classGtk.Application], together with its application-wide
--  actions. Window-specific actions are prefixed with the "win." prefix and
--  application-wide actions are prefixed with the "app." prefix. Actions must
--  be addressed with the prefixed name when referring to them from a menu
--  model.
--
--  Note that widgets that are placed inside a `GtkApplicationWindow` can also
--  activate these actions, if they implement the [ifaceGtk.Actionable]
--  interface.
--
--  The settings [propertyGtk.Settings:gtk-shell-shows-app-menu] and
--  [propertyGtk.Settings:gtk-shell-shows-menubar] tell GTK whether the desktop
--  environment is showing the application menu and menubar models outside the
--  application as part of the desktop shell. For instance, on OS X, both menus
--  will be displayed remotely; on Windows neither will be.
--
--  If the desktop environment does not display the menubar, it can be shown
--  in the `GtkApplicationWindow` by setting the
--  [propertyGtk.ApplicationWindow:show-menubar] property to true. If the
--  desktop environment does not display the application menu, then it will
--  automatically be included in the menubar or in the window's client-side
--  decorations.
--
--  See [classGtk.PopoverMenu] for information about the XML language used by
--  `GtkBuilder` for menu models.
--
--  See also: [methodGtk.Application.set_menubar].
--
--  ## A GtkApplicationWindow with a menubar
--
--  The code sample below shows how to set up a `GtkApplicationWindow` with a
--  menu bar defined on the [classGtk.Application]:
--
--  ```c GtkApplication *app = gtk_application_new ("org.gtk.test", 0);
--
--  GtkBuilder *builder = gtk_builder_new_from_string ( "<interface>" " <menu
--  id='menubar'>" " <submenu>" " <attribute name='label'
--  translatable='yes'>_Edit</attribute>" " <item>" " <attribute name='label'
--  translatable='yes'>_Copy</attribute>" " <attribute
--  name='action'>win.copy</attribute>" " </item>" " <item>" " <attribute
--  name='label' translatable='yes'>_Paste</attribute>" " <attribute
--  name='action'>win.paste</attribute>" " </item>" " </submenu>" " </menu>"
--  "</interface>", -1);
--
--  GMenuModel *menubar = G_MENU_MODEL (gtk_builder_get_object (builder,
--  "menubar")); gtk_application_set_menubar (GTK_APPLICATION (app), menubar);
--  g_object_unref (builder);
--
--  // ...
--
--  GtkWidget *window = gtk_application_window_new (app); ```

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;          use GNAT.Strings;
with Gdk;                   use Gdk;
with Glib;                  use Glib;
with Glib.Action;           use Glib.Action;
with Glib.Action_Group;     use Glib.Action_Group;
with Glib.Action_Map;       use Glib.Action_Map;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Glib.Variant;          use Glib.Variant;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Application;       use Gtk.Application;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Native;            use Gtk.Native;
with Gtk.Root;              use Gtk.Root;
with Gtk.Shortcut_Manager;  use Gtk.Shortcut_Manager;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;

package Gtk.Application_Window is

   type Gtk_Application_Window_Record is new Gtk_Window_Record with null record;
   type Gtk_Application_Window is access all Gtk_Application_Window_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Application_Window;
       Application : not null access Gtk.Application.Gtk_Application_Record'Class);
   procedure Initialize
      (Self        : not null access Gtk_Application_Window_Record'Class;
       Application : not null access Gtk.Application.Gtk_Application_Record'Class);
   --  Creates a new `GtkApplicationWindow`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Application an application

   function Gtk_Application_Window_New
      (Application : not null access Gtk.Application.Gtk_Application_Record'Class)
       return Gtk_Application_Window;
   --  Creates a new `GtkApplicationWindow`.
   --  @param Application an application

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_application_window_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Id
      (Self : not null access Gtk_Application_Window_Record) return Guint;
   --  Returns the unique ID of the window.
   --  If the window has not yet been added to a `GtkApplication`, returns
   --  `0`.
   --  @return the unique ID for the window, or `0` if the window has not yet
   --  been added to an application

   function Get_Show_Menubar
      (Self : not null access Gtk_Application_Window_Record) return Boolean;
   --  Returns whether the window will display a menubar for the app menu and
   --  menubar as needed.
   --  @return True if the window will display a menubar when needed

   procedure Set_Show_Menubar
      (Self         : not null access Gtk_Application_Window_Record;
       Show_Menubar : Boolean);
   --  Sets whether the window will display a menubar for the app menu and
   --  menubar as needed.
   --  @param Show_Menubar whether to show a menubar when needed

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Action_Added
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String);

   procedure Action_Enabled_Changed
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean);

   procedure Action_Removed
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String);

   procedure Action_State_Changed
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant);

   procedure Activate_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant);

   procedure Change_Action_State
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant);

   function Get_Action_Enabled
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Boolean;

   function Get_Action_Parameter_Type
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Get_Action_State
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Hint
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Type
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Has_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Boolean;

   function List_Actions
      (Self : not null access Gtk_Application_Window_Record)
       return GNAT.Strings.String_List;

   function Query_Action
      (Self           : not null access Gtk_Application_Window_Record;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean;

   procedure Add_Action
      (Self   : not null access Gtk_Application_Window_Record;
       Action : Glib.Action.Gaction);

   procedure Add_Action_Entries
      (Self      : not null access Gtk_Application_Window_Record;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address);

   function Lookup_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Action.Gaction;

   procedure Remove_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String);

   procedure Announce
      (Self     : not null access Gtk_Application_Window_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Application_Window_Record)
       return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Application_Window_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Application_Window_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Application_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Application_Window_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Application_Window_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Application_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Application_Window_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Application_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Application_Window_Record)
       return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Application_Window_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Application_Window_Record);

   procedure Unrealize
      (Self : not null access Gtk_Application_Window_Record);

   function Get_Display
      (Self : not null access Gtk_Application_Window_Record)
       return Gdk.Gdk_Display;

   function Get_Focus
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Focus
      (Self  : not null access Gtk_Application_Window_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Show_Menubar_Property : constant Glib.Properties.Property_Boolean;
   --  If this property is true, the window will display a menubar unless it
   --  is shown by the desktop shell.
   --
   --  See [methodGtk.Application.set_menubar].
   --
   --  If false, the window will not display a menubar, regardless of whether
   --  the desktop shell is showing it or not.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ActionGroup"
   --
   --  - "Gio.ActionMap"
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Native"
   --
   --  - "Gtk.Root"
   --
   --  - "Gtk.ShortcutManager"

   package Implements_Gaction_Group is new Glib.Types.Implements
     (Glib.Action_Group.Gaction_Group, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Glib.Action_Group.Gaction_Group
   renames Implements_Gaction_Group.To_Interface;
   function "-"
     (Interf : Glib.Action_Group.Gaction_Group)
   return Gtk_Application_Window
   renames Implements_Gaction_Group.To_Object;

   package Implements_Gaction_Map is new Glib.Types.Implements
     (Glib.Action_Map.Gaction_Map, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Glib.Action_Map.Gaction_Map
   renames Implements_Gaction_Map.To_Interface;
   function "-"
     (Interf : Glib.Action_Map.Gaction_Map)
   return Gtk_Application_Window
   renames Implements_Gaction_Map.To_Object;

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Application_Window
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Application_Window
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Application_Window
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Root is new Glib.Types.Implements
     (Gtk.Root.Gtk_Root, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Gtk.Root.Gtk_Root
   renames Implements_Gtk_Root.To_Interface;
   function "-"
     (Interf : Gtk.Root.Gtk_Root)
   return Gtk_Application_Window
   renames Implements_Gtk_Root.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Application_Window
   renames Implements_Gtk_Shortcut_Manager.To_Object;

private
   Show_Menubar_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-menubar");
end Gtk.Application_Window;
