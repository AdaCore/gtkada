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
--  GtkApplicationWindow is a Gtk.Window.Gtk_Window subclass that offers some
--  extra functionality for better integration with
--  Gtk.Application.Gtk_Application features. Notably, it can handle both the
--  application menu as well as the menubar. See Gtk.Application.Set_App_Menu
--  and Gtk.Application.Set_Menubar.
--
--  This class implements the Glib.Action_Group.Gaction_Group and
--  Glib.Action_Map.Gaction_Map interfaces, to let you add window-specific
--  actions that will be exported by the associated
--  Gtk.Application.Gtk_Application, together with its application-wide
--  actions. Window-specific actions are prefixed with the "win." prefix and
--  application-wide actions are prefixed with the "app." prefix. Actions must
--  be addressed with the prefixed name when referring to them from a
--  Glib.Menu_Model.Gmenu_Model.
--
--  Note that widgets that are placed inside a GtkApplicationWindow can also
--  activate these actions, if they implement the GtkActionable interface.
--
--  As with Gtk.Application.Gtk_Application, the GDK lock will be acquired
--  when processing actions arriving from other processes and should therefore
--  be held when activating actions locally (if GDK threads are enabled).
--
--  The settings Gtk.Settings.Gtk_Settings:gtk-shell-shows-app-menu and
--  Gtk.Settings.Gtk_Settings:gtk-shell-shows-menubar tell GTK+ whether the
--  desktop environment is showing the application menu and menubar models
--  outside the application as part of the desktop shell. For instance, on OS
--  X, both menus will be displayed remotely; on Windows neither will be.
--  gnome-shell (starting with version 3.4) will display the application menu,
--  but not the menubar.
--
--  If the desktop environment does not display the menubar, then
--  Gtk.Application_Window.Gtk_Application_Window will automatically show a
--  Gtk.Menu_Bar.Gtk_Menu_Bar for it. (see the Gtk.Application.Gtk_Application
--  docs for some screenshots of how this looks on different platforms). This
--  behaviour can be overridden with the
--  Gtk.Application_Window.Gtk_Application_Window:show-menubar property. If the
--  desktop environment does not display the application menu, then it will
--  automatically be included in the menubar.
--
--  == A GtkApplicationWindow with a menubar ==
--
--    app = gtk_application_new ();
--    builder = gtk_builder_new ();
--    gtk_builder_add_from_string (builder,
--       "<interface>"
--       "  <menu id='menubar'>"
--       "    <submenu label='_Edit'>"
--       "      <item label='_Copy' action='win.copy'/>"
--       "      <item label='_Paste' action='win.paste'/>"
--       "    </submenu>"
--       "  </menu>"
--       "</interface>");
--    gtk_application_set_menubar (G_APPLICATION (app),
--       G_MENU_MODEL (gtk_builder_get_object (builder, "menubar")));
--    g_object_unref (builder);
--    ...
--    window = gtk_application_window_new (app);
--
--  == Handling fallback yourself ==
--
--    <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" parse="text" href="../../../../examples/sunny.c">
--    <xi:fallback>FIXME: MISSING XINCLUDE CONTENT</xi:fallback>
--    </xi:include>
--  The XML format understood by Gtk.Builder.Gtk_Builder for
--  Glib.Menu_Model.Gmenu_Model consists of a toplevel <tag
--  class="starttag">menu</tag> element, which contains one or more <tag
--  class="starttag">item</tag> elements. Each <tag class="starttag">item</tag>
--  element contains <tag class="starttag">attribute</tag> and <tag
--  class="starttag">link</tag> elements with a mandatory name attribute. <tag
--  class="starttag">link</tag> elements have the same content model as <tag
--  class="starttag">menu</tag>.
--
--  Attribute values can be translated using gettext, like other
--  Gtk.Builder.Gtk_Builder content. <tag class="starttag">attribute</tag>
--  elements can be marked for translation with a 'translatable="yes"'
--  attribute. It is also possible to specify message context and translator
--  comments, using the context and comments attributes. To make use of this,
--  the Gtk.Builder.Gtk_Builder must have been given the gettext domain to use.
--
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;      use GNAT.Strings;
with Glib;              use Glib;
with Glib.Action;       use Glib.Action;
with Glib.Action_Group; use Glib.Action_Group;
with Glib.Action_Map;   use Glib.Action_Map;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Glib.Variant;      use Glib.Variant;
with Gtk.Application;   use Gtk.Application;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Window;        use Gtk.Window;

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
   --  Creates a new Gtk.Application_Window.Gtk_Application_Window.
   --  Since: gtk+ 3.4
   --  "application": a Gtk.Application.Gtk_Application

   function Gtk_Application_Window_New
      (Application : not null access Gtk.Application.Gtk_Application_Record'Class)
       return Gtk_Application_Window;
   --  Creates a new Gtk.Application_Window.Gtk_Application_Window.
   --  Since: gtk+ 3.4
   --  "application": a Gtk.Application.Gtk_Application

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_application_window_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Id
      (Self : not null access Gtk_Application_Window_Record) return Guint;
   --  Returns the unique ID of the window. If the window has not yet been
   --  added to a Gtk.Application.Gtk_Application, returns '0'.
   --  Since: gtk+ 3.6

   function Get_Show_Menubar
      (Self : not null access Gtk_Application_Window_Record) return Boolean;
   --  Returns whether the window will display a menubar for the app menu and
   --  menubar as needed.
   --  Since: gtk+ 3.4

   procedure Set_Show_Menubar
      (Self         : not null access Gtk_Application_Window_Record;
       Show_Menubar : Boolean);
   --  Sets whether the window will display a menubar for the app menu and
   --  menubar as needed.
   --  Since: gtk+ 3.4
   --  "show_menubar": whether to show a menubar when needed

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

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

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Show_Menubar_Property : constant Glib.Properties.Property_Boolean;
   --  If this property is True, the window will display a menubar that
   --  includes the app menu and menubar, unless these are shown by the desktop
   --  shell. See Gtk.Application.Set_App_Menu and Gtk.Application.Set_Menubar.
   --
   --  If False, the window will not display a menubar, regardless of whether
   --  the desktop shell is showing the menus or not.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Gio.ActionGroup"
   --
   --  - "Gio.ActionMap"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Application_Window_Record, Gtk_Application_Window);
   function "+"
     (Widget : access Gtk_Application_Window_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Application_Window
   renames Implements_Gtk_Buildable.To_Object;

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

private
   Show_Menubar_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-menubar");
end Gtk.Application_Window;
