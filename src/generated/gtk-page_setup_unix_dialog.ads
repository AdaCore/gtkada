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

--  Presents a page setup dialog for platforms which don't provide a native
--  page setup dialog, like Unix.
--
--  <picture> <source srcset="pagesetupdialog-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example
--  GtkPageSetupUnixDialog" src="pagesetupdialog.png"> </picture>
--  It can be used very much like any other GTK dialog, at the cost of the
--  portability offered by the high-level printing API in
--  [classGtk.PrintOperation].
--
--  ## CSS nodes
--
--  `GtkPageSetupUnixDialog` has a single CSS node with the name `window` and
--  style class `.pagesetup`.
--
--  <group>Printing</group>
--  <gtkada_demo>create_print.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                   use Gdk;
with Glib;                  use Glib;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Native;            use Gtk.Native;
with Gtk.Page_Setup;        use Gtk.Page_Setup;
with Gtk.Print_Settings;    use Gtk.Print_Settings;
with Gtk.Root;              use Gtk.Root;
with Gtk.Shortcut_Manager;  use Gtk.Shortcut_Manager;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;

package Gtk.Page_Setup_Unix_Dialog is

   type Gtk_Page_Setup_Unix_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_Page_Setup_Unix_Dialog is access all Gtk_Page_Setup_Unix_Dialog_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Page_Setup_Unix_Dialog;
       Title  : UTF8_String := "";
       Parent : Gtk.Window.Gtk_Window := null);
   procedure Initialize
      (Self   : not null access Gtk_Page_Setup_Unix_Dialog_Record'Class;
       Title  : UTF8_String := "";
       Parent : Gtk.Window.Gtk_Window := null);
   --  Creates a new page setup dialog.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Title the title of the dialog
   --  @param Parent transient parent of the dialog

   function Gtk_Page_Setup_Unix_Dialog_New
      (Title  : UTF8_String := "";
       Parent : Gtk.Window.Gtk_Window := null)
       return Gtk_Page_Setup_Unix_Dialog;
   --  Creates a new page setup dialog.
   --  @param Title the title of the dialog
   --  @param Parent transient parent of the dialog

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_page_setup_unix_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Page_Setup
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup;
   --  Gets the currently selected page setup from the dialog.
   --  @return the current page setup. Has transfer-ownership='full'.

   procedure Set_Page_Setup
      (Self       : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);
   --  Sets the `GtkPageSetup` from which the page setup dialog takes its
   --  values.
   --  @param Page_Setup a `GtkPageSetup`

   function Get_Print_Settings
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Print_Settings.Gtk_Print_Settings;
   --  Gets the current print settings from the dialog.
   --  @return the current print settings. Has transfer-ownership='none'.

   procedure Set_Print_Settings
      (Self           : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Print_Settings : access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class);
   --  Sets the `GtkPrintSettings` from which the page setup dialog takes its
   --  values.
   --  @param Print_Settings a `GtkPrintSettings`

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record);

   procedure Unrealize
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record);

   function Get_Display
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gdk.Gdk_Display;

   function Get_Focus
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Focus
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);

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
   --  - "Gtk.Root"
   --
   --  - "Gtk.ShortcutManager"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Page_Setup_Unix_Dialog_Record, Gtk_Page_Setup_Unix_Dialog);
   function "+"
     (Widget : access Gtk_Page_Setup_Unix_Dialog_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Page_Setup_Unix_Dialog
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Page_Setup_Unix_Dialog_Record, Gtk_Page_Setup_Unix_Dialog);
   function "+"
     (Widget : access Gtk_Page_Setup_Unix_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Page_Setup_Unix_Dialog
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Page_Setup_Unix_Dialog_Record, Gtk_Page_Setup_Unix_Dialog);
   function "+"
     (Widget : access Gtk_Page_Setup_Unix_Dialog_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Page_Setup_Unix_Dialog
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Page_Setup_Unix_Dialog_Record, Gtk_Page_Setup_Unix_Dialog);
   function "+"
     (Widget : access Gtk_Page_Setup_Unix_Dialog_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Page_Setup_Unix_Dialog
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Root is new Glib.Types.Implements
     (Gtk.Root.Gtk_Root, Gtk_Page_Setup_Unix_Dialog_Record, Gtk_Page_Setup_Unix_Dialog);
   function "+"
     (Widget : access Gtk_Page_Setup_Unix_Dialog_Record'Class)
   return Gtk.Root.Gtk_Root
   renames Implements_Gtk_Root.To_Interface;
   function "-"
     (Interf : Gtk.Root.Gtk_Root)
   return Gtk_Page_Setup_Unix_Dialog
   renames Implements_Gtk_Root.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Page_Setup_Unix_Dialog_Record, Gtk_Page_Setup_Unix_Dialog);
   function "+"
     (Widget : access Gtk_Page_Setup_Unix_Dialog_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Page_Setup_Unix_Dialog
   renames Implements_Gtk_Shortcut_Manager.To_Object;

end Gtk.Page_Setup_Unix_Dialog;
