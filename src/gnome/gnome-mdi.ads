------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Glib; use Glib;
with Gnome.App;
with Gnome.App_Helper;
with Gnome.MDI_Child;
with Glib.Object;
with Gtk.Widget;

package Gnome.MDI is

   use Gnome.MDI_Child;

   type Gnome_MDI_Record is new Glib.Object.GObject_Record with private;
   type Gnome_MDI is access all Gnome_MDI_Record'Class;

   type Gnome_MDI_Mode is mod 2 ** 32;
   Notebook     : constant Gnome_MDI_Mode :=  0;
   Toplevel     : constant Gnome_MDI_Mode :=  1;
   Modal        : constant Gnome_MDI_Mode :=  2;
   Default_Mode : constant Gnome_MDI_Mode := 42;

   type Flat_UI_Info_Array is new Gnome.App_Helper.UI_Info_Array (Natural);
   type Flat_UI_Info_Array_Access is access all Flat_UI_Info_Array;
   --  Type used to map C pointer to ui_info.
   --  Note that this type has unknown real bounds, so use with caution.

   procedure Gnome_New
     (Widget  : out Gnome_MDI;
      Appname : String;
      Title   : String);

   procedure Initialize
     (Widget  : access Gnome_MDI_Record'Class;
      Appname : String;
      Title   : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Add_Child
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class) return Gint;

   function Add_Toplevel_View
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class) return Gint;

   function Add_View
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class) return Gint;

   function Find_Child
     (MDI    : access Gnome_MDI_Record;
      Name   : String) return Gnome_MDI_Child;

   function Get_Active_Child
     (MDI : access Gnome_MDI_Record) return Gnome_MDI_Child;

   function Get_Active_View
     (MDI : access Gnome_MDI_Record) return Gtk.Widget.Gtk_Widget;

   function Get_Active_Window
     (MDI : access Gnome_MDI_Record) return Gnome.App.Gnome_App;

   function Get_App_From_View
     (View : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gnome.App.Gnome_App;

   function Get_Child_From_View
     (View : access Gtk.Widget.Gtk_Widget_Record'Class) return Gnome_MDI_Child;

   function Get_Child_Menu_Info
     (App : access Gnome.App.Gnome_App_Record'Class)
      return Flat_UI_Info_Array_Access;

   function Get_Menubar_Info
     (App : access Gnome.App.Gnome_App_Record'Class)
      return Flat_UI_Info_Array_Access;

   function Get_Toolbar_Info
     (App : access Gnome.App.Gnome_App_Record'Class)
      return Flat_UI_Info_Array_Access;

   function Get_View_From_Window
     (MDI    : access Gnome_MDI_Record;
      App    : access Gnome.App.Gnome_App_Record'Class)
      return Gtk.Widget.Gtk_Widget;

   procedure Open_Toplevel (MDI : access Gnome_MDI_Record);

   procedure Register
     (MDI    : access Gnome_MDI_Record;
      Object : access Glib.Object.GObject_Record'Class);

   function Remove_All
     (MDI    : access Gnome_MDI_Record;
      Force  : Gint) return Gint;

   function Remove_Child
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class;
      Force  : Gint) return Gint;

   function Remove_View
     (MDI    : access Gnome_MDI_Record;
      View   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Force  : Gint) return Gint;

   procedure Set_Active_View
     (MDI  : access Gnome_MDI_Record;
      View : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Child_List_Path
     (MDI  : access Gnome_MDI_Record;
      Path : String);

   procedure Set_Child_Menu_Path
     (MDI  : access Gnome_MDI_Record;
      Path : String);

   procedure Set_Menubar_Template
     (MDI       : access Gnome_MDI_Record;
      Menu_Tmpl : access Gnome.App_Helper.UI_Info_Array);

   procedure Set_Mode
     (MDI  : access Gnome_MDI_Record;
      Mode : Gnome_MDI_Mode);

   procedure Set_Toolbar_Template
     (MDI       : access Gnome_MDI_Record;
      Tbar_Tmpl : access Gnome.App_Helper.UI_Info_Array);

   procedure Unregister
     (MDI    : access Gnome_MDI_Record;
      Object : access Glib.Object.GObject_Record'Class);

   procedure Update_Child
     (MDI   : access Gnome_MDI_Record;
      Child : access Gnome_MDI_Child_Record'Class);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "add_child"
   --    function Handler (Widget : access Gnome_MDI_Record'Class;
   --       Arg1 : access Gnome_MDI_Child_Record'Class)
   --       return Gint;
   --
   --  - "remove_child"
   --    function Handler (Widget : access Gnome_MDI_Record'Class;
   --       Arg1 : access Gnome_MDI_Child_Record'Class)
   --       return Gint;
   --
   --  - "add_view"
   --    function Handler (Widget : access Gnome_MDI_Record'Class;
   --       Arg1 : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gint;
   --
   --  - "remove_view"
   --    function Handler (Widget : access Gnome_MDI_Record'Class;
   --       Arg1 : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gint;
   --
   --  - "child_changed"
   --    procedure Handler (Widget : access Gnome_MDI_Record'Class;
   --       Arg1 : access Gnome_MDI_Child_Record'Class);
   --
   --  - "view_changed"
   --    procedure Handler (Widget : access Gnome_MDI_Record'Class;
   --       Arg1 : access Gtk.Widget.Gtk_Widget_Record'Class);
   --
   --  - "app_created"
   --    procedure Handler (Widget : access Gnome_MDI_Record'Class;
   --       Arg1 : access Gnome.App.Gnome_App_Record'Class);
   --
   --  </signals>

private
   type Gnome_MDI_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gnome_mdi_get_type");
end Gnome.MDI;
