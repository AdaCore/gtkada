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

--  `GtkAssistant` is used to represent a complex as a series of steps.
--
--  <picture> <source srcset="assistant-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkAssistant"
--  src="assistant.png"> </picture>
--  Each step consists of one or more pages. `GtkAssistant` guides the user
--  through the pages, and controls the page flow to collect the data needed
--  for the operation.
--
--  `GtkAssistant` handles which buttons to show and to make sensitive based
--  on page sequence knowledge and the [enumGtk.AssistantPageType] of each page
--  in addition to state information like the *completed* and *committed* page
--  statuses.
--
--  If you have a case that doesn't quite fit in `GtkAssistant`s way of
--  handling buttons, you can use the Gtk.Assistant_Page.Custom page type and
--  handle buttons yourself.
--
--  `GtkAssistant` maintains a `GtkAssistantPage` object for each added child,
--  which holds additional per-child properties. You obtain the
--  `GtkAssistantPage` for a child with [methodGtk.Assistant.get_page].
--
--  # GtkAssistant as GtkBuildable
--
--  The `GtkAssistant` implementation of the `GtkBuildable` interface exposes
--  the Action_Area as internal children with the name "action_area".
--
--  To add pages to an assistant in `GtkBuilder`, simply add it as a child to
--  the `GtkAssistant` object. If you need to set per-object properties, create
--  a `GtkAssistantPage` object explicitly, and set the child widget as a
--  property on it.
--
--  # CSS nodes
--
--  `GtkAssistant` has a single CSS node with the name window and style class
--  .assistant.
--
--  <group>Windows</group>
--  <gtkada_demo>create_assistant.adb</gtkada_demo>
--  <see>Gtk.Assistant_Page</see>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                   use Gdk;
with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Assistant_Page;    use Gtk.Assistant_Page;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Native;            use Gtk.Native;
with Gtk.Root;              use Gtk.Root;
with Gtk.Shortcut_Manager;  use Gtk.Shortcut_Manager;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;

package Gtk.Assistant is

   pragma Obsolescent;
   --  This widget will be removed in GTK 5

   type Gtk_Assistant_Record is new Gtk_Window_Record with null record;
   type Gtk_Assistant is access all Gtk_Assistant_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Assistant_Page_Func is access function (Current_Page : Glib.Gint) return Glib.Gint;
   --  Type of callback used to calculate the next page in a `GtkAssistant`.
   --  It's called both for computing the next page when the user presses the
   --  "forward" button and for handling the behavior of the "last" button.
   --  See [methodGtk.Assistant.set_forward_page_func].
   --  @param Current_Page The page number used to calculate the next page.
   --  @return The next page number

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Assistant);
   procedure Initialize (Self : not null access Gtk_Assistant_Record'Class);
   --  Creates a new `GtkAssistant`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Assistant_New return Gtk_Assistant;
   --  Creates a new `GtkAssistant`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_assistant_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Self  : not null access Gtk_Assistant_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Add_Action_Widget);
   --  Adds a widget to the action area of a `GtkAssistant`.
   --  Deprecated since 4.10, 1
   --  @param Child a `GtkWidget`

   function Append_Page
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   pragma Obsolescent (Append_Page);
   --  Appends a page to the Assistant.
   --  Deprecated since 4.10, 1
   --  @param Page a `GtkWidget`
   --  @return the index (starting at 0) of the inserted page

   procedure Commit (Self : not null access Gtk_Assistant_Record);
   pragma Obsolescent (Commit);
   --  Erases the visited page history.
   --  GTK will then hide the back button on the current page, and removes the
   --  cancel button from subsequent pages.
   --  Use this when the information provided up to the current page is
   --  hereafter deemed permanent and cannot be modified or undone. For
   --  example, showing a progress page to track a long-running, unreversible
   --  operation after the user has clicked apply on a confirmation page.
   --  Deprecated since 4.10, 1

   function Get_Current_Page
      (Self : not null access Gtk_Assistant_Record) return Glib.Gint;
   pragma Obsolescent (Get_Current_Page);
   --  Returns the page number of the current page.
   --  Deprecated since 4.10, 1
   --  @return The index (starting from 0) of the current page in the
   --  Assistant, or -1 if the Assistant has no pages, or no current page

   procedure Set_Current_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page_Num : Glib.Gint);
   pragma Obsolescent (Set_Current_Page);
   --  Switches the page to Page_Num.
   --  Note that this will only be necessary in custom buttons, as the
   --  Assistant flow can be set with Gtk.Assistant.Set_Forward_Page_Func.
   --  Deprecated since 4.10, 1
   --  @param Page_Num index of the page to switch to, starting from 0. If
   --  negative, the last page will be used. If greater than the number of
   --  pages in the Assistant, nothing will be done.

   function Get_N_Pages
      (Self : not null access Gtk_Assistant_Record) return Glib.Gint;
   pragma Obsolescent (Get_N_Pages);
   --  Returns the number of pages in the Assistant
   --  Deprecated since 4.10, 1
   --  @return the number of pages in the Assistant

   function Get_Nth_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page_Num : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Nth_Page);
   --  Returns the child widget contained in page number Page_Num.
   --  Deprecated since 4.10, 1
   --  @param Page_Num the index of a page in the Assistant, or -1 to get the
   --  last page
   --  @return the child widget, or null if Page_Num is out of bounds. Has
   --  transfer-ownership='none'.

   function Get_Page
      (Self  : not null access Gtk_Assistant_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Assistant_Page.Gtk_Assistant_Page;
   pragma Obsolescent (Get_Page);
   --  Returns the `GtkAssistantPage` object for Child.
   --  Deprecated since 4.10, 1
   --  @param Child a child of Assistant
   --  @return the `GtkAssistantPage` for Child. Has
   --  transfer-ownership='none'.

   function Get_Page_Complete
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   pragma Obsolescent (Get_Page_Complete);
   --  Gets whether Page is complete.
   --  Deprecated since 4.10, 1
   --  @param Page a page of Assistant
   --  @return True if Page is complete.

   procedure Set_Page_Complete
      (Self     : not null access Gtk_Assistant_Record;
       Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Complete : Boolean);
   pragma Obsolescent (Set_Page_Complete);
   --  Sets whether Page contents are complete.
   --  This will make Assistant update the buttons state to be able to
   --  continue the task.
   --  Deprecated since 4.10, 1
   --  @param Page a page of Assistant
   --  @param Complete the completeness status of the page

   function Get_Page_Title
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String;
   pragma Obsolescent (Get_Page_Title);
   --  Gets the title for Page.
   --  Deprecated since 4.10, 1
   --  @param Page a page of Assistant
   --  @return the title for Page

   procedure Set_Page_Title
      (Self  : not null access Gtk_Assistant_Record;
       Page  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Title : UTF8_String);
   pragma Obsolescent (Set_Page_Title);
   --  Sets a title for Page.
   --  The title is displayed in the header area of the assistant when Page is
   --  the current page.
   --  Deprecated since 4.10, 1
   --  @param Page a page of Assistant
   --  @param Title the new title for Page

   function Get_Page_Type
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Assistant_Page.Gtk_Assistant_Page_Type;
   pragma Obsolescent (Get_Page_Type);
   --  Gets the page type of Page.
   --  Deprecated since 4.10, 1
   --  @param Page a page of Assistant
   --  @return the page type of Page

   procedure Set_Page_Type
      (Self     : not null access Gtk_Assistant_Record;
       Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       The_Type : Gtk.Assistant_Page.Gtk_Assistant_Page_Type);
   pragma Obsolescent (Set_Page_Type);
   --  Sets the page type for Page.
   --  The page type determines the page behavior in the Assistant.
   --  Deprecated since 4.10, 1
   --  @param Page a page of Assistant
   --  @param The_Type the new type for Page

   function Get_Pages
      (Self : not null access Gtk_Assistant_Record)
       return Glib.List_Model.Glist_Model;
   pragma Obsolescent (Get_Pages);
   --  Gets a list model of the assistant pages.
   --  Deprecated since 4.10, 1
   --  @return A list model of the pages.

   function Insert_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint) return Glib.Gint;
   pragma Obsolescent (Insert_Page);
   --  Inserts a page in the Assistant at a given position.
   --  Deprecated since 4.10, 1
   --  @param Page a `GtkWidget`
   --  @param Position the index (starting at 0) at which to insert the page,
   --  or -1 to append the page to the Assistant
   --  @return the index (starting from 0) of the inserted page

   procedure Next_Page (Self : not null access Gtk_Assistant_Record);
   pragma Obsolescent (Next_Page);
   --  Navigate to the next page.
   --  It is a programming error to call this function when there is no next
   --  page.
   --  This function is for use when creating pages of the
   --  Gtk.Assistant_Page.Custom type.
   --  Deprecated since 4.10, 1

   function Prepend_Page
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   pragma Obsolescent (Prepend_Page);
   --  Prepends a page to the Assistant.
   --  Deprecated since 4.10, 1
   --  @param Page a `GtkWidget`
   --  @return the index (starting at 0) of the inserted page

   procedure Previous_Page (Self : not null access Gtk_Assistant_Record);
   pragma Obsolescent (Previous_Page);
   --  Navigate to the previous visited page.
   --  It is a programming error to call this function when no previous page
   --  is available.
   --  This function is for use when creating pages of the
   --  Gtk.Assistant_Page.Custom type.
   --  Deprecated since 4.10, 1

   procedure Remove_Action_Widget
      (Self  : not null access Gtk_Assistant_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Remove_Action_Widget);
   --  Removes a widget from the action area of a `GtkAssistant`.
   --  Deprecated since 4.10, 1
   --  @param Child a `GtkWidget`

   procedure Remove_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page_Num : Glib.Gint);
   pragma Obsolescent (Remove_Page);
   --  Removes the Page_Num's page from Assistant.
   --  Deprecated since 4.10, 1
   --  @param Page_Num the index of a page in the Assistant, or -1 to remove
   --  the last page

   procedure Set_Forward_Page_Func
      (Self      : not null access Gtk_Assistant_Record;
       Page_Func : Gtk_Assistant_Page_Func);
   pragma Obsolescent (Set_Forward_Page_Func);
   --  Sets the page forwarding function to be Page_Func.
   --  This function will be used to determine what will be the next page when
   --  the user presses the forward button. Setting Page_Func to null will make
   --  the assistant to use the default forward function, which just goes to
   --  the next visible page.
   --  Deprecated since 4.10, 1
   --  @param Page_Func the `GtkAssistantPageFunc`, or null to use the default
   --  one

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Forward_Page_Func_User_Data is

      type Gtk_Assistant_Page_Func is access function
        (Current_Page : Glib.Gint;
         Data         : User_Data_Type) return Glib.Gint;
      --  Type of callback used to calculate the next page in a `GtkAssistant`.
      --  It's called both for computing the next page when the user presses the
      --  "forward" button and for handling the behavior of the "last" button.
      --  See [methodGtk.Assistant.set_forward_page_func].
      --  @param Current_Page The page number used to calculate the next page.
      --  @param Data user data.
      --  @return The next page number

      procedure Set_Forward_Page_Func
         (Self      : not null access Gtk.Assistant.Gtk_Assistant_Record'Class;
          Page_Func : Gtk_Assistant_Page_Func;
          Data      : User_Data_Type);
      pragma Obsolescent (Set_Forward_Page_Func);
      --  Sets the page forwarding function to be Page_Func.
      --  This function will be used to determine what will be the next page
      --  when the user presses the forward button. Setting Page_Func to null
      --  will make the assistant to use the default forward function, which
      --  just goes to the next visible page.
      --  Deprecated since 4.10, 1
      --  @param Page_Func the `GtkAssistantPageFunc`, or null to use the
      --  default one
      --  @param Data user data for Page_Func

   end Set_Forward_Page_Func_User_Data;

   procedure Update_Buttons_State
      (Self : not null access Gtk_Assistant_Record);
   pragma Obsolescent (Update_Buttons_State);
   --  Forces Assistant to recompute the buttons state.
   --  GTK automatically takes care of this in most situations, e.g. when the
   --  user goes to a different page, or when the visibility or completeness of
   --  a page changes.
   --  One situation where it can be necessary to call this function is when
   --  changing a value on the current page affects the future page flow of the
   --  assistant.
   --  Deprecated since 4.10, 1

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Assistant_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Assistant_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Assistant_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Assistant_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Assistant_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Assistant_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Assistant_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Assistant_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Assistant_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Assistant_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Assistant_Record) return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Assistant_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Assistant_Record);

   procedure Unrealize (Self : not null access Gtk_Assistant_Record);

   function Get_Display
      (Self : not null access Gtk_Assistant_Record) return Gdk.Gdk_Display;

   function Get_Focus
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Focus
      (Self  : not null access Gtk_Assistant_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Pages_Property : constant Glib.Properties.Property_Interface;
   --  Type: Glib.List_Model.Glist_Model
   --  `GListModel` containing the pages.

   Use_Header_Bar_Property : constant Glib.Properties.Property_Int;
   --  True if the assistant uses a `GtkHeaderBar` for action buttons instead
   --  of the action-area.
   --
   --  For technical reasons, this property is declared as an integer
   --  property, but you should only set it to True or False.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Assistant_Void is not null access procedure (Self : access Gtk_Assistant_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Apply : constant Glib.Signal_Name := "apply";
   procedure On_Apply
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False);
   procedure On_Apply
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the apply button is clicked.
   --
   --  The default behavior of the `GtkAssistant` is to switch to the page
   --  after the current page, unless the current page is the last one.
   --
   --  A handler for the ::apply signal should carry out the actions for which
   --  the wizard has collected data. If the action takes a long time to
   --  complete, you might consider putting a page of type
   --  Gtk.Assistant_Page.Progress after the confirmation page and handle this
   --  operation within the [signalGtk.Assistant::prepare] signal of the
   --  progress page.

   Signal_Cancel : constant Glib.Signal_Name := "cancel";
   procedure On_Cancel
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False);
   procedure On_Cancel
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when then the cancel button is clicked.

   Signal_Close : constant Glib.Signal_Name := "close";
   procedure On_Close
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False);
   procedure On_Close
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted either when the close button of a summary page is clicked, or
   --  when the apply button in the last page in the flow (of type
   --  Gtk.Assistant_Page.Confirm) is clicked.

   Signal_Escape : constant Glib.Signal_Name := "escape";
   procedure On_Escape
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False);
   procedure On_Escape
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The action signal for the Escape binding.

   type Cb_Gtk_Assistant_Gtk_Widget_Void is not null access procedure
     (Self : access Gtk_Assistant_Record'Class;
      Page : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Widget_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Page : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Prepare : constant Glib.Signal_Name := "prepare";
   procedure On_Prepare
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Prepare
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a new page is set as the assistant's current page, before
   --  making the new page visible.
   --
   --  A handler for this signal can do any preparations which are necessary
   --  before showing Page.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Assistant_Record, Gtk_Assistant);
   function "+"
     (Widget : access Gtk_Assistant_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Assistant
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Assistant_Record, Gtk_Assistant);
   function "+"
     (Widget : access Gtk_Assistant_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Assistant
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Assistant_Record, Gtk_Assistant);
   function "+"
     (Widget : access Gtk_Assistant_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Assistant
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Assistant_Record, Gtk_Assistant);
   function "+"
     (Widget : access Gtk_Assistant_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Assistant
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Root is new Glib.Types.Implements
     (Gtk.Root.Gtk_Root, Gtk_Assistant_Record, Gtk_Assistant);
   function "+"
     (Widget : access Gtk_Assistant_Record'Class)
   return Gtk.Root.Gtk_Root
   renames Implements_Gtk_Root.To_Interface;
   function "-"
     (Interf : Gtk.Root.Gtk_Root)
   return Gtk_Assistant
   renames Implements_Gtk_Root.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Assistant_Record, Gtk_Assistant);
   function "+"
     (Widget : access Gtk_Assistant_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Assistant
   renames Implements_Gtk_Shortcut_Manager.To_Object;

private
   Use_Header_Bar_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("use-header-bar");
   Pages_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("pages");
end Gtk.Assistant;
