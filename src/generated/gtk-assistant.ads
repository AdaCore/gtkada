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
--  A Gtk.Assistant.Gtk_Assistant is a widget used to represent a generally
--  complex operation splitted in several steps, guiding the user through its
--  pages and controlling the page flow to collect the necessary data.
--
--  The design of GtkAssistant is that it controls what buttons to show and to
--  make sensitive, based on what it knows about the page sequence and the
--  [type][GtkAssistantPageType] of each page, in addition to state information
--  like the page [completion][gtk-assistant-set-page-complete] and
--  [committed][gtk-assistant-commit] status.
--
--  If you have a case that doesn't quite fit in Gtk_Assistants way of
--  handling buttons, you can use the GTK_ASSISTANT_PAGE_CUSTOM page type and
--  handle buttons yourself.
--
--  # GtkAssistant as GtkBuildable
--
--  The GtkAssistant implementation of the Gtk.Buildable.Gtk_Buildable
--  interface exposes the Action_Area as internal children with the name
--  "action_area".
--
--  To add pages to an assistant in Gtk.Builder.Gtk_Builder, simply add it as
--  a child to the GtkAssistant object, and set its child properties as
--  necessary.
--
--  # CSS nodes
--
--  GtkAssistant has a single CSS node with the name assistant.
--
--  </description>
--  <group>Windows</group>
--  <testgtk>create_assistant.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

package Gtk.Assistant is

   type Gtk_Assistant_Record is new Gtk_Window_Record with null record;
   type Gtk_Assistant is access all Gtk_Assistant_Record'Class;

   type Gtk_Assistant_Page_Type is (
      Gtk_Assistant_Page_Content,
      Gtk_Assistant_Page_Intro,
      Gtk_Assistant_Page_Confirm,
      Gtk_Assistant_Page_Summary,
      Gtk_Assistant_Page_Progress,
      Gtk_Assistant_Page_Custom);
   pragma Convention (C, Gtk_Assistant_Page_Type);
   --  An enum for determining the page role inside the
   --  Gtk.Assistant.Gtk_Assistant. It's used to handle buttons sensitivity and
   --  visibility.
   --
   --  Note that an assistant needs to end its page flow with a page of type
   --  Gtk.Assistant.Gtk_Assistant_Page_Confirm,
   --  Gtk.Assistant.Gtk_Assistant_Page_Summary or
   --  Gtk.Assistant.Gtk_Assistant_Page_Progress to be correct.
   --
   --  The Cancel button will only be shown if the page isn't "committed". See
   --  Gtk.Assistant.Commit for details.

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Assistant_Page_Func is access function (Current_Page : Glib.Gint) return Glib.Gint;
   --  A function used by Gtk.Assistant.Set_Forward_Page_Func to know which is
   --  the next page given a current one. It's called both for computing the
   --  next page when the user presses the "forward" button and for handling
   --  the behavior of the "last" button.
   --  "current_page": The page number used to calculate the next page.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Assistant_Page_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Assistant_Page_Type);
   type Property_Gtk_Assistant_Page_Type is new Gtk_Assistant_Page_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Assistant : out Gtk_Assistant);
   procedure Initialize
      (Assistant : not null access Gtk_Assistant_Record'Class);
   --  Creates a new Gtk.Assistant.Gtk_Assistant.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Assistant_New return Gtk_Assistant;
   --  Creates a new Gtk.Assistant.Gtk_Assistant.
   --  Since: gtk+ 2.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_assistant_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Assistant : not null access Gtk_Assistant_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a widget to the action area of a Gtk.Assistant.Gtk_Assistant.
   --  Since: gtk+ 2.10
   --  "child": a Gtk.Widget.Gtk_Widget

   function Append_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Appends a page to the Assistant.
   --  Since: gtk+ 2.10
   --  "page": a Gtk.Widget.Gtk_Widget

   procedure Commit (Assistant : not null access Gtk_Assistant_Record);
   --  Erases the visited page history so the back button is not shown on the
   --  current page, and removes the cancel button from subsequent pages.
   --  Use this when the information provided up to the current page is
   --  hereafter deemed permanent and cannot be modified or undone. For
   --  example, showing a progress page to track a long-running, unreversible
   --  operation after the user has clicked apply on a confirmation page.
   --  Since: gtk+ 2.22

   function Get_Current_Page
      (Assistant : not null access Gtk_Assistant_Record) return Glib.Gint;
   --  Returns the page number of the current page.
   --  Since: gtk+ 2.10

   procedure Set_Current_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Num  : Glib.Gint);
   --  Switches the page to Page_Num.
   --  Note that this will only be necessary in custom buttons, as the
   --  Assistant flow can be set with Gtk.Assistant.Set_Forward_Page_Func.
   --  Since: gtk+ 2.10
   --  "page_num": index of the page to switch to, starting from 0. If
   --  negative, the last page will be used. If greater than the number of
   --  pages in the Assistant, nothing will be done.

   function Get_N_Pages
      (Assistant : not null access Gtk_Assistant_Record) return Glib.Gint;
   --  Returns the number of pages in the Assistant
   --  Since: gtk+ 2.10

   function Get_Nth_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Num  : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Returns the child widget contained in page number Page_Num.
   --  Since: gtk+ 2.10
   --  "page_num": the index of a page in the Assistant, or -1 to get the last
   --  page

   function Get_Page_Complete
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Gets whether Page is complete.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant

   procedure Set_Page_Complete
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Complete  : Boolean);
   --  Sets whether Page contents are complete.
   --  This will make Assistant update the buttons state to be able to
   --  continue the task.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "complete": the completeness status of the page

   function Get_Page_Has_Padding
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Gets whether page has padding.
   --  Since: gtk+ 3.18
   --  "page": a page of Assistant

   procedure Set_Page_Has_Padding
      (Assistant   : not null access Gtk_Assistant_Record;
       Page        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Has_Padding : Boolean);
   --  Sets whether the assistant is adding padding around the page.
   --  Since: gtk+ 3.18
   --  "page": a page of Assistant
   --  "has_padding": whether this page has padding

   function Get_Page_Header_Image
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Get_Page_Header_Image);
   --  Gets the header image for Page.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.2, 1
   --  "page": a page of Assistant

   procedure Set_Page_Header_Image
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   pragma Obsolescent (Set_Page_Header_Image);
   --  Sets a header image for Page.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.2, 1
   --  "page": a page of Assistant
   --  "pixbuf": the new header image Page

   function Get_Page_Side_Image
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Get_Page_Side_Image);
   --  Gets the side image for Page.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.2, 1
   --  "page": a page of Assistant

   procedure Set_Page_Side_Image
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   pragma Obsolescent (Set_Page_Side_Image);
   --  Sets a side image for Page.
   --  This image used to be displayed in the side area of the assistant when
   --  Page is the current page.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.2, 1
   --  "page": a page of Assistant
   --  "pixbuf": the new side image Page

   function Get_Page_Title
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String;
   --  Gets the title for Page.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant

   procedure Set_Page_Title
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Title     : UTF8_String);
   --  Sets a title for Page.
   --  The title is displayed in the header area of the assistant when Page is
   --  the current page.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "title": the new title for Page

   function Get_Page_Type
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Assistant_Page_Type;
   --  Gets the page type of Page.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant

   procedure Set_Page_Type
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       The_Type  : Gtk_Assistant_Page_Type);
   --  Sets the page type for Page.
   --  The page type determines the page behavior in the Assistant.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "type": the new type for Page

   function Insert_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Glib.Gint) return Glib.Gint;
   --  Inserts a page in the Assistant at a given position.
   --  Since: gtk+ 2.10
   --  "page": a Gtk.Widget.Gtk_Widget
   --  "position": the index (starting at 0) at which to insert the page, or
   --  -1 to append the page to the Assistant

   procedure Next_Page (Assistant : not null access Gtk_Assistant_Record);
   --  Navigate to the next page.
   --  It is a programming error to call this function when there is no next
   --  page.
   --  This function is for use when creating pages of the
   --  GTK_ASSISTANT_PAGE_CUSTOM type.
   --  Since: gtk+ 3.0

   function Prepend_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Prepends a page to the Assistant.
   --  Since: gtk+ 2.10
   --  "page": a Gtk.Widget.Gtk_Widget

   procedure Previous_Page
      (Assistant : not null access Gtk_Assistant_Record);
   --  Navigate to the previous visited page.
   --  It is a programming error to call this function when no previous page
   --  is available.
   --  This function is for use when creating pages of the
   --  GTK_ASSISTANT_PAGE_CUSTOM type.
   --  Since: gtk+ 3.0

   procedure Remove_Action_Widget
      (Assistant : not null access Gtk_Assistant_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a widget from the action area of a Gtk.Assistant.Gtk_Assistant.
   --  Since: gtk+ 2.10
   --  "child": a Gtk.Widget.Gtk_Widget

   procedure Remove_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Num  : Glib.Gint);
   --  Removes the Page_Num's page from Assistant.
   --  Since: gtk+ 3.2
   --  "page_num": the index of a page in the Assistant, or -1 to remove the
   --  last page

   procedure Set_Forward_Page_Func
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Func : Gtk_Assistant_Page_Func);
   --  Sets the page forwarding function to be Page_Func.
   --  This function will be used to determine what will be the next page when
   --  the user presses the forward button. Setting Page_Func to null will make
   --  the assistant to use the default forward function, which just goes to
   --  the next visible page.
   --  Since: gtk+ 2.10
   --  "page_func": the Gtk_Assistant_Page_Func, or null to use the default
   --  one

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Forward_Page_Func_User_Data is

      type Gtk_Assistant_Page_Func is access function
        (Current_Page : Glib.Gint;
         Data         : User_Data_Type) return Glib.Gint;
      --  A function used by Gtk.Assistant.Set_Forward_Page_Func to know which is
      --  the next page given a current one. It's called both for computing the
      --  next page when the user presses the "forward" button and for handling
      --  the behavior of the "last" button.
      --  "current_page": The page number used to calculate the next page.
      --  "data": user data.

      procedure Set_Forward_Page_Func
         (Assistant : not null access Gtk.Assistant.Gtk_Assistant_Record'Class;
          Page_Func : Gtk_Assistant_Page_Func;
          Data      : User_Data_Type);
      --  Sets the page forwarding function to be Page_Func.
      --  This function will be used to determine what will be the next page
      --  when the user presses the forward button. Setting Page_Func to null
      --  will make the assistant to use the default forward function, which
      --  just goes to the next visible page.
      --  Since: gtk+ 2.10
      --  "page_func": the Gtk_Assistant_Page_Func, or null to use the default
      --  one
      --  "data": user data for Page_Func

   end Set_Forward_Page_Func_User_Data;

   procedure Update_Buttons_State
      (Assistant : not null access Gtk_Assistant_Record);
   --  Forces Assistant to recompute the buttons state.
   --  GTK+ automatically takes care of this in most situations, e.g. when the
   --  user goes to a different page, or when the visibility or completeness of
   --  a page changes.
   --  One situation where it can be necessary to call this function is when
   --  changing a value on the current page affects the future page flow of the
   --  assistant.
   --  Since: gtk+ 2.10

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Use_Header_Bar_Property : constant Glib.Properties.Property_Int;
   --  True if the assistant uses a Gtk.Header_Bar.Gtk_Header_Bar for action
   --  buttons instead of the action-area.
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
   --  The ::apply signal is emitted when the apply button is clicked.
   --
   --  The default behavior of the Gtk.Assistant.Gtk_Assistant is to switch to
   --  the page after the current page, unless the current page is the last
   --  one.
   --
   --  A handler for the ::apply signal should carry out the actions for which
   --  the wizard has collected data. If the action takes a long time to
   --  complete, you might consider putting a page of type
   --  Gtk.Assistant.Gtk_Assistant_Page_Progress after the confirmation page
   --  and handle this operation within the
   --  Gtk.Assistant.Gtk_Assistant::prepare signal of the progress page.

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
   --  The ::cancel signal is emitted when then the cancel button is clicked.

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
   --  The ::close signal is emitted either when the close button of a summary
   --  page is clicked, or when the apply button in the last page in the flow
   --  (of type Gtk.Assistant.Gtk_Assistant_Page_Confirm) is clicked.

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
   --  The ::prepare signal is emitted when a new page is set as the
   --  assistant's current page, before making the new page visible.
   --
   --  A handler for this signal can do any preparations which are necessary
   --  before showing Page.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

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

private
   Use_Header_Bar_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("use-header-bar");
end Gtk.Assistant;
