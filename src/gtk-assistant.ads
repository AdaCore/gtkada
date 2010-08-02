-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  A Gtk_Assistant is a widget used to represent a generally complex
--  operation split into several steps, guiding the user through its
--  pages and controlling the page flow to collect the necessary data.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Windows</group>
--  <testgtk>create_assistant.adb</testgtk>

with Glib;
with Glib.Properties;
with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;

package Gtk.Assistant is

   type Gtk_Assistant_Record is new Gtk.Window.Gtk_Window_Record with private;
   type Gtk_Assistant is access all Gtk_Assistant_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Assistant);
   procedure Initialize (Widget : access Gtk_Assistant_Record'Class);
   --  Creates a new Gtk_Assistant.

   type Gtk_Assistant_Page_Type is
     (Gtk_Assistant_Page_Content,
      Gtk_Assistant_Page_Intro,
      Gtk_Assistant_Page_Confirm,
      Gtk_Assistant_Page_Summary,
      Gtk_Assistant_Page_Progress);
   --  Definition of various page types.  See Get_Page_Type/Set_Page_Type
   --  for more info.

   procedure Add_Action_Widget
     (Assistant : access Gtk_Assistant_Record;
      Child     : access Gtk_Widget_Record'Class);
   --  Adds a widget to the action area of a Gtk_Assistant.

   function Append_Page
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class)
      return Gint;
   --  Appends a page to the Assistant.
   --
   --  Return value: the index (starting at 0) of the inserted page

   function Get_Current_Page (Assistant : access Gtk_Assistant_Record)
      return Gint;
   --  Returns the index (starting from 0) of the current page in the
   --  Assistant.  If the Assistant has no pages, -1 will be returned

   function Get_N_Pages (Assistant : access Gtk_Assistant_Record) return Gint;
   --  Returns the number of pages in the Assistant

   function Get_Nth_Page
     (Assistant : access Gtk_Assistant_Record;
      Page_Num  : Gint)
      return Gtk_Widget;
   --  Assistant: a Gtk_Assistant
   --  Page_Num: The index of a page in the Assistant, or -1 to get the
   --  last page
   --
   --  Returns the child widget contained in page number Page_Num, or null
   --  if Page_Num is out of bounds.

   function Get_Page_Complete
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class)
      return Boolean;
   --  Returns whether Page is complete.

   function Get_Page_Header_Image
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class)
      return Gdk_Pixbuf;
   --  Gets the header image for Page.  Returns null if there's no header
   --  image for the page.

   function Get_Page_Side_Image
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class)
      return Gdk_Pixbuf;
   --  Gets the side image for Page.  Returns null if there's no side
   --  image for the page.

   function Get_Page_Title
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class)
      return String;
   --  Gets the title for Page.

   function Get_Page_Type
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class)
      return Gtk_Assistant_Page_Type;
   --  Gets the page type of Page.

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_Assistant.

   function Insert_Page
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class;
      Position  : Gint)
      return Gint;
   --  Assistant: a Gtk_Assistant
   --  Page: a Gtk_Widget
   --  Position: the index (starting at 0) at which to insert the page,
   --  or -1 to append the page to the Assistant
   --
   --  Inserts a page in the Assistant at a given position.
   --
   --  Return value: the index (starting from 0) of the inserted page

   function Prepend_Page
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class)
      return Gint;
   --  Prepends a page to the Assistant.
   --
   --  Return value: the index (starting at 0) of the inserted page

   procedure Remove_Action_Widget
     (Assistant : access Gtk_Assistant_Record;
      Child     : access Gtk_Widget_Record'Class);
   --  Removes a widget from the action area of a Gtk_Assistant.

   procedure Set_Current_Page
     (Assistant : access Gtk_Assistant_Record;
      Page_Num  : Gint);
   --  Assistant: a Gtk_Assistant
   --  Page_Num: index of the page to switch to, starting from 0.
   --  If negative, the last page will be used. If greater
   --  than the number of pages in the Assistant, nothing
   --  will be done.
   --
   --  Switches the page to Page_Num. Note that this will only be necessary
   --  in custom buttons, as the Assistant flow can be set with
   --  Set_Forward_Page_Func.

   generic
      type Data_Type (<>) is private;
   package Generic_Assistant_Functions is
      type Page_Func is access function
        (Current_Page : Gint;
         User_Data    : Data_Type)
         return Gint;
      --  Spec for page forwarding function.

      type Destroy_Notify is access procedure (User_Data : in out Data_Type);
      --  Destroy_Notify is called just prior to the destruction of
      --  User_Data.

      procedure Set_Forward_Page_Func
        (Assistant : Gtk_Assistant;
         Func      : Page_Func;
         User_Data : Data_Type;
         Destroy   : Destroy_Notify := null);
      --  Sets the Assistant's page forwarding function to be Func.  This
      --  function will be used to determine what will be the next page when
      --  the user presses the forward button. Setting Func to null will make
      --  the assistant use the default forward function, which just goes
      --  to the next visible page.
   end Generic_Assistant_Functions;

   procedure Set_Page_Complete
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class;
      Complete  : Boolean);
   --  Sets whether Page contents are complete. This will make
   --  Assistant update the buttons' state to be able to continue the task.

   procedure Set_Page_Header_Image
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class;
      Pixbuf    : Gdk_Pixbuf);
   --  Sets a header image for Page. This image is displayed in the header
   --  area of the assistant when Page is the current page.

   procedure Set_Page_Side_Image
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class;
      Pixbuf    : Gdk_Pixbuf);
   --  Sets a side image for Page. This image is displayed in the side
   --  area of the assistant when Page is the current page.

   procedure Set_Page_Title
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class;
      Title     : String);
   --  Sets a title for Page. The title is displayed in the header
   --  area of the assistant when Page is the current page.

   procedure Set_Page_Type
     (Assistant : access Gtk_Assistant_Record;
      Page      : access Gtk_Widget_Record'Class;
      Page_Type : Gtk_Assistant_Page_Type);
   --  Sets the page type for Page. The page type determines the page
   --  behavior in the Assistant.

   procedure Update_Buttons_State (Assistant : access Gtk_Assistant_Record);
   --  Forces Assistant to recompute the state of the buttons.
   --
   --  GTK+ automatically takes care of this in most situations,
   --  e.g. when the user goes to a different page, or when the
   --  visibility or completeness of a page changes.
   --
   --  One situation where it can be necessary to call this
   --  function is when changing a value on the current page
   --  affects the future page flow of the assistant.

   ----------------------
   -- Child Properties --
   ----------------------
   --  The following properties can be set on children of this widget. See
   --  in particular Gtk.Containers.Child_Set_Property.

   --  <child_properties>
   --  Name:  Complete_Property
   --  Type:  Boolean
   --  Descr: Whether all required fields on the page have been filled out
   --
   --  Name:  Header_Image_Property
   --  Type:  Object
   --  Descr: Header image for the assistant page
   --
   --  Name:  Page_Type_Property
   --  Type:  Enum
   --  Descr: The type of the assistant page
   --
   --  Name:  Sidebar_Image_Property
   --  Type:  Object
   --  Descr: Sidebar image for the assistant page
   --
   --  Name:  Title_Property
   --  Type:  String
   --  Descr: The title of the assistant page
   --  </child_properties>

   Complete_Property      : constant Glib.Properties.Property_Boolean;
   Header_Image_Property  : constant Glib.Properties.Property_Object;
   Page_Type_Property     : constant Glib.Properties.Property_Enum;
   Sidebar_Image_Property : constant Glib.Properties.Property_Object;
   Title_Property         : constant Glib.Properties.Property_String;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Content_Padding_Property
   --  Type:  Int
   --  Descr: Number of pixels around the content pages.
   --
   --  Name:  Header_Padding_Property
   --  Type:  Int
   --  Descr: Number of pixels around the header.
   --  </style_properties>

   Content_Padding_Property : constant Glib.Properties.Property_Int;
   Header_Padding_Property  : constant Glib.Properties.Property_Int;

private

   type Gtk_Assistant_Record is
     new Gtk.Window.Gtk_Window_Record with null record;

   pragma Import (C, Get_Type, "gtk_assistant_get_type");

   Complete_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("complete");
   Header_Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("header-image");
   Page_Type_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("page-type");
   Sidebar_Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("sidebar-image");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");

   Content_Padding_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("content-padding");
   Header_Padding_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("header-padding");

end Gtk.Assistant;
