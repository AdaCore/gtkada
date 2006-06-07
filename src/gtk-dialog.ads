-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  Dialog boxes are a convenient way to prompt the user for a small amount of
--  input, eg. to display a message, ask a question, or anything else that does
--  not require extensive effort on the user's part.
--
--  Gtkada treats a dialog as a window split horizontally. The top section is a
--  Gtk_Vbox, and is where widgets such as a Gtk_Label or a Gtk_Entry should be
--  packed. The second area is known as the action_area. This is generally used
--  for packing buttons into the dialog which may perform functions such as
--  cancel, ok, or apply. The two areas are separated by a Gtk_Hseparator.
--
--  If 'dialog' is a newly created dialog, the two primary areas of the window
--  can be accessed using Get_Vbox and Get_Action_Area as can be seen from the
--  example, below.
--
--  A 'modal' dialog (that is, one which freezes the rest of the application
--  from user input), can be created by calling Set_Modal on the dialog.
--
--  See Gtkada.Dialogs for a higher level dialog interface.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Windows</group>
--  <testgtk>create_dialog.adb</testgtk>

with Glib.Properties;
with Gtk.Box;
with Gtk.Widget;
with Gtk.Window;

package Gtk.Dialog is

   type Gtk_Dialog_Record is new Gtk.Window.Gtk_Window_Record with private;
   type Gtk_Dialog is access all Gtk_Dialog_Record'Class;

   -----------------------
   -- Enumeration types --
   -----------------------

   type Gtk_Dialog_Flags is mod 3;
   pragma Convention (C, Gtk_Dialog_Flags);
   Modal               : constant Gtk_Dialog_Flags := 0;
   Destroy_With_Parent : constant Gtk_Dialog_Flags := 1;
   No_Separator        : constant Gtk_Dialog_Flags := 2;
   --  Various flags that can be set for the dialog, with the following
   --  implications:
   --     - Modal : the dialog is modal, see Gtk.Window.Set_Modal
   --     - Destroy_With_Parent: The dialog is destroyed if its parent is
   --       destroyed. See Gtk.Window.Set_Destroy_With_Parent
   --     - No_Separator: No separator bar above the buttons.

   type Gtk_Response_Type is new Gint;
   --  Type used for Response_Id's.
   --  Positive values are totally user-interpreted.
   --  GtkAda will sometimes return Gtk_Response_None if no Response_Id is
   --  available.
   --
   --  Typical usage is:
   --    if Gtk.Dialog.Run (Dialog) = Gtk_Response_Accept then
   --       blah;
   --    end if;

   Gtk_Response_None : constant Gtk_Response_Type := -1;
   --  GtkAda returns this if a response widget has no Response_Id,
   --  or if the dialog gets programmatically hidden or destroyed.

   Gtk_Response_Reject : constant Gtk_Response_Type := -2;
   Gtk_Response_Accept : constant Gtk_Response_Type := -3;
   --  GtkAda won't return these unless you pass them in
   --  as the response for an action widget. They are
   --  for your convenience.

   Gtk_Response_Delete_Event : constant Gtk_Response_Type := -4;
   --  If the dialog is deleted through the button in the titlebar

   Gtk_Response_OK     : constant Gtk_Response_Type := -5;
   Gtk_Response_Cancel : constant Gtk_Response_Type := -6;
   Gtk_Response_Close  : constant Gtk_Response_Type := -7;
   Gtk_Response_Yes    : constant Gtk_Response_Type := -8;
   Gtk_Response_No     : constant Gtk_Response_Type := -9;
   Gtk_Response_Apply  : constant Gtk_Response_Type := -10;
   Gtk_Response_Help   : constant Gtk_Response_Type := -11;
   --  These are returned from dialogs, and you can also use them
   --  yourself if you like.

   type Response_Type_Array is array (Natural range <>) of Gtk_Response_Type;

   -----------------
   -- Subprograms --
   -----------------

   procedure Gtk_New (Dialog : out Gtk_Dialog);
   --  Create a new dialog.
   --  Widgets should not be packed into this widget directly, but into the
   --  vbox and action_area, as described above.

   procedure Gtk_New
     (Dialog : out Gtk_Dialog;
      Title  : UTF8_String;
      Parent : Gtk.Window.Gtk_Window;
      Flags  : Gtk_Dialog_Flags);
   --  Create a new dialog with a specific title, and specific attributes.
   --  Parent is the transient parent for the dialog (ie the one that is
   --  used for reference for the flag Destroy_With_Parent, or to compute the
   --  initial position of the dialog).

   procedure Initialize (Dialog : access Gtk_Dialog_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize
     (Dialog : access Gtk_Dialog_Record'Class;
      Title  : UTF8_String;
      Parent : Gtk.Window.Gtk_Window;
      Flags  : Gtk_Dialog_Flags);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Dialog.

   function Get_Action_Area
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   --  Return the action area box associated with a Dialog.

   function Get_Vbox
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   --  Return the vertical box associated with a Dialog.

   procedure Add_Action_Widget
     (Dialog      : access Gtk_Dialog_Record;
      Child       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Response_Id : Gtk_Response_Type);
   --  Add an activatable widget to the action area of Dialog.
   --  When the widget is activated (ie emits the "activate" signal), Dialog
   --  will emit the "response" signal with Response_Id.

   function Add_Button
     (Dialog      : access Gtk_Dialog_Record;
      Text        : UTF8_String;
      Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget;
   --  Add a button with the given text to the dialog. Note that you can also
   --  pass one of the constants defined in Gtk.Stock for the predefined
   --  buttons.
   --  When the button is clicked, Dialog will emit the "response" signal.
   --  The button widget is returned.

   function Get_Response_For_Widget
     (Dialog : access Gtk_Dialog_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Gets the response id of a widget in the action area of a dialog, or
   --  Gtk_Response_None if Widget doesn't have a response Id set

   procedure Set_Alternative_Button_Order_From_Array
     (Dialog    : access Gtk_Dialog_Record;
      New_Order : Response_Type_Array);
   --  Sets an alternative button order. If the gtk-alternative-button-order
   --  setting is set to %TRUE, the dialog buttons are reordered according to
   --  the order of the response ids passed to this function.
   --
   --  By default, GTK+ dialogs use the button order advocated by the Gnome
   --  Human Interface Guidelines with the affirmative button at the far right,
   --  and the cancel button left of it. But the builtin GTK+ dialogs and
   --  message dialogs' do provide an alternative button order, which is more
   --  suitable on some platforms, e.g. Windows.
   --
   --  Use this function after adding all the buttons to your dialog.

   function Gtk_Alternative_Dialog_Button_Order
     (Screen : Gdk.Gdk_Screen := null)  return Boolean;
   --  Returns True if dialogs are expected to use an alternative button order
   --  on the given screen (or current screen if null) . See
   --  Set_Alternative_Button_Order_From_Array for more details about
   --  alternative button order.
   --
   --  If you need to use this function, you should probably connect to the
   --  ::notify:gtk-alternative-button-order signal on the Gtk_Settings object
   --  associated to Screen, in order to be notified if the button order
   --  setting changes.
   --
   --  Returns: Whether the alternative button order should be used

   procedure Set_Response_Sensitive
     (Dialog      : access Gtk_Dialog_Record;
      Response_Id : Gtk_Response_Type;
      Setting     : Boolean);
   --  Call Gtk.Widget.Set_Sensitive for all the buttons in the dialog
   --  associated with Response_Id.

   procedure Set_Default_Response
     (Dialog : access Gtk_Dialog_Record; Response_Id : Gtk_Response_Type);
   --  Set the last widget in the dialog's action area with the given
   --  Response_Id as the default widget for Dialog.
   --  Pressing Enter will activate this default widget.

   procedure Set_Has_Separator
     (Dialog : access Gtk_Dialog_Record; Setting : Boolean);
   function Get_Has_Separator (Dialog : access Gtk_Dialog_Record)
      return Boolean;
   --  Set whether the dialog has a separator above the buttons.

   function Run (Dialog : access Gtk_Dialog_Record) return Gtk_Response_Type;
   --  Block in a recursive main loop until Dialog emits the "response"
   --  signal, or is destroyed. If the dialog is destroyed, Gtk_Response_None
   --  is returned. Otherwise, the response_id from the "response" signal is
   --  returned.
   --  Run will call Show on the dialog automatically. However, it is your
   --  responsability to call Show for any child you have inserted in the
   --  dialog.
   --  The dialog is automatically set to modal when this function is
   --  called. You can exit at any time from this function by emitting the
   --  "response" signal directly.
   --  When Run returns, you are responsible for hiding or destroying the
   --  dialog if necessary.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Has_Separator_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: The dialog has a separator bar above its buttons
   --    See also:
   --
   --  </properties>

   Has_Separator_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "response"
   --    procedure Handler
   --      (Dialog      : access Gtk_Dialog_Record'Class;
   --       Response_Id : Gint);
   --    Emitted when an action widget is clicked, the dialog receives a delete
   --    event, or the application programmer calls Response. On delete event,
   --    the response ID is GTK_RESPONSE_NONE. Otherwise, it depends on which
   --    action widget was clicked.
   --
   --  - "close"
   --    procedure Handler (Dialog : access Gtk_Dialog_Record'Class);
   --    Emit this signal to force a closing of the dialog.
   --  </signals>

   Signal_Close    : constant String := "close";
   Signal_Response : constant String := "response";

   procedure Response
     (Dialog      : access Gtk_Dialog_Record;
      Response_Id : Gtk_Response_Type);
   --  Emit the "response" signal

private
   type Gtk_Dialog_Record is new Gtk.Window.Gtk_Window_Record with null record;

   Has_Separator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has_separator");

   pragma Import (C, Get_Type, "gtk_dialog_get_type");
end Gtk.Dialog;

--  No binding: gtk_dialog_set_alternative_button_order
--  No binding: gtk_dialog_new_with_buttons
