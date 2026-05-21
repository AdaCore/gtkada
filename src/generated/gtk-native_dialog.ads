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

--  Base class for platform dialogs that don't use `GtkDialog`.
--
--  Native dialogs are used in order to integrate better with a platform, by
--  looking the same as other native applications and supporting platform
--  specific features.
--
--  The [classGtk.Dialog] functions cannot be used on such objects, but we
--  need a similar API in order to drive them. The `GtkNativeDialog` object is
--  an API that allows you to do this. It allows you to set various common
--  properties on the dialog, as well as show and hide it and get a
--  [signalGtk.NativeDialog::response] signal when the user finished with the
--  dialog.
--
--  Note that unlike `GtkDialog`, `GtkNativeDialog` objects are not toplevel
--  widgets, and GTK does not keep them alive. It is your responsibility to
--  keep a reference until you are done with the object.
--
--  <group>Dialogs</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Window;      use Gtk.Window;

package Gtk.Native_Dialog is

   type Gtk_Native_Dialog_Record is new GObject_Record with null record;
   type Gtk_Native_Dialog is access all Gtk_Native_Dialog_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_native_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Destroy (Self : not null access Gtk_Native_Dialog_Record);
   --  Destroys a dialog.
   --  When a dialog is destroyed, it will break any references it holds to
   --  other objects.
   --  If it is visible it will be hidden and any underlying window system
   --  resources will be destroyed.
   --  Note that this does not release any reference to the object (as opposed
   --  to destroying a `GtkWindow`) because there is no reference from the
   --  windowing system to the `GtkNativeDialog`.

   function Get_Modal
      (Self : not null access Gtk_Native_Dialog_Record) return Boolean;
   --  Returns whether the dialog is modal.
   --  @return True if the dialog is set to be modal

   procedure Set_Modal
      (Self  : not null access Gtk_Native_Dialog_Record;
       Modal : Boolean);
   --  Sets a dialog modal or non-modal.
   --  Modal dialogs prevent interaction with other windows in the same
   --  application. To keep modal dialogs on top of main application windows,
   --  use [methodGtk.NativeDialog.set_transient_for] to make the dialog
   --  transient for the parent; most window managers will then disallow
   --  lowering the dialog below the parent.
   --  @param Modal whether the window is modal

   function Get_Title
      (Self : not null access Gtk_Native_Dialog_Record) return UTF8_String;
   --  Gets the title of the `GtkNativeDialog`.
   --  @return the title of the dialog, or null if none has been set
   --  explicitly. The returned string is owned by the widget and must not be
   --  modified or freed.

   procedure Set_Title
      (Self  : not null access Gtk_Native_Dialog_Record;
       Title : UTF8_String);
   --  Sets the title of the `GtkNativeDialog.`
   --  @param Title title of the dialog

   function Get_Transient_For
      (Self : not null access Gtk_Native_Dialog_Record)
       return Gtk.Window.Gtk_Window;
   --  Fetches the transient parent for this window.
   --  @return the transient parent for this window, or null if no transient
   --  parent has been set.

   procedure Set_Transient_For
      (Self   : not null access Gtk_Native_Dialog_Record;
       Parent : access Gtk.Window.Gtk_Window_Record'Class);
   --  Dialog windows should be set transient for the main application window
   --  they were spawned from.
   --  This allows window managers to e.g. keep the dialog on top of the main
   --  window, or center the dialog over the main window.
   --  Passing null for Parent unsets the current transient window.
   --  @param Parent parent window

   function Get_Visible
      (Self : not null access Gtk_Native_Dialog_Record) return Boolean;
   --  Determines whether the dialog is visible.
   --  @return True if the dialog is visible

   procedure Hide (Self : not null access Gtk_Native_Dialog_Record);
   --  Hides the dialog if it is visible, aborting any interaction.
   --  Once this is called the [signalGtk.NativeDialog::response] signal will
   --  *not* be emitted until after the next call to
   --  [methodGtk.NativeDialog.show].
   --  If the dialog is not visible this does nothing.

   procedure Show (Self : not null access Gtk_Native_Dialog_Record);
   --  Shows the dialog on the display.
   --  When the user accepts the state of the dialog the dialog will be
   --  automatically hidden and the [signalGtk.NativeDialog::response] signal
   --  will be emitted.
   --  Multiple calls while the dialog is visible will be ignored.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window should be modal with respect to its transient
   --  parent.

   Title_Property : constant Glib.Properties.Property_String;
   --  The title of the dialog window

   Transient_For_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Window.Gtk_Window
   --  The transient parent of the dialog, or null for none.

   Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window is currently visible.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Native_Dialog_Gint_Void is not null access procedure
     (Self        : access Gtk_Native_Dialog_Record'Class;
      Response_Id : Glib.Gint);

   type Cb_GObject_Gint_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Response_Id : Glib.Gint);

   Signal_Response : constant Glib.Signal_Name := "response";
   procedure On_Response
      (Self  : not null access Gtk_Native_Dialog_Record;
       Call  : Cb_Gtk_Native_Dialog_Gint_Void;
       After : Boolean := False);
   procedure On_Response
      (Self  : not null access Gtk_Native_Dialog_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user responds to the dialog.
   --
   --  When this is called the dialog has been hidden.
   --
   --  If you call [methodGtk.NativeDialog.hide] before the user responds to
   --  the dialog this signal will not be emitted.

private
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Transient_For_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("transient-for");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
end Gtk.Native_Dialog;
