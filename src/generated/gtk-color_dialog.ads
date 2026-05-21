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

--  Asynchronous API to present a color chooser dialog.
--
--  `GtkColorDialog` collects the arguments that are needed to present the
--  dialog to the user, such as a title for the dialog and whether it should be
--  modal.
--
--  The dialog is shown with the [methodGtk.ColorDialog.choose_rgba] function.
--
--  See [classGtk.ColorDialogButton] for a convenient control that uses
--  `GtkColorDialog` and presents the results.
--
--  <group>Dialogs</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;         use Gdk.RGBA;
with Glib;             use Glib;
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Gtk.Window;       use Gtk.Window;

package Gtk.Color_Dialog is

   type Gtk_Color_Dialog_Record is new GObject_Record with null record;
   type Gtk_Color_Dialog is access all Gtk_Color_Dialog_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the [thread-default main
   --  context][g-main-context-push-thread-default] where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Color_Dialog);
   procedure Initialize
      (Self : not null access Gtk_Color_Dialog_Record'Class);
   --  Creates a new `GtkColorDialog` object.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Color_Dialog_New return Gtk_Color_Dialog;
   --  Creates a new `GtkColorDialog` object.
   --  Since: gtk+ 4.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Choose_Rgba
      (Self          : not null access Gtk_Color_Dialog_Record;
       Parent        : access Gtk.Window.Gtk_Window_Record'Class;
       Initial_Color : Gdk.RGBA.Gdk_RGBA;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback);
   --  Presents a color chooser dialog to the user.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Initial_Color the color to select initially
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Choose_Rgba_Finish
      (Self   : not null access Gtk_Color_Dialog_Record;
       Result : Glib.G_Async_Result) return Gdk.RGBA.Gdk_RGBA;
   --  Finishes the [methodGtk.ColorDialog.choose_rgba] call
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @return the selected color

   function Get_Modal
      (Self : not null access Gtk_Color_Dialog_Record) return Boolean;
   --  Returns whether the color chooser dialog blocks interaction with the
   --  parent window while it is presented.
   --  Since: gtk+ 4.10
   --  @return true if the color chooser dialog is modal

   procedure Set_Modal
      (Self  : not null access Gtk_Color_Dialog_Record;
       Modal : Boolean);
   --  Sets whether the color chooser dialog blocks interaction with the
   --  parent window while it is presented.
   --  Since: gtk+ 4.10
   --  @param Modal the new value

   function Get_Title
      (Self : not null access Gtk_Color_Dialog_Record) return UTF8_String;
   --  Returns the title that will be shown on the color chooser dialog.
   --  Since: gtk+ 4.10
   --  @return the title

   procedure Set_Title
      (Self  : not null access Gtk_Color_Dialog_Record;
       Title : UTF8_String);
   --  Sets the title that will be shown on the color chooser dialog.
   --  Since: gtk+ 4.10
   --  @param Title the new title

   function Get_With_Alpha
      (Self : not null access Gtk_Color_Dialog_Record) return Boolean;
   --  Returns whether colors may have alpha.
   --  Since: gtk+ 4.10
   --  @return true if colors may have alpha

   procedure Set_With_Alpha
      (Self       : not null access Gtk_Color_Dialog_Record;
       With_Alpha : Boolean);
   --  Sets whether colors may have alpha.
   --  Since: gtk+ 4.10
   --  @param With_Alpha the new value

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the color chooser dialog is modal.

   Title_Property : constant Glib.Properties.Property_String;
   --  A title that may be shown on the color chooser dialog.

   With_Alpha_Property : constant Glib.Properties.Property_Boolean;
   --  Whether colors may have alpha (translucency).
   --
   --  When with-alpha is false, the color that is selected will be forced to
   --  have alpha == 1.

private
   With_Alpha_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("with-alpha");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
end Gtk.Color_Dialog;
