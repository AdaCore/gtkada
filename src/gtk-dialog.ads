-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  @pxref{Package_Gtkada.Dialogs} for a higher level dialog interface.
--  </description>
--  <c_version>1.3.6</c_version>

with Glib.Properties;
with Gtk.Box;
with Gtk.Window;

package Gtk.Dialog is

   type Gtk_Dialog_Record is new Gtk.Window.Gtk_Window_Record with private;
   type Gtk_Dialog is access all Gtk_Dialog_Record'Class;

   -----------------------
   -- Enumeration types --
   -----------------------

   type Gtk_Dialog_Flags is
     (Gtk_Dialog_Modal,
      --  Call Gtk.Window.Set_Modal (Win, True)

      Gtk_Dialog_Destroy_With_Parent,
      --  Call Gtk.Window.Set_Destroy_With_Parent

      Gtk_Dialog_No_Separator
      --  No separator bar above buttons
     );
   --  Parameters for dialog construction

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
   --  If the dialog is deleted.

   Gtk_Response_OK     : constant Gtk_Response_Type := -5;
   Gtk_Response_Cancel : constant Gtk_Response_Type := -6;
   Gtk_Response_Close  : constant Gtk_Response_Type := -7;
   Gtk_Response_Yes    : constant Gtk_Response_Type := -8;
   Gtk_Response_No     : constant Gtk_Response_Type := -9;
   Gtk_Response_Apply  : constant Gtk_Response_Type := -10;
   Gtk_Response_Help   : constant Gtk_Response_Type := -11;
   --  These are returned from dialogs, and you can also use them
   --  yourself if you like.

   -----------------
   -- Subprograms --
   -----------------

   procedure Gtk_New (Dialog : out Gtk_Dialog);
   --  Create a new dialog.
   --  Widgets should not be packed into this widget directly, but into the
   --  vbox and action_area, as described above.

   procedure Initialize (Dialog : access Gtk_Dialog_Record'Class);
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
   --      (Dialog      : access Gtk_Fialog_Record'Class;
   --       Response_Id : Gint);
   --
   --    The user has selected a response for the dialog.
   --  </signals>

private
   type Gtk_Dialog_Record is new Gtk.Window.Gtk_Window_Record with null record;

   Has_Separator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has_separator");

   pragma Import (C, Get_Type, "gtk_dialog_get_type");
end Gtk.Dialog;

--  missing:
--  GtkWidget* Gtk_New
--    (const gchar     *title,
--     GtkWindow       *parent,
--     GtkDialogFlags   flags,
--     const gchar     *first_button_text,
--     ...);

--  procedure add_action_widget
--    (GtkDialog   *dialog,
--     GtkWidget   *child,
--     Gtk_Response_Type response_id);

--  GtkWidget* add_button
--    (GtkDialog   *dialog,
--     const gchar *button_text,
--     Gtk_Response_Type response_id);

--  procedure add_buttons
--    (GtkDialog   *dialog,
--     const gchar *first_button_text,
--     ...);

--  procedure set_response_sensitive
--    (GtkDialog *dialog,
--     Gtk_Response_Type response_id,
--     gboolean   setting);

--  procedure set_default_response
--    (GtkDialog *dialog,
--     Gtk_Response_Type response_id);

--  procedure set_has_separator
--    (GtkDialog *dialog,
--     gboolean   setting);

--  gboolean get_has_separator (GtkDialog *dialog);

--  procedure Response
--    (GtkDialog *dialog,
--     Gtk_Response_Type response_id);
--  Emit response signal

--  function Run (GtkDialog *dialog) return Gtk_Response_Type;
--  Return response_id

