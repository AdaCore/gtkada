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

--  Interface for single-line text editing widgets.
--
--  Typical examples of editable widgets are [classGtk.Entry] and
--  [classGtk.SpinButton]. It contains functions for generically manipulating
--  an editable widget, a large number of action signals used for key bindings,
--  and several signals that an application can connect to modify the behavior
--  of a widget.
--
--  As an example of the latter usage, by connecting the following handler to
--  [signalGtk.Editable::insert-text], an application can convert all entry
--  into a widget into uppercase.
--
--  ## Forcing entry to uppercase.
--
--  ```c include <ctype.h>
--
--  void insert_text_handler (GtkEditable *editable, const char *text, int
--  length, int *position, gpointer data) { char *result = g_utf8_strup (text,
--  length);
--
--  g_signal_handlers_block_by_func (editable, (gpointer) insert_text_handler,
--  data); gtk_editable_insert_text (editable, result, length, position);
--  g_signal_handlers_unblock_by_func (editable, (gpointer)
--  insert_text_handler, data);
--
--  g_signal_stop_emission_by_name (editable, "insert_text");
--
--  g_free (result); } ```
--
--  ## Implementing GtkEditable
--
--  The most likely scenario for implementing `GtkEditable` on your own widget
--  is that you will embed a `GtkText` inside a complex widget, and want to
--  delegate the editable functionality to that text widget. `GtkEditable`
--  provides some utility functions to make this easy.
--
--  In your class_init function, call [funcGtk.Editable.install_properties],
--  passing the first available property ID:
--
--  ```c static void my_class_init (MyClass *class) { ...
--  g_object_class_install_properties (object_class, NUM_PROPERTIES, props);
--  gtk_editable_install_properties (object_clas, NUM_PROPERTIES); ... } ```
--
--  In your interface_init function for the `GtkEditable` interface, provide
--  an implementation for the get_delegate vfunc that returns your text widget:
--
--  ```c GtkEditable * get_editable_delegate (GtkEditable *editable) { return
--  GTK_EDITABLE (MY_WIDGET (editable)->text_widget); }
--
--  static void my_editable_init (GtkEditableInterface *iface) {
--  iface->get_delegate = get_editable_delegate; } ```
--
--  You don't need to provide any other vfuncs. The default implementations
--  work by forwarding to the delegate that the
--  GtkEditableInterface.get_delegate vfunc returns.
--
--  In your instance_init function, create your text widget, and then call
--  [methodGtk.Editable.init_delegate]:
--
--  ```c static void my_widget_init (MyWidget *self) { ... self->text_widget =
--  gtk_text_new (); gtk_editable_init_delegate (GTK_EDITABLE (self)); ... }
--  ```
--
--  In your dispose function, call [methodGtk.Editable.finish_delegate] before
--  destroying your text widget:
--
--  ```c static void my_widget_dispose (GObject *object) { ...
--  gtk_editable_finish_delegate (GTK_EDITABLE (self)); g_clear_pointer
--  (&self->text_widget, gtk_widget_unparent); ... } ```
--
--  Finally, use [funcGtk.Editable.delegate_set_property] in your
--  `set_property` function (and similar for `get_property`), to set the
--  editable properties:
--
--  ```c ... if (gtk_editable_delegate_set_property (object, prop_id, value,
--  pspec)) return;
--
--  switch (prop_id) ... ```
--
--  It is important to note that if you create a `GtkEditable` that uses a
--  delegate, the low level [signalGtk.Editable::insert-text] and
--  [signalGtk.Editable::delete-text] signals will be propagated from the
--  "wrapper" editable to the delegate, but they will not be propagated from
--  the delegate to the "wrapper" editable, as they would cause an infinite
--  recursion. If you wish to connect to the [signalGtk.Editable::insert-text]
--  and [signalGtk.Editable::delete-text] signals, you will need to connect to
--  them on the delegate obtained via [methodGtk.Editable.get_delegate].

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Values;     use Glib.Values;
with Gtk.Accessible;  use Gtk.Accessible;
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
with Interfaces.C;    use Interfaces.C;

package Gtk.Editable is

   type Gtk_Editable is new Glib.Types.GType_Interface;
   Null_Gtk_Editable : constant Gtk_Editable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_editable_get_type");

   -------------
   -- Methods --
   -------------

   function Delegate_Get_Accessible_Platform_State
      (Self  : Gtk_Editable;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;
   --  Retrieves the accessible platform state from the editable delegate.
   --  This is an helper function to retrieve the accessible state for
   --  `GtkEditable` interface implementations using a delegate pattern.
   --  You should call this function in your editable widget implementation of
   --  the [vfuncGtk.Accessible.get_platform_state] virtual function, for
   --  instance:
   --  ```c static void accessible_interface_init (GtkAccessibleInterface
   --  *iface) { iface->get_platform_state =
   --  your_editable_get_accessible_platform_state; }
   --  static gboolean your_editable_get_accessible_platform_state
   --  (GtkAccessible *accessible, GtkAccessiblePlatformState state) { return
   --  gtk_editable_delegate_get_accessible_platform_state (GTK_EDITABLE
   --  (accessible), state); } ```
   --  Note that the widget which is the delegate *must* be a direct child of
   --  this widget, otherwise your implementation of
   --  [vfuncGtk.Accessible.get_platform_state] might not even be called, as
   --  the platform change will originate from the parent of the delegate, and,
   --  as a result, will not work properly.
   --  So, if you can't ensure the direct child condition, you should give the
   --  delegate the Gtk.Accessible.Accessible_Role_Text_Box role, or you can
   --  change your tree to allow this function to work.
   --  Since: gtk+ 4.10
   --  @param State what kind of accessible state to retrieve
   --  @return the accessible platform state of the delegate

   procedure Delete_Selection (Self : Gtk_Editable);
   pragma Import (C, Delete_Selection, "gtk_editable_delete_selection");
   --  Deletes the currently selected text of the editable.
   --  This call doesn't do anything if there is no selected text.

   procedure Delete_Text
      (Self      : Gtk_Editable;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);
   pragma Import (C, Delete_Text, "gtk_editable_delete_text");
   --  Deletes a sequence of characters.
   --  The characters that are deleted are those characters at positions from
   --  Start_Pos up to, but not including End_Pos. If End_Pos is negative, then
   --  the characters deleted are those from Start_Pos to the end of the text.
   --  Note that the positions are specified in characters, not bytes.
   --  @param Start_Pos start position
   --  @param End_Pos end position

   procedure Finish_Delegate (Self : Gtk_Editable);
   pragma Import (C, Finish_Delegate, "gtk_editable_finish_delegate");
   --  Undoes the setup done by [methodGtk.Editable.init_delegate].
   --  This is a helper function that should be called from dispose, before
   --  removing the delegate object.

   function Get_Alignment (Self : Gtk_Editable) return Interfaces.C.C_float;
   pragma Import (C, Get_Alignment, "gtk_editable_get_alignment");
   --  Gets the alignment of the editable.
   --  @return the alignment

   procedure Set_Alignment
      (Self   : Gtk_Editable;
       Xalign : Interfaces.C.C_float);
   pragma Import (C, Set_Alignment, "gtk_editable_set_alignment");
   --  Sets the alignment for the contents of the editable.
   --  This controls the horizontal positioning of the contents when the
   --  displayed text is shorter than the width of the editable.
   --  @param Xalign The horizontal alignment, from 0 (left) to 1 (right).
   --  Reversed for RTL layouts

   function Get_Chars
      (Self      : Gtk_Editable;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;
   --  Retrieves a sequence of characters.
   --  The characters that are retrieved are those characters at positions
   --  from Start_Pos up to, but not including End_Pos. If End_Pos is negative,
   --  then the characters retrieved are those characters from Start_Pos to the
   --  end of the text.
   --  Note that positions are specified in characters, not bytes.
   --  @param Start_Pos start of text
   --  @param End_Pos end of text
   --  @return a pointer to the contents of the widget as a string. This
   --  string is allocated by the `GtkEditable` implementation and should be
   --  freed by the caller.

   function Get_Delegate (Self : Gtk_Editable) return Gtk_Editable;
   pragma Import (C, Get_Delegate, "gtk_editable_get_delegate");
   --  Gets the `GtkEditable` that Editable is delegating its implementation
   --  to.
   --  Typically, the delegate is a [classGtk.Text] widget.
   --  @return the delegate `GtkEditable`

   function Get_Editable (Self : Gtk_Editable) return Boolean;
   --  Retrieves whether Editable is editable.
   --  @return True if Editable is editable.

   procedure Set_Editable (Self : Gtk_Editable; Is_Editable : Boolean);
   --  Determines if the user can edit the text in the editable widget.
   --  @param Is_Editable True if the user is allowed to edit the text in the
   --  widget

   function Get_Enable_Undo (Self : Gtk_Editable) return Boolean;
   --  Gets if undo/redo actions are enabled for Editable
   --  @return True if undo is enabled

   procedure Set_Enable_Undo (Self : Gtk_Editable; Enable_Undo : Boolean);
   --  If enabled, changes to Editable will be saved for undo/redo actions.
   --  This results in an additional copy of text changes and are not stored
   --  in secure memory. As such, undo is forcefully disabled when
   --  [propertyGtk.Text:visibility] is set to False.
   --  @param Enable_Undo if undo/redo should be enabled

   function Get_Max_Width_Chars (Self : Gtk_Editable) return Glib.Gint;
   pragma Import (C, Get_Max_Width_Chars, "gtk_editable_get_max_width_chars");
   --  Retrieves the desired maximum width of Editable, in characters.
   --  @return the maximum width of the entry, in characters

   procedure Set_Max_Width_Chars (Self : Gtk_Editable; N_Chars : Glib.Gint);
   pragma Import (C, Set_Max_Width_Chars, "gtk_editable_set_max_width_chars");
   --  Sets the desired maximum width in characters of Editable.
   --  @param N_Chars the new desired maximum width, in characters

   function Get_Position (Self : Gtk_Editable) return Glib.Gint;
   pragma Import (C, Get_Position, "gtk_editable_get_position");
   --  Retrieves the current position of the cursor relative to the start of
   --  the content of the editable.
   --  Note that this position is in characters, not in bytes.
   --  @return the cursor position

   procedure Set_Position (Self : Gtk_Editable; Position : Glib.Gint);
   pragma Import (C, Set_Position, "gtk_editable_set_position");
   --  Sets the cursor position in the editable to the given value.
   --  The cursor is displayed before the character with the given (base 0)
   --  index in the contents of the editable. The value must be less than or
   --  equal to the number of characters in the editable. A value of -1
   --  indicates that the position should be set after the last character of
   --  the editable. Note that Position is in characters, not in bytes.
   --  @param Position the position of the cursor

   procedure Get_Selection_Bounds
      (Self          : Gtk_Editable;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);
   --  Retrieves the selection bound of the editable.
   --  Start_Pos will be filled with the start of the selection and End_Pos
   --  with end. If no text was selected both will be identical and False will
   --  be returned.
   --  Note that positions are specified in characters, not bytes.
   --  @param Start_Pos location to store the starting position
   --  @param End_Pos location to store the end position
   --  @return True if there is a non-empty selection, False otherwise

   function Get_Text (Self : Gtk_Editable) return UTF8_String;
   --  Retrieves the contents of Editable.
   --  The returned string is owned by GTK and must not be modified or freed.
   --  @return a pointer to the contents of the editable

   procedure Set_Text (Self : Gtk_Editable; Text : UTF8_String);
   --  Sets the text in the editable to the given value.
   --  This is replacing the current contents.
   --  @param Text the text to set

   function Get_Width_Chars (Self : Gtk_Editable) return Glib.Gint;
   pragma Import (C, Get_Width_Chars, "gtk_editable_get_width_chars");
   --  Gets the number of characters of space reserved for the contents of the
   --  editable.
   --  @return number of chars to request space for, or negative if unset

   procedure Set_Width_Chars (Self : Gtk_Editable; N_Chars : Glib.Gint);
   pragma Import (C, Set_Width_Chars, "gtk_editable_set_width_chars");
   --  Changes the size request of the editable to be about the right size for
   --  N_Chars characters.
   --  Note that it changes the size request, the size can still be affected
   --  by how you pack the widget into containers. If N_Chars is -1, the size
   --  reverts to the default size.
   --  @param N_Chars width in chars

   procedure Init_Delegate (Self : Gtk_Editable);
   pragma Import (C, Init_Delegate, "gtk_editable_init_delegate");
   --  Sets up a delegate for `GtkEditable`.
   --  This is assuming that the get_delegate vfunc in the `GtkEditable`
   --  interface has been set up for the Editable's type.
   --  This is a helper function that should be called in instance init, after
   --  creating the delegate object.

   procedure Insert_Text
      (Self     : Gtk_Editable;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint);
   --  Inserts Length bytes of Text into the contents of the widget, at
   --  position Position.
   --  Note that the position is in characters, not in bytes. The function
   --  updates Position to point after the newly inserted text.
   --  @param Text the text to insert
   --  @param Length the length of the text in bytes, or -1
   --  @param Position location of the position text will be inserted at

   procedure Select_Region
      (Self      : Gtk_Editable;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);
   pragma Import (C, Select_Region, "gtk_editable_select_region");
   --  Selects a region of text.
   --  The characters that are selected are those characters at positions from
   --  Start_Pos up to, but not including End_Pos. If End_Pos is negative, then
   --  the characters selected are those characters from Start_Pos to the end
   --  of the text.
   --  Note that positions are specified in characters, not bytes.
   --  @param Start_Pos start of region
   --  @param End_Pos end of region

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Insert_Text
     (Editable : Gtk_Editable;
      New_Text : UTF8_String;
      Position : in out Glib.Gint);
   --  Convenience subprogram, identical to Insert_Text above without
   --  the requirement to supply the New_Text_Length argument.

   ---------------
   -- Functions --
   ---------------

   function Delegate_Get_Property
      (Object  : not null access Glib.Object.GObject_Record'Class;
       Prop_Id : Guint;
       Value   : in out Glib.Values.GValue;
       Pspec   : in out Glib.Param_Spec) return Boolean;
   --  Gets a property of the `GtkEditable` delegate for Object.
   --  This is helper function that should be called in the `get_property`
   --  function of your `GtkEditable` implementation, before handling your own
   --  properties.
   --  @param Object a `GObject`
   --  @param Prop_Id a property ID
   --  @param Value value to set
   --  @param Pspec the `GParamSpec` for the property
   --  @return True if the property was found

   function Delegate_Set_Property
      (Object  : not null access Glib.Object.GObject_Record'Class;
       Prop_Id : Guint;
       Value   : in out Glib.Values.GValue;
       Pspec   : in out Glib.Param_Spec) return Boolean;
   --  Sets a property on the `GtkEditable` delegate for Object.
   --  This is a helper function that should be called in the `set_property`
   --  function of your `GtkEditable` implementation, before handling your own
   --  properties.
   --  @param Object a `GObject`
   --  @param Prop_Id a property ID
   --  @param Value value to set
   --  @param Pspec the `GParamSpec` for the property
   --  @return True if the property was found

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cursor_Position_Property : constant Glib.Properties.Property_Int;
   --  The current position of the insertion cursor in chars.

   Editable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the entry contents can be edited.

   Enable_Undo_Property : constant Glib.Properties.Property_Boolean;
   --  If undo/redo should be enabled for the editable.

   Max_Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The desired maximum width of the entry, in characters.

   Selection_Bound_Property : constant Glib.Properties.Property_Int;
   --  The position of the opposite end of the selection from the cursor in
   --  chars.

   Text_Property : constant Glib.Properties.Property_String;
   --  The contents of the entry.

   Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  Number of characters to leave space for in the entry.

   Xalign_Property : constant Glib.Properties.Property_Float;
   --  The horizontal alignment, from 0 (left) to 1 (right).
   --
   --  Reversed for RTL layouts.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Editable_Void is not null access procedure (Self : Gtk_Editable);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted at the end of a single user-visible operation on the contents.
   --
   --  E.g., a paste operation that replaces the contents of the selection
   --  will cause only one signal emission (even though it is implemented by
   --  first deleting the selection, then inserting the new content, and may
   --  cause multiple ::notify::text signals to be emitted).

   type Cb_Gtk_Editable_Gint_Gint_Void is not null access procedure
     (Self      : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);

   type Cb_GObject_Gint_Gint_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);

   Signal_Delete_Text : constant Glib.Signal_Name := "delete-text";
   procedure On_Delete_Text
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Delete_Text
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when text is deleted from the widget by the user.
   --
   --  The default handler for this signal will normally be responsible for
   --  deleting the text, so by connecting to this signal and then stopping the
   --  signal with g_signal_stop_emission, it is possible to modify the range
   --  of deleted text, or prevent it from being deleted entirely.
   --
   --  The Start_Pos and End_Pos parameters are interpreted as for
   --  [methodGtk.Editable.delete_text].
   -- 
   --  Callback parameters:
   --    --  @param Start_Pos the starting position
   --    --  @param End_Pos the end position

   type Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void is not null access procedure
     (Self     : Gtk_Editable;
      Text     : UTF8_String;
      Length   : Glib.Gint;
      Position : access Glib.Gint);

   type Cb_GObject_UTF8_String_Gint_Gint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Text     : UTF8_String;
      Length   : Glib.Gint;
      Position : access Glib.Gint);

   Signal_Insert_Text : constant Glib.Signal_Name := "insert-text";
   procedure On_Insert_Text
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Insert_Text
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_UTF8_String_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when text is inserted into the widget by the user.
   --
   --  The default handler for this signal will normally be responsible for
   --  inserting the text, so by connecting to this signal and then stopping
   --  the signal with g_signal_stop_emission, it is possible to modify the
   --  inserted text, or prevent it from being inserted entirely.
   -- 
   --  Callback parameters:
   --    --  @param Text the new text to insert
   --    --  @param Length the length of the new text, in bytes, or -1 if new_text
   --    --  is nul-terminated
   --    --  @param Position the position, in characters, at which to insert the new
   --    --  text. this is an in-out parameter. After the signal emission is
   --    --  finished, it should point after the newly inserted text.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Editable"

   function "+" (W : Gtk_Editable) return Gtk_Editable;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Changed is access procedure (Self : Gtk_Editable);
   pragma Convention (C, Virtual_Changed);

   type Virtual_Delete_Text is access procedure
     (Self      : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);
   pragma Convention (C, Virtual_Delete_Text);
   --  Deletes a sequence of characters.
   --  The characters that are deleted are those characters at positions from
   --  Start_Pos up to, but not including End_Pos. If End_Pos is negative, then
   --  the characters deleted are those from Start_Pos to the end of the text.
   --  Note that the positions are specified in characters, not bytes.
   --  @param Start_Pos start position
   --  @param End_Pos end position

   type Virtual_Do_Delete_Text is access procedure
     (Self      : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);
   pragma Convention (C, Virtual_Do_Delete_Text);
   --  Deletes a sequence of characters.
   --  The characters that are deleted are those characters at positions from
   --  Start_Pos up to, but not including End_Pos. If End_Pos is negative, then
   --  the characters deleted are those from Start_Pos to the end of the text.
   --  Note that the positions are specified in characters, not bytes.
   --  @param Start_Pos start position
   --  @param End_Pos end position

   type Virtual_Do_Insert_Text is access procedure
     (Self     : Gtk_Editable;
      Text     : Gtkada.Types.Chars_Ptr;
      Length   : Glib.Gint;
      Position : in out Glib.Gint);
   pragma Convention (C, Virtual_Do_Insert_Text);
   --  Inserts Length bytes of Text into the contents of the widget, at
   --  position Position.
   --  Note that the position is in characters, not in bytes. The function
   --  updates Position to point after the newly inserted text.
   --  @param Text the text to insert
   --  @param Length the length of the text in bytes, or -1
   --  @param Position location of the position text will be inserted at

   type Virtual_Get_Delegate is access function (Self : Gtk_Editable) return Gtk_Editable;
   pragma Convention (C, Virtual_Get_Delegate);
   --  Gets the `GtkEditable` that Editable is delegating its implementation
   --  to.
   --  Typically, the delegate is a [classGtk.Text] widget.
   --  @return the delegate `GtkEditable`

   type Virtual_Get_Selection_Bounds is access function
     (Self      : Gtk_Editable;
      Start_Pos : access Glib.Gint;
      End_Pos   : access Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Selection_Bounds);
   --  Retrieves the selection bound of the editable.
   --  Start_Pos will be filled with the start of the selection and End_Pos
   --  with end. If no text was selected both will be identical and False will
   --  be returned.
   --  Note that positions are specified in characters, not bytes.
   --  @param Start_Pos location to store the starting position
   --  @param End_Pos location to store the end position
   --  @return True if there is a non-empty selection, False otherwise

   type Virtual_Get_Text is access function (Self : Gtk_Editable) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Text);
   --  Retrieves the contents of Editable.
   --  The returned string is owned by GTK and must not be modified or freed.
   --  @return a pointer to the contents of the editable

   type Virtual_Insert_Text is access procedure
     (Self     : Gtk_Editable;
      Text     : Gtkada.Types.Chars_Ptr;
      Length   : Glib.Gint;
      Position : in out Glib.Gint);
   pragma Convention (C, Virtual_Insert_Text);
   --  Inserts Length bytes of Text into the contents of the widget, at
   --  position Position.
   --  Note that the position is in characters, not in bytes. The function
   --  updates Position to point after the newly inserted text.
   --  @param Text the text to insert
   --  @param Length the length of the text in bytes, or -1
   --  @param Position location of the position text will be inserted at

   type Virtual_Set_Selection_Bounds is access procedure
     (Self      : Gtk_Editable;
      Start_Pos : Glib.Gint;
      End_Pos   : Glib.Gint);
   pragma Convention (C, Virtual_Set_Selection_Bounds);
   --  Selects a region of text.
   --  The characters that are selected are those characters at positions from
   --  Start_Pos up to, but not including End_Pos. If End_Pos is negative, then
   --  the characters selected are those characters from Start_Pos to the end
   --  of the text.
   --  Note that positions are specified in characters, not bytes.
   --  @param Start_Pos start of region
   --  @param End_Pos end of region

   subtype Editable_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Changed
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Changed);
   pragma Import (C, Set_Changed, "gtkada_Editable_set_changed");

   procedure Set_Delete_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Delete_Text);
   pragma Import (C, Set_Delete_Text, "gtkada_Editable_set_delete_text");

   procedure Set_Do_Delete_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Do_Delete_Text);
   pragma Import (C, Set_Do_Delete_Text, "gtkada_Editable_set_do_delete_text");

   procedure Set_Do_Insert_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Do_Insert_Text);
   pragma Import (C, Set_Do_Insert_Text, "gtkada_Editable_set_do_insert_text");

   procedure Set_Get_Delegate
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Get_Delegate);
   pragma Import (C, Set_Get_Delegate, "gtkada_Editable_set_get_delegate");

   procedure Set_Get_Selection_Bounds
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Get_Selection_Bounds);
   pragma Import (C, Set_Get_Selection_Bounds, "gtkada_Editable_set_get_selection_bounds");

   procedure Set_Get_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Get_Text);
   pragma Import (C, Set_Get_Text, "gtkada_Editable_set_get_text");

   procedure Set_Insert_Text
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Insert_Text);
   pragma Import (C, Set_Insert_Text, "gtkada_Editable_set_insert_text");

   procedure Set_Set_Selection_Bounds
     (Self    : Editable_Interface_Descr;
      Handler : Virtual_Set_Selection_Bounds);
   pragma Import (C, Set_Set_Selection_Bounds, "gtkada_Editable_set_set_selection_bounds");
   --  See Glib.Object.Add_Interface

private
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Selection_Bound_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("selection-bound");
   Max_Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width-chars");
   Enable_Undo_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-undo");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");

Null_Gtk_Editable : constant Gtk_Editable :=
   Gtk_Editable (Glib.Types.Null_Interface);
end Gtk.Editable;
