------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2022, AdaCore                     --
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
--  This package is obsolete and replaced by Glib.Values.
--  Future versions of GtkAda will no longer provide this package.
--
--  This package provides a convenient interface to C, providing easy
--  conversion from a C's (void*) pointer to any Ada type used in
--  GtkAda.  Although this package has been designed to be easily
--  reusable by being as general as possible, these functions are mainly
--  used when writing callbacks and/or marshallers (see Gtk.Marshallers
--  and Gtk.Handlers).
--
--  Therefore, the main type in this package is Gtk_Args, which is the
--  equivalent of the C's (GtkArg*) array, i.e an array of unions.  This
--  package provides functions to extract the values from this type.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Signal handling</group>

with Cairo.Region;
with Glib.Values;
with Glib.Object;
with Glib.Types;
with Gdk.Drag_Contexts;
with Gdk.Event;
with Gdk.RGBA;
with Gdk.Rectangle;
with Gdk.Types;
with Gtk.Dialog;
with Gtk.Enums;
with Gtk.GEntry;
with Gtk.Notebook;
with Gtk.Print_Operation;
with Gtk.Status_Bar;
with Gtk.Text_Iter;
with Gtk.Text_View;
with Gtk.Tree_Model;
with Gtk.Widget;

package Gtk.Arguments is

   --  <doc_ignore>Do not create automatic documentation for this package

   subtype Gtk_Args is Glib.Values.GValues;
   --  This type represents a table of arguments. Each argument of the
   --  table can be of any type. You can access them through one of the
   --  To_* functions found below. The index of the first element is
   --  always 1.

   function Make_Args
     (Nb : Guint; Args : Glib.Values.C_GValues) return Gtk_Args
     renames Glib.Values.Make_Values;
   --  Build a Gtk_Args structure from the given C array. Nb should be the
   --  number of elements in the Args array.

   ---------------------------------------------------
   -- Conversion functions, interfacing to Gtk_Args --
   ---------------------------------------------------

   function To_Gint    (Args : Gtk_Args; Num : Positive) return Gint;
   function To_Guint   (Args : Gtk_Args; Num : Positive) return Guint;
   function To_Boolean (Args : Gtk_Args; Num : Positive) return Boolean;
   function To_Event   (Args : Gtk_Args; Num : Positive)
     return Gdk.Event.Gdk_Event;
   function To_String  (Args : Gtk_Args; Num : Positive) return UTF8_String;
   function To_Notebook_Page
     (Args : Gtk_Args; Num : Positive) return Gtk_Notebook_Page;
   function To_Address (Args : Gtk_Args; Num : Positive) return System.Address;
   function To_C_Proxy (Args : Gtk_Args; Num : Positive) return Glib.C_Proxy;
   function To_Object
     (Args : Gtk_Args; Num : Positive) return Glib.Object.GObject;
   --  This function can return null, if the C object was not created.
   function To_Requisition (Args : Gtk_Args; Num : Positive)
      return Gtk.Widget.Gtk_Requisition_Access;
   function To_Allocation
     (Args : Gtk_Args; Num : Positive) return Gtk.Widget.Gtk_Allocation_Access;

   function Unchecked_To_Boolean
     (Args : Glib.Values.C_GValues; Num : Guint) return Boolean;
   function Unchecked_To_Address
     (Args : Glib.Values.C_GValues; Num : Guint) return System.Address;
   function Unchecked_To_Object
     (Args : Glib.Values.C_GValues; Num : Guint) return Glib.Object.GObject;
   function Unchecked_To_Interface
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Glib.Types.GType_Interface;
   function Unchecked_To_Gint
     (Args : Glib.Values.C_GValues; Num : Guint) return Gint;
   function Unchecked_To_Gint_Access
     (Args : Glib.Values.C_GValues; Num : Guint) return access Gint;
   function Unchecked_To_Gdouble
     (Args : Glib.Values.C_GValues; Num : Guint) return Gdouble;
   function Unchecked_To_Gdouble_Access
     (Args : Glib.Values.C_GValues; Num : Guint) return access Gdouble;
   function Unchecked_To_Guint
     (Args : Glib.Values.C_GValues; Num : Guint) return Guint;
   function Unchecked_To_Context_Id
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Gtk.Status_Bar.Context_Id;
   function Unchecked_To_UTF8_String
     (Args : Glib.Values.C_GValues; Num : Guint) return UTF8_String;
   function Unchecked_To_Gdk_RGBA
     (Args : Glib.Values.C_GValues; Num : Guint) return Gdk.RGBA.Gdk_RGBA;
   function Unchecked_To_Gdk_Key_Type
     (Args : Glib.Values.C_GValues; Num : Guint) return Gdk.Types.Gdk_Key_Type;
   function Unchecked_To_Gdk_Event_Sequence
     (Args : Glib.Values.C_GValues; Num : Guint)
     return Gdk.Event.Gdk_Event_Sequence;
   function Unchecked_To_Gdk_Event_Button
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Button);
   function Unchecked_To_Gdk_Event_Owner_Change
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Owner_Change);
   function Unchecked_To_Gdk_Event_Scroll
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Scroll);
   function Unchecked_To_Gdk_Event
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event);
   function Unchecked_To_Gdk_Event_Any
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Any);
   function Unchecked_To_Gdk_Event_Configure
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Configure);
   function Unchecked_To_Gdk_Event_Crossing
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Crossing);
   function Unchecked_To_Gdk_Event_Expose
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Expose);
   function Unchecked_To_Gdk_Event_Focus
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Focus);
   function Unchecked_To_Gdk_Event_Grab_Broken
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Grab_Broken);
   function Unchecked_To_Gdk_Event_Key
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Key);
   function Unchecked_To_Gdk_Event_Motion
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Motion);
   function Unchecked_To_Gdk_Event_Property
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Property);
   function Unchecked_To_Gdk_Event_Proximity
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Proximity);
   function Unchecked_To_Gdk_Event_Selection
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Selection);
   function Unchecked_To_Gdk_Event_Visibility
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Visibility);
   function Unchecked_To_Gdk_Event_Window_State
     is new Glib.Values.Unsafe_Proxy_Nth (Gdk.Event.Gdk_Event_Window_State);
   function Unchecked_To_Gdk_Drag_Action
     is new Glib.Values.Unsafe_Enum_Nth (Gdk.Drag_Contexts.Gdk_Drag_Action);
   function Unchecked_To_Gdk_Drag_Cancel_Reason
     is new Glib.Values.Unsafe_Enum_Nth
              (Gdk.Drag_Contexts.Gdk_Drag_Cancel_Reason);
   function Unchecked_To_Gtk_Movement_Step
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Movement_Step);
   function Unchecked_To_Gtk_Drag_Result
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Drag_Result);
   function Unchecked_To_Gtk_State_Flags
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_State_Flags);
   function Unchecked_To_Gtk_State_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_State_Type);
   function Unchecked_To_Gtk_Scroll_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Scroll_Type);
   function Unchecked_To_Gtk_Text_Direction
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Text_Direction);
   function Unchecked_To_Gtk_Pan_Direction
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Pan_Direction);
   function Unchecked_To_Gtk_Position_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Position_Type);
   function Unchecked_To_Gtk_Event_Sequence_State
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Event_Sequence_State);
   function Unchecked_To_Gtk_Direction_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Direction_Type);
   function Unchecked_To_Gtk_Widget_Help_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Widget.Gtk_Widget_Help_Type);
   function Unchecked_To_Gtk_Delete_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Delete_Type);
   function Unchecked_To_Gtk_Menu_Direction_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Menu_Direction_Type);
   function Unchecked_To_Gtk_Orientation
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Orientation);
   function Unchecked_To_Gtk_Notebook_Tab
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Notebook.Gtk_Notebook_Tab);
   function Unchecked_To_Gtk_Toolbar_Style
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Toolbar_Style);
   function Unchecked_To_Gtk_Scroll_Step
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Enums.Gtk_Scroll_Step);
   function Unchecked_To_Gtk_Response_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gtk.Dialog.Gtk_Response_Type);
   function Unchecked_To_Gtk_Print_Operation_Result
     is new Glib.Values.Unsafe_Enum_Nth
        (Gtk.Print_Operation.Gtk_Print_Operation_Result);
   function Unchecked_To_Gdk_Modifier_Type
     is new Glib.Values.Unsafe_Enum_Nth (Gdk.Types.Gdk_Modifier_Type);
   function Unchecked_To_Gtk_Entry_Icon_Position
     (Args : Glib.Values.C_GValues; Num : Guint)
     return Gtk.GEntry.Gtk_Entry_Icon_Position;
   function Unchecked_To_Gtk_Text_Iter
     is new Glib.Values.Unsafe_Proxy_Nth (Gtk.Text_Iter.Gtk_Text_Iter);
   function Unchecked_To_Gtk_Text_Extend_Selection
     is new Glib.Values.Unsafe_Enum_Nth
              (Gtk.Text_View.Gtk_Text_Extend_Selection);
   function Unchecked_To_Gtk_Tree_Iter
     is new Glib.Values.Unsafe_Proxy_Nth (Gtk.Tree_Model.Gtk_Tree_Iter);
   function Unchecked_To_Gtk_Tree_Model
     is new Glib.Values.Unsafe_Proxy_Nth (Gtk.Tree_Model.Gtk_Tree_Model);
   function Unchecked_To_Param_Spec
     is new Glib.Values.Unsafe_Proxy_Nth (Glib.Param_Spec);
   function Unchecked_To_Cairo_Context
     (Args : Glib.Values.C_GValues; Num : Guint)
     return Cairo.Cairo_Context;
   function Unchecked_To_Cairo_Rectangle_Int_Access
     (Args : Glib.Values.C_GValues; Num : Guint)
     return access Cairo.Region.Cairo_Rectangle_Int;
   function Unchecked_To_Cairo_Rectangle_Int
     (Args : Glib.Values.C_GValues; Num : Guint)
     return Cairo.Region.Cairo_Rectangle_Int;
   function Unchecked_To_Gdk_Rectangle
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Gdk.Rectangle.Gdk_Rectangle
      renames Unchecked_To_Cairo_Rectangle_Int;
   function Unchecked_To_Gtk_Allocation
     (Args : Glib.Values.C_GValues; Num : Guint)
     return Gtk.Widget.Gtk_Allocation
     renames Unchecked_To_Cairo_Rectangle_Int;
   function Unchecked_To_Gdk_Rectangle_Access
     (Args : Glib.Values.C_GValues; Num : Guint)
     return access Gdk.Rectangle.Gdk_Rectangle
     renames Unchecked_To_Cairo_Rectangle_Int_Access;
   pragma Inline (Unchecked_To_Object);
   pragma Inline (Unchecked_To_Boolean);
   pragma Inline (Unchecked_To_Gint);
   pragma Inline (Unchecked_To_Guint);
   pragma Inline (Unchecked_To_UTF8_String);
   --  Return the num-value (starting at 0) in Args.
   --  These functions are unsafe and do not check that Args contains at least
   --  Num values.
   --  The names are directly the ones needed by gtk+ as described in its GIR
   --  files, and as generated automatically by GtkAda.

private
   pragma Inline (To_Gint);
   pragma Inline (To_Guint);
   pragma Inline (To_Boolean);
   pragma Inline (To_Object);
   pragma Inline (To_Event);
   pragma Inline (To_String);

   --  </doc_ignore>
end Gtk.Arguments;
