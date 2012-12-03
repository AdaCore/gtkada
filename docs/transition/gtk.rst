***
Gtk
***

.. highlight:: ada

Gtk.Action
==========

``Block_Activate_From,`` ``Unblock_Activate_From,`` ``Connect_Proxy,`` ``Disconnect_Proxy:``
these obsolete subprograms have been removed without a replacement.

``Get_Action`` has been removed without a replacement.

``Convert`` has been removed, use ``Glib.Object.Get_User_Data`` instead.

Gtk.Aspect_Frame
================

``Direct`` accessors ``Get_Xalign,`` ``Get_Yalign`` and ``Get_Ratio`` have been removed:
use the corresponding properties instead.

Gtk.Assistant
=============

The values in ``Gtk_Assistant_Page_Type`` were renamed for consistency,
removing their ``Gtk_`` prefix.

The package ``Generic_Assistant_Functions`` has been renamed to
``Set_Forward_Page_Func_User_Data.``

Gtk.Builder
===========

``Add_From_File`` now returns a ``Guint`` and the error as a parameter.

``Get_Widget`` has been removed (use ``Get_Object`` instead, and cast to the appropriate
type)

Gtk.Button_Box
==============

``Set_Child_Size`` was removed. Equivalent behavior can only be done by
changing the theme properties child-min-width and child-min-height.

Gtk.Cell_Layout
===============

``Get_Cell_Renderers`` has been renamed to ``Get_Cells.``

Gtk.Cell_Renderer
=================

The ``Render`` subprogram is now called with a ``Cairo_Context`` rather than a
``Gdk_Window.``

Gtk.Cell_View
=============

``Get_Cell_Renderers`` is obsolete, use the ``Gtk.Cell_Layout`` interface and
``Gtk.Cell_Layout.Get_Cells.``

Gtk.Clist
=========

This widget has been removed: use a ``Gtk.Tree_View`` instead.

Gtk.Container
=============

Procedure ``Propagate_Expose`` has been removed and will be replaced with
``Propagate_Draw.``

``Class_Find_Child_Property,`` ``Class_list_Child_Properties`` and
``Class_Install_Child_Property`` are no longer bound.

``Children`` was removed (use ``Get_Children`` instead).

Gtk.Color_Button
================

The function ``Get_Color`` returning ``Gdk.Color.Gdk_Color`` is now a procedure
with an out parameter.

Gtk.Color_Selection
===================

``Get_Color`` and ``Set_Color`` have been removed: use ``Get_Current_Color`` and
``Set_Current_Color`` instead.

Gtk.Color_Selection_Dialog
==========================

Subprogram ``Get_Colorsel`` has been renamed ``Get_Color_Selection,`` to match
the ``Gtk+`` naming.

``Get_OK_Button,`` ``Get_Cancel_Button,`` ``Get_Help_Button`` have been removed.
Instead, use::

   Gtk_Button (Glib.Properties.Get_Property (Dialog, Ok_Button_Property)),
   Gtk_Button (Glib.Properties.Get_Property (Dialog, Cancel_Button_Property)),
   Gtk_Button (Glib.Properties.Get_Property (Dialog, Help_Button_Property))

Gtk.Combo
=========

This widget has been removed: use a ``Gtk.Combo_Box`` instead.

Gtk.Combo_Box
=============

The "text only" variant has been moved to the new package ``Gtk.Combo_Box_Text.``

Gtk.Combo_Box_Entry
===================

This widget has been removed: use a ``Gtk.Combo_Box`` instead.

Gtk.Clipboard
=============

The base type is now a ``GObject_Record`` instead of an opaque type: use the
``GObject`` facilities for lifecycle management.

There are now separate "``User_Data"`` generic version for callback-based methods.

Gtk.Ctree
=========

This widget has been removed: use a ``Gtk.Tree_View`` instead.

Gtk.Curve
=========

This widget has been removed, with no direct replacement.  Use drawing
functionality from ``Cairo`` instead.

Gtk.Dialog
==========

Subprogram ``Get_Vbox`` was replaced with ``Get_Content_Area.``

Subprogram ``Set_Has_Separator`` has been removed: use the corresponding flag
in the call to ``Gtk_New/Initialize`` instead.

Gtk.Dnd
=======

``Source_Set_Icon`` has been removed: use ``Source_Set_Icon_Pixbuf`` instead.
``Set_Icon_Pixmap`` has been removed: use ``Set_Icon_Pixbuf`` instead.

Obsolete ``Set_Default_Icon`` working on ``Gdk.Pixmap`` has been removed without a replacement.

Gtk.Editable
============

The type representing a ``Gtk_Editable_Record`` has been changed from a
``Widget`` (which is a ``GObject)`` to an interface (a ``System.Address).``
Therefore the ``Gtk_Editable_Record`` type has been eliminated.  User code
referencing only the ``Gtk_Editable`` type should function unchanged.

Code using the tag as a test before converting a widget to a ``Gtk.Editable``
can now work using the ``Implements_Editable`` package.

For instance, if ``Widget`` is a ``GObject_Record,`` the following code::

      if Widget.all in Gtk_Editable_Record'Class then
         Cut_Clipboard (Gtk_Editable (Widget));

becomes::

      if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
         Cut_Clipboard`` (+Widget);

where the function "+" is defined by instantiating ``Implements_Editable``::

   package Implements_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, GObject_Record, GObject);
   function "+"
     (Widget : access GObject_Record'Class)
      return Gtk.Editable.Gtk_Editable
      renames Implements_Editable.To_Interface;

The ``Select_Region`` subprogram parameter name ``The_End`` has been normalized
to ``End_Pos``.

Gtk.Entry_Completion
====================

The "match-selected" and "cursor-on-match" signals were erroneously
given the internal filter model instead of the users model. This oversight
has been fixed in GTK+ 3; if you have handlers for these signals, they
will likely need slight adjustments. 

Gtk.Enums
=========

The following types were removed::

  ``GtkAnchorType``
  ``GtkCurveType``
  ``GtkMetricType``
  ``GtkGridLines``
  ``GtkUpdateType``
  ``GtkVisibility``
  ``GtkSideType``
  ``GtkMatchType``
  ``GtkPreviewType``
  ``GtkSubmenuDirection``
  ``GtkSubmenuPlacement``
  ``GtkTreeViewMode``

``Gtk_Icon_Size`` is no longer an enumeration type, but an integer, so that
new sizes can be defined through ``Gtk.Icon_Factory.Icon_Size_Register``.

Gtk.File_Chooser_Button
=======================

Subprograms ``Gtk_New_With_Backend`` and ``Initialize_With_Backend`` have been
removed: use ``Gtk_New`` and ``Initialize`` instead.

Gtk.File_Chooser_Dialog
=======================

Subprograms ``Gtk_New_With_Backend`` and ``Initialize_With_Backend`` have been
removed: use ``Gtk_New`` and ``Initialize`` instead.

Gtk.File_Chooser_Widget
=======================

Subprograms ``Gtk_New_With_Backend`` and ``Initialize_With_Backend`` have been
removed: use ``Gtk_New`` and ``Initialize`` instead.

Gtk.File_Selection
==================

This package has been replaced by ``Gtk.File_Chooser.``
You may also use ``Gtkada.File_Selection`` for a simple interface to the
``Gtk.File_Chooser.``

Gtk.Fixed
=========

Subprograms ``Set_Has_Windows`` and ``Get_Has_Windows`` are now in ``Gtk.Widget.``

Gtk.Gamma_Curve
===============

This widget has been removed without any replacement.

Gtk.GC
======

This package has been removed: ``Cairo`` packages should be used for drawing.

Gtk.GEntry
==========

The names for ``Gtk_Entry_Record`` parameters have been normalized across
the board to "``The_Entry".``

``Append_Text`` has been removed: use ``Set_Text`` and ``Get_Text`` instead.

Gtk.GRange
==========

``Set_Update_Policy`` has been removed, with no replacement. If you require
delayed updates, you will need to code it yourself.

Gtk.HRuler
==========

This widget has been removed without any replacement.

Gtk.Icon_Factory
================

``Gtk_Icon_Set`` and ``Gtk_Icon_Source`` have been moved to their own packages.
``Functions`` ``Gtk_New`` are now procedures.

Gtk.Image
=========

The subprograms working with ``Gdk_Pixmap`` have been removed, use the
variants working on ``Gdk_Pixbuf`` instead.

Gtk.Image_Menu_Item
===================

All controlling parameters were renamed to ``Self``. There was no consistency
before.

``Gtk_New_From_Stock`` now requires an ``Accel_Group`` parameter, which can be set to
null.

Gtk.Input_Dialog
================

This package is no longer part of gtk+, so this binding has been removed
without replacement.

Gtk.Item
========

This obsolete package has been removed with no replacement.

Gtk.Item_Factory
================

This obsolete package has been removed in favor of ``Gtk.UI_Manager.``

Gtk.Layout
==========

``Get_Width`` and ``Get_Height`` have been removed, use ``Get_Size`` instead.

Gtk.Link_Button
===============

All widget parameter names have been normalized to "``Self".``

The ``Set_Uri_Hook`` function has been eliminated, and along with it the
``Uri_Func`` type and the ``Generic_Uri_Hook`` package.  ``Register`` a callback
for the button's "clicked" signal instead.

Gtk.List_Item
=============

This widget has been removed: use a ``Gtk.Tree_View`` instead.

Gtk.Main
========

``Do_Event`` was renamed ``Main_Do_Event.``

``Grab_Add`` and ``Grab_Removed`` are available in ``Gtk.Widget`` (as was already
 the case with gtk2).

The ``Quit`` package has been removed without replacement.

The ``Idle`` and ``Timeout`` handling been removed: use equivalent functions in
package ``Glib.Main`` instead.

Gtk.Menu
========

``User_Menu_Popup`` has been replaced by ``Popup_User_Data.``

The version of ``Popup`` was took an access to ``C_Gtk_Menu_Positon_Func`` has
been removed. If you need to pass ``User_Data`` to the callback, you need to
instantiate the package ``Popup_User_Data.`` Note that in this package the
position of the ``Data`` parameter has changed.

Gtk.Menu_Item
=============

For subprogram ``Set_Right_Justified,`` the parameter "``Justify"`` has been
renamed to "``Right_Justified".``

The obsolete procedures ``Remove_Submenu,`` ``Set_Right_Justify,`` and
``Right_Justify`` have been removed.  Instead, use ``Set_Submenu``,
``Set_Right_Justified,`` or ``Set_Right_Justified`` with ``Justify=True,``
respectively.

Calling ``Gtk_New`` with one ``Menu_Item`` argument has the same effect now
as before.  However, from this version on, if a ``Label`` argument exists
(even if set to ""), a ``Gtk_Label`` child will be created with the given
value.

Gtk.Menu_Tool_Button
====================

``Set_Arrow_Tooltip`` has been removed, use ``Set_Arrow_Tooltip_Markup`` or
``Set_Arrow_Tooltip_Text`` instead.

Gtk.Notebook
============

``Get_Children`` has been removed: call ``Gtk.Container.Get_Children`` instead.

``Set_Tab_Label_Packing`` has been removed (this is left under control of the
theme).

``Set_Page`` has been removed, use ``Set_Current_Page`` instead.

``Insert_Page`` now returns the number of the page that has been inserted.

Gtk.List
========

This package has been removed: use a ``Gtk_Tree_View`` instead.

Gtk.Object
==========

``Gtk.Object`` has been removed in gtk+-3.

The following subprograms and declarations are now in ``Gtk.Widget``::

    ``Flags``
    ``Unset_Flags``

    ``Floating``
    ``In_Destruction_Is_Set``

    ``Signal_Destroy``

The subprogram ``Gtk.Object.Sink`` has been removed: use ``Glib.Object.Ref_Sink``
 instead.

Gtk.Old_Editable
================

This obsolescent API has been removed, use ``Gtk.Editable`` where relevant.

Gtk.Option_Menu
===============

``Gtk.Option_Menu`` has been removed.  Using ``Gtk.Combo_Box`` instead is
recommended.

Gtk.Pixmap
==========

This widget has been removed and is generally replaced with a ``Gtk.Image.``

Gtk.Preview
===========

This widget has been removed without replacement.

Gtk.Print_Operation
===================

``Get_Status`` was renames to ``Get_Status_String`` when it returns a string, to
match the gtk+ API.

Gtk.Progress
============

This widget has been removed without any replacement.

Gtk.Progress_Bar
================

This widget is now derived from ``Gtk.Widget`` directly, rather than from
``Gtk.Progress`` (which has been removed).

The enumeration type ``Gtk_Progress_Bar_Orientation`` has been removed,
and this widget now implements the ``Gtk_Orientable`` interface.  To fully
achieve the same functionality as the GtkAda 2.x ``Get_Orientation/``
``Set_Orientation`` subprograms, it is now necessary to call
``Get_Orientation/Set_Orientation`` along with ``Get_Inverted/Set_Inverted.``

Procedure ``Set_Pulse_Step's`` "``Step"`` parameter has been renamed to "``Formal."``

``Set_Ellipsize`` and ``Get_Ellipsize`` parameter names have been normalized
from "``Pbar"`` to "``Progress_Bar".``

If you intend to show text over the progress bar, you need to call
``Set_Text`` as before, but also call ``Set_Show_Text(True)``.

Gtk.Recent_Manager
==================

The type ``Gtk_Recent_Info`` is now bound in its own package.

Gtk.Ruler
=========

This widget has been removed without any replacement.

Gtk.Settings
============

``Properties`` are now named with the suffix "_Property". For instance,
``Gtk_Theme_Name`` is now ``Gtk_Theme_Name_Property.``

Gtk.Scale_Button
================

This package now conforms to the API conventions practiced throughout
the rest of the toolkit.  ``Gtk_New`` is implemented as a procedure rather
than as a function, and the use of ``GNAT.Strings.String_List`` replaces
``Gtkada.Types.Chars_Ptr_Array`` throughout.

Gtk.Selection
=============

This package has been renamed ``Gtk.Selection_Data,`` for homogeneity with
the naming conventions.

``Gtk.Selection.Selection_Data`` is now called 
``Gtk.Selection_Data.Gtk_Selection_Data.``

Handling of ``Target_Lists`` has been moved to the new package ``Gtk.Target_List,``
along with ``Target_Entry_Array.``

The type ``Gtk_Target_Entry`` has been moved to the new package ``Gtk.Target_Entry.``

The way of obtaining the selection data from callbacks using the ``Args/GValues``
approach has changed, from::

      Data  : constant Gtk.Selection.Selection_Data :=
        Gtk.Selection.Selection_Data (Get_Proxy (Nth (Args, 2)));

to::

      Data  : constant Gtk.Selection_Data.Gtk_Selection_Data :=
        From_Object (Get_Address (Nth (Args, 2)));

The type ``Target_Flags`` has been moved to ``Gtk.Enums.Gtk_Target_Flags.``

The flag corresponding to ``Target_No_Constraint`` has been removed: use the
value 0 instead.

Gtk.Scrolled_Window
===================

``Set_Policy's`` parameters were renamed to ``Hscrollbar_Policy`` and
``Vscrollbar_Policy`` instead of ``H_Scrollbar_Policy`` and ``V_Scrollbar_Policy.``

Gtk.Socket / Gtk.Plug
=====================

The binding for these two packages was removed. They are not portable
across platforms, and require access to the low-level X11 window ID,
for which we do not provide a binding.

Gtk.Status_Icon
===============

``Status_Icon`` widget parameter names have been normalized to "``Status_Icon".``

``Get_Blinking`` and ``Set_Blinking`` have been removed, it is no longer possible to
make the status icon blink.

Gtk.Style
=========

All functions based on ``Gdk.GC`` or ``Gdk.Pixmap`` have been removed.
This package is deprecated (but not removed yet) in gtk3
Use functions in ``Gtk.Style_Context`` instead.

A number of drawing functions have been removed: use the ``Paint_*`` functions
instead.

``Replace`` a call to ``Get_Font`` with::

    with Gtk.Style_Context;  use Gtk.Style_Context;
    Get_Style_Context (Widget).Get_Font (Gtk_State_Flags_Normal);

Gtk.Text
========

This obsolescent API has been removed: use a ``Gtk.Text_View/Gtk.Text_Buffer``
instead.

Gtk.Text_Attributes
===================

``Set_Fg_Stipple,`` ``Get_Fg_Stipple,`` ``Set_Bg_Stipple,`` ``Get_Bg_Stipple`` have been
removed without a replacement.

Gtk.Text_View
=============

The functions ``Get/Set_Disable_Scroll_On_Focus`` have no effect in recent
versions of gtk+ and have been removed.

Gtk.Tree_Dnd
============

This package was removed, and its contents split into ``Gtk.Tree_Drag_Source``
and ``Gtk.Tree_Drag_Source.``

The ```Drag_Dest_``` and ```Drag_Source_``` prefixes were removed from the subprogram,
so for instance ``Drag_Dest_Drag_Data_Received`` has become ``Drag_Data_Received.``

Gtk.Tree_Model
==============

A ``Gtk_Tree_Model`` is now an interface (implemented by ``Gtk_List_Store``
and ``Gtk_Tree_Store),`` no longer a tagged type. It means that in callbacks
that receive a ``Gtk_Tree_Model`` parameter, you can no longer cast this
parameter to a ``Gtk_Tree_Store`` for instance. ``Instead,`` you need to do
the following::

       --  Model is the parameter, of type Gtk_Tree_Model
       Tree : constant Gtk_Tree_Store := Gtk_Tree_Store (-Model);

``Gtk_New,`` for a ``Gtk_Tree_Path,`` are now procedures instead of functions,
to follow the usual GtkAda convention.

``Gtk_Tree_Row_Reference`` has been moved to its own package
``Gtk.Tree_Row_Reference.``

``Gtk_New`` and ``Gtk_New_First`` (for a tree path) now take a "out" parameter,
for consistency with the rest of the API.

Gtk.Tree_View_Column
====================

``Get_Cell_Renderers`` is obsolete, use the ``Gtk.Cell_Layout`` interface and
``Gtk.Cell_Layout.Get_Cells.``

Gtk.Tips_Query
==============

This obsolete package has been removed.

Gtk.Tool_Item
=============

``Set_Tooltip`` has been removed: use ``Set_Tooltip_Text`` and ``Set_Tooltip_Markup``
instead.

Gtk.Toolbar
===========

All ``Gtk_Toolbar`` widget parameter names have been normalized to "``Toolbar".``

``Subprograms`` ``Append_*,`` ``Prepend_*`` and ``Insert_*`` have been removed: use ``Insert``
instead.

``Subprograms`` ``Get_Tooltips/Set_Tooltips`` have been removed.  Use the
``Gtk_Enable_Tooltips`` property instead.

Gtk.Tooltips
============

The package ``Gtk.Tooltips`` has been removed, in favor of ``Gtk.Tooltip.``

For creating simple tooltips on all GtkAda widgets, the easiest is to use
``Gtk.Widget.Set_Tooltip_Text`` or ``Gtk.Set_Tooltip_Markup.`` See the example
in testgtk/create_tooltip.adb.

Gtk.Tree_View
=============

``Procedure`` ``Create_Row_Drag_Icon`` now returns a ``Cairo_Surface.``

``Get_Hadjustment,`` ``Set_Hadjustment,`` ``Get_Vadjustment,`` ``Set_Vadjustment`` have been
removed: use the equivalent properties.

``Widget_To_Tree_Coords`` and ``Tree_To_Widget_Coords`` have been removed: use
``Convert_Widget_To_Tree_Coords`` and ``Convert_Tree_To_Widget_Coords.``

Gtk.VRuler
==========

This widget has been removed without any replacement.

Gtk.Widget
==========

The old ``Draw`` function no longer exists, and should be replaced with calls
to ``Queue_Draw_Area.`` ``However,`` a new ``Draw`` function was added with a different
profile and different semantic.

Function```Get_Snapshot`` has been removed. ``Draw`` should be used instead.

``Hide_All`` has been removed: use ``Hide`` instead.

``Set_Extension_Events`` and ``End_Extension_Events`` are no longer needed and have
been removed.

``Set_Colormap`` and ``Get_Colormap`` are no longer needed and have been removed.

``Set_Scroll_Adjustments`` has been removed without a replacement.

``Shape_Combine_Mask,`` ``Input_Shape_Combine_Mask`` and ``Reset_Shapes`` have been removed
without replacements.

``Set_Uposition`` has been removed: use the properties of the containing widget
to fix the position of contained widgets. The functions in ``Gtk.Window,`` for
instance ``Gtk.Window.Move,`` should be used for top-level widgets.

``Set_USize`` has been removed: use ``Set_Size_Request`` instead.

``Size_Request`` is now obsolescent. The recommend replacement is to use
``Get_Preferred_Width`` and ``Get_Preferred_Height.``

``Set_Default_Colormap,`` ``Get_Default_Colormap,`` ``Push_Colormap`` and ``Pop_Colormap`` were
removed. They are no longer needed, since all drawing is done through ``Cairo``
which doesn't use a colormap but directly the red/green/blue components.

``Queue_Clear`` and ``Queue_Clear_Area`` have been removed, call ``Queue_Draw`` and
``Queue_Draw_Area`` instead.

The signal "expose_event" no longer exists. It has been replaced with the
"draw" signal which provides a preconfigured ``Cairo_Context`` suitable for
the drawing (including the clip area that is used to speed up the rendering).

``Activate`` is now a function.

``Child_Focus:`` removed default value for ``Direction`` parameter
(was ``Dir_Tab_Forward)``

``Get_Allocation_Height`` and ``Get_Allocation_Width`` are now named
``Get_Allocated_Height`` and ``Get_Allocated_Width.``

``Get_Allocation_X`` and ``Get_Allocation_Y`` were removed, and can be accessed
through ``Get_Allocation.X`` and ``Get_Allocation.Y`` instead.

A lot of flags (``Can_Focus,`` ``Can_Default,...)`` now have explicit setters and
getters. This removed a number of subprograms, like::

   ``Double_Buffered_Is_Set`` (see ``Get_Double_Buffered)``
   ``Can_Focus_Is_Set`` (see ``Get_Can_Focus)``
   ``Mapped_Is_Set`` (see ``Get_Mapped)``
   ``Realized_Is_Set`` (see ``Get_Realized)``
   ``Has_Default_Is_Set`` (see ``Has_Default)``
   ``Has_Focus_Is_Set`` (see ``Has_Focus)``
   ``Has_Grab_Is_Set`` (see ``Has_Grab)``
   ``Rc_Style_Is_Set`` (see ``Has_Rc_Style)``
   ``In_Destruction_Is_Set`` (see ``In_Destruction)``
   ``Drawable_Is_Set`` (see ``Is_Drawable)``
   ``No_Window_Is_Set`` (see ``Has_Window)``

``Size_Allocate`` now takes an "in out" parameter for the allocation

``Set_Flags`` was renamed ``Set_State_Flags``
``Unset_Flags`` was renamed ``Unset_State_Flags``
``Flags`` and ``Flag_Is_Set`` must be replaced with a call to ``Get_State_Flags``

``Get_Child_Requisition`` is now a procedure with an in out parameter. It is
obsolescent.

``Default_Motion_Notify_Event`` was removed.

``Has_Default_Motion_Notify_Handler`` was removed.

``Get_Default_Visual`` was removed.

``Restore_Default_Style`` was removed (use ``Set_Style`` with a null parameter
instead).

``Class_Find_Style_Property,`` ``Class_List_Style_Properties`` and
``Class_Install_Style_Property`` were removed. ``They`` are mostly of interest
when writting theme engines.

``Class_Path`` and ``Path`` were replaced with ``Get_Path.``

``Allow_Shrink_Property`` and ``Allow_Grow_Property`` have been removed: use
 ``Get_Hexpand`` and ``Get_Vexpand`` instead.

``Render_Icon`` has been replaced by ``Render_Icon_Pixbuf.``

Gtk.Window
==========

``Set_Has_Frame,`` ``Get_Has_Frame,`` ``Set_Frame_Dimensions,`` ``Get_Frame_Dimensions:``
these special-purpose subprograms have been removed without replacement.

``Get_Gravity,`` ``Set_Gravity:`` these have been removed, use the property
``Gravity_Property`` instead.

``Resize`` no longer accepts parameters set to -1 to indicate the preferred
size of the window. This was a GtkAda extension, which can be achieved
using ``Get_Preferred_Size`` and passing the result to ``Size.``

``Group_Add_Window`` was renamed to ``Add_Window.``
``Group_Remove_Window`` was renamed to ``Remove_Window.``
``Group_List_Windows`` was renames to ``List_Windows.``

``Initialize`` now has the same default value for its ``The_Type`` parameter
as ``Gtk_New.``

