***
Gdk
***

Gdk.Bitmap
==========

This package has been removed: ``Cairo`` packages should be used for drawing, and
``Gdk.Pixbuf`` for representing image data in memory.

Gdk.Color
=========

``Alloc`` no longer exists, and is not necessary since all drawing is now done
internally using ``Cairo`` which directly manipulates red/green/blue.

Gdk.Cursor
==========

The ``Gdk_New`` function working on ``Gdk_Pixmap`` has been removed. Use
``Gdk.Pixbuf.Gdk_New_From_Pixbuf`` to create a cursor from a pixbuf.

The ``Gdk_New`` function working on a ``String`` has also been removed.

A ``Gdk_Cursor`` is now derived from a ``Glib.Object.`` This has little
impact on programs, except that ``Null_Cursor`` can be replaced simply
by "null".

``Destroy`` was removed, and should be replaced with ``Unref.``

Gdk.Dnd
=======

The functions for handling ``Drag_Contexts`` have been moved to new package
``Gdk.Drag_Contexts.``

The ``Gdk_Drag_Context`` itself now inherits from ``GObject,`` which means that it no
longer requires its own ``Ref/Unref`` functions.

``Drag_Find_Window`` has been removed, use ``Drag_Find_Window_For_Screen`` instead.

``Drag_Get_Protocol`` has been replaced with ``Drag_Context_Get_Protocol.``

Gdk.Drawable
============

All ``Draw_*`` subprograms have been removed: use ``Cairo`` for low-level drawing.

The type ``Gdk_Drawable`` no longer exists.

Gdk.Event
=========

A lot of the getters (and all of the setters) were removed. Instead, the
``Gdk_Event`` type fields can now be edited directly. This is slightly more
efficient, and more importantly better documents which fields are valid for
which event types.

The APIs to ``Get_Message_Type,`` ``Set_Message_Type,`` ``Get_Data,`` and ``Set_Data`` have
been removed without replacement.

``Get_Graphics_Expose`` and ``Send_Client_Message`` have been removed with no
replacement.

``Deep_Copy`` was removed. It is now possible to simply use ":=" on the record
type itself.

``Get`` and ``Peek`` are now functions instead of procedures with a single out
parameter.

``Is_Created`` has been removed (you can compare with null)
``Send_Client_Message_To_All`` has been removed (deprecated in gtk+)

``Allocate`` has been removed. ``Instead,`` users should directly use
``Gdk.Event.Gdk_New`` and set the appropriate fields.

``Get_X`` and ``Get_Y`` were replaced by ``Get_Coords.``
``Get_X_Root`` and ``Get_Y_Root`` were replaced by ``Get_Root_Coords``

``Get_Button,`` ``Get_State,`` ``Get_Key_Val`` and ``Get_Keycode`` were kept (so you do not
have to directly access the field of ``Gdk_Event).`` ``However,`` they no longer raise
an exception if you pass them an invalid event type, but return an out-of-range
value.

Gdk.Font
========

This package has been removed: use ``Pango.Font`` for fonts manipulation,
``Cairo.Font_Face`` and ``Cairo.Font_Options`` for text rendering.

Gdk.GC
======

This package has been removed: ``Cairo`` packages should be used for drawing.

Gdk.Image
=========

This package has been removed: use a ``Gdk.Pixbuf`` instead.

Gdk.Main
========

``Set_Locale`` functions are no longer needed and have been removed.

Functions ``Set_Use_Xshm`` and ``Get_Use_Xshm`` have been removed.

Gdk.Pixbuf
==========

``Render_Threshold_Alpha,`` ``Render_To_Drawable,`` ``Render_To_Drawable_Alpha,``
``Render_Pixmap_And_Mask,`` ``Render_Pixmap_And_Mask_For_Colormap`` have been removed.

Use APIs provided by ``Gdk.Cairo`` to draw a pixbuf on a ``Gdk_Drawable.``

``Get_From_Drawable`` has been removed, use ``Get_From_Surface`` or ``Get_From_Window.``

Gdk.Pixmap
==========

This package has been removed: ``Cairo`` packages should be used for drawing, and
``Gdk.Pixbuf`` for representing image data in memory.

Gdk.Region
==========

This package has been removed and replaced with ``Cairo_Region.``

Gdk.RGB
=======

This package is deprecated in gtk3. Use ``Pixmaps/Cairo`` for drawing, and
use ``Gdk.Pixbuf`` for offscreen image manipulation and rendering to drawables.

Instead of ``Gdk.Rgb.Get_Cmap,`` use ``Gtk.Widget.Get_Default_Colormap.``

Gdk.Window
==========

A ``Gdk_Window`` now derives from ``GObject.`` This is mostly transparent for
applications, unless you are passing a ``Gdk_Window`` directly to C code,
in which case you must use ``Get_Object()`` on it.

``Copy_Area`` and ``Set_Back_Pixmap`` have been removed: use ``Gdk_Drawable`` and
``Gdk.Cairo`` functions instead.

``Clear_Area`` and ``Clear_Area_E`` were removed. Use ``Cairo`` for all drawings.

``Get_Desk_Relative_Origin:`` this function has been removed without a replacement.

``Get_Toplevels`` has been removed, use ``Gtk.Window.List_Toplevels`` instead.

``Set_Hints`` has been removed.  Depending on what you are trying to do, use
``Gtk.Window.Resize,`` ``Gtk.Window.Set_Size_Request,`` ``Gtk.Window.Move,``
``Gtk.Window.Parse_Geometry,`` and ``Gtk.Window.Set_Geometry_Hints.``

``Window_At_Pointer`` was renamed to ``At_Pointer.``

``Get_Origin`` is now a procedure, because the return value had no meaning anyway.

``Get_Geometry:`` no longer returns the color depth of the window, which is no
longer relevant to gtk+.

The first parameter of the various methods was renamed "``Self"`` instead of
"window" to avoid a number of cases where we would end up with duplicate
parameter names.

Gdk.Window_Attr
===============

Parameter "``Colormap"`` has been removed from procedure ``Gdk_New.`` This parameter
 is no longer needed.

``Set_Colormap`` and ``Get_Colormap`` should no longer be needed and have been removed
as well.

