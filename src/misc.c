/*
-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
*/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include <glib.h>
#include <glib-object.h>

#include <pango/pango.h>

#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <gtk/gtksignal.h>
#include <gtk/gtktextview.h>
#include <gtk/gtktypeutils.h>
#include <gtk/gtkwidget.h>

gint
convert_a (void* a)
{
   return GPOINTER_TO_INT (a);
}

void*
convert_i (gint s)
{
   return GINT_TO_POINTER (s);
}

guint
convert_ua (void* a)
{
   return GPOINTER_TO_UINT (a);
}

void*
convert_ui (guint s)
{
   return GUINT_TO_POINTER (s);
}

/********************************************************************
 *  Returns the major/minor/macro version number of Gtk+. This is
 *  needed as the windows version uses a different convention for the
 *  corresponding variables gtk_{major/minor/micro)_version than the
 *  unix version.
 ********************************************************************/

guint
ada_gtk_major_version () {
  return GTK_MAJOR_VERSION;
}

guint
ada_gtk_minor_version () {
  return GTK_MINOR_VERSION;
}

guint
ada_gtk_micro_version () {
  return GTK_MICRO_VERSION;
}

/********************************************************************
 **  Returns the real widget name (as opposed to gtk_widget_get_name,
 **  this one returns NULL instead of the class name if no name was
 **  set.
 ********************************************************************/

char*
ada_gtk_get_real_name (GtkWidget* w) {
  return w->name;
}

/********************************************************************
 **  This function parses the command line and returns
 **  true if macro_switch exists. It is also removed from
 **  the command line
 ********************************************************************/

int
ada_gtk_parse_cmd_line (int *gnat_argc, char **gnat_argv, char* macro_switch)
{
  int i;

  for (i=1; i<*gnat_argc; i++)
    {
      if (! strcmp (gnat_argv[i], macro_switch)) {
	while (i < * gnat_argc - 1) {
	  gnat_argv [i] = gnat_argv [i + 1];
	  i++;
	}
	gnat_argv [i] = NULL;
	(*gnat_argc)--;
	return 1;
      }
    }
  return 0;
}

/******************************************
 ** GSignal                              **
 ******************************************/

int c_signal_query_size () {
  return sizeof (GSignalQuery);
}

const gchar* ada_gsignal_query_signal_name (GSignalQuery* query) {
  return query->signal_name;
}

const GType* ada_gsignal_query_params (GSignalQuery* query, guint* n_params) {
  *n_params = query->n_params;
  return query->param_types;
}

guint ada_gsignal_query_id (GSignalQuery* query) {
  return query->signal_id;
}

GType ada_gsignal_query_return_type (GSignalQuery* query) {
  return query->return_type;
}

/*********************************************************************
 ** Creating new widgets
 ** For each new widget created by the user, we create a new
 ** class record, that has the following layout:
 **
 **  struct NewClassRecord {
 **     struct AncestorClass ancestor_class;   // the ancestor
 **     void (*handler1) (...);                // handler for first signal   
 **     void (*handler2) (...);                // handler for second signal
 **     ...
 **     void (*handlern) (...);                // handler for nth signal
 **     VirtualFunctions * virtual;            // pointer to virtual interface
 **                                            // functions
 **     GObjectGetPropertyFunc real_get_property;
 **                                            // pointer to the get_property
 **                                            // in user's code
 **     GObjectSetPropertyFunc real_set_property;
 **                                            // likewise for set_property
 **  };
 **
 ** struct VirtualFunctions {
 **     void (*virtual) (...);                 // first virtual function for
 **                                            // interfaces
 **     ...
 **     void (*virtual) (...);
 ** }

 *********************************************************************/

GType ada_type_from_class (gpointer klass) {
  return G_TYPE_FROM_CLASS (klass);
}

void* ada_get_nth_virtual_function (GObjectClass* class, gint num)
{
  GTypeQuery query;
  gpointer * virtual_functions;
  
  g_type_query (G_TYPE_FROM_CLASS (class), &query);
  virtual_functions = (gpointer*)((char*)(class)
				  + query.class_size
				  - sizeof (GObjectGetPropertyFunc)
				  - sizeof (GObjectSetPropertyFunc)
				  - sizeof (gpointer));
  return virtual_functions + num * sizeof (gpointer);
}

void*
ada_initialize_class_record
  (GObject*      object,
   gint          nsignals,
   char*         signals[],
   GType         parameters[],
   gint          max_parameters,
   GObjectClass* old_class_record,
   gchar*        type_name,
   gint          num_virtual_functions,
   gpointer*     virtual_functions)
{
  GObjectClass* klass;
  
  if (!old_class_record)
    {
      /* Note: The memory allocated in this function is never freed. No need
	 to worry, since this is only allocated once per user's widget type,
	 and might be used until the end of the application */

      /* Right now, object->klass points to the ancestor's class */
      GType ancestor = G_TYPE_FROM_CLASS (G_OBJECT_GET_CLASS (object));
      GTypeInfo * class_info = g_new (GTypeInfo, 1);
      GTypeQuery query;
      GType new_type;
      gpointer* virtual;
      int j;

      /* We need to know the ancestor's class/instance sizes */
      g_type_query (ancestor, &query);

      class_info->class_size = query.class_size
	+ nsignals * sizeof (void*)
	+ sizeof (GObjectGetPropertyFunc)
	+ sizeof (GObjectSetPropertyFunc)
	+ sizeof (void*); /* Last one if for virtual functions */
      class_info->base_init = NULL;
      class_info->base_finalize = NULL;
      class_info->class_init = NULL;
      class_info->class_finalize = NULL;
      class_info->class_data = NULL; /* Would be nice to use this for the set_property???*/
      class_info->instance_size = query.instance_size;  /* ??? should be parameter */
      class_info->n_preallocs = 0;
      class_info->instance_init = NULL;
      class_info->value_table = NULL;

      /* Need to create a new type, otherwise Gtk+ won't free objects of
         this type */
      new_type = g_type_register_static
	(ancestor, g_strdup (type_name), class_info, 0);
      klass = g_type_class_ref (new_type);

      g_assert (klass != NULL);

      /* Initialize signals */
      for (j = 0; j < nsignals; j++)
	{
	  int count = 0;
	  guint id;
	  GClosure *closure;

	  while (count < max_parameters
		 &&
		 (parameters [j * max_parameters + count] != G_TYPE_NONE))
	    {
	      count++;
	    }

  
	  closure = g_signal_type_cclosure_new
	    (new_type, query.class_size + j * sizeof (void*)); /* offset */
	  
	  id = g_signal_newv
	    (signals[j],                       /* signal_name */
	     new_type,                         /* itype */
	     G_SIGNAL_RUN_LAST,                /* signal_flags */
	     closure,                          /* class_closure */
	     NULL,                             /* accumulator */
	     NULL,                             /* accu_data */
	     g_cclosure_marshal_VOID__VOID,    /* c_marshaller, unused at the
	       Ada level ??? This probably makes the widget unusable from C */
	     G_TYPE_NONE,                      /* return_type */
	     count,                            /* n_params */
	     parameters + j * max_parameters); /* param_types */
	}

      /* Initialize the function pointers for the new signals to NULL */
      memset ((char*)(klass) + query.class_size, 0,
	      nsignals * sizeof (void*)
	      + sizeof (GObjectGetPropertyFunc)
	      + sizeof (GObjectSetPropertyFunc)
	      + sizeof (void*));

      virtual = (gpointer*) malloc (num_virtual_functions * sizeof (gpointer));
      *((gpointer**)((char*)klass + query.class_size + nsignals * sizeof (void*)))
	= virtual;

      for (j = 0; j < num_virtual_functions; j++) {
	virtual [j] = virtual_functions [j];
      }
      
    }
  else
    {
      klass = g_type_class_ref (G_TYPE_FROM_CLASS (old_class_record));
      
    }

  G_OBJECT_GET_CLASS (object) = klass;
  return klass;
}

void
ada_widget_set_realize (GtkObject *widget, void (* realize) (GtkWidget *))
{
  GTK_WIDGET_GET_CLASS (widget)->realize = realize;
}

void
ada_widget_set_scroll_adjustments_signal (gpointer klass, char* name)
{
  if (GTK_IS_WIDGET_CLASS (klass))
    {
      guint id = g_signal_lookup (name, G_TYPE_FROM_CLASS (klass));
      GTK_WIDGET_CLASS (klass)->set_scroll_adjustments_signal = id;
    }
}

void
ada_gtk_widget_set_default_size_allocate_handler
   (gpointer klass, void (*handler)(GtkWidget        *widget,
				    GtkAllocation    *allocation))
{
  GTK_WIDGET_CLASS (klass)->size_allocate = handler;
}

void
ada_gtk_widget_set_allocation (GtkWidget* widget, GtkAllocation* allocation) {
  widget->allocation = *allocation;
}

gpointer
ada_gtk_default_expose_event_handler (gpointer klass) {
  return GTK_WIDGET_CLASS (klass)->expose_event;
}

/*********************************************************************
 **  Gdk.RGB functions
 *********************************************************************/

guint32
ada_rgb_cmap_get (GdkRgbCmap* cmap, gint index)
{
  return cmap->colors [index];
}

void
ada_rgb_cmap_set (GdkRgbCmap* cmap, gint index, guint32 value)
{
  cmap->colors [index] = value;
}

/******************************************************
 **  Gtk.Socket
 ******************************************************/

GdkWindow*
ada_gtk_socket_get_plug_window (GtkSocket* socket) {
  return socket->plug_window;
}

/*****************************************************
 ** Gtk.Selection and Gtk.Dnd functions
 *****************************************************/

GdkAtom
ada_gtk_dnd_get_selection (GtkSelectionData* selection) {
  return selection->selection;
}

GdkAtom
ada_gtk_dnd_get_target (GtkSelectionData* selection) {
  return selection->target;
}

GdkAtom
ada_gtk_dnd_get_type (GtkSelectionData* selection) {
  return selection->type;
}

gint
ada_gtk_dnd_get_format (GtkSelectionData* selection) {
  return selection->format;
}

guchar*
ada_gtk_dnd_get_data (GtkSelectionData* selection) {
  return selection->data;
}

gint
ada_gtk_dnd_get_length (GtkSelectionData* selection) {
  return selection->length;
}

GdkDragAction
ada_gtk_dnd_context_get_actions (GdkDragContext* context) {
  return context->actions;
}

GdkDragAction
ada_gtk_dnd_context_get_suggested_action (GdkDragContext* context) {
  return context->suggested_action;
}

GdkDragAction
ada_gtk_dnd_context_get_action (GdkDragContext* context) {
  return context->action;
}

/*
 * Gnode macros
 *
 */

gboolean
ada_gnode_is_root (GNode * node)
{
  return G_NODE_IS_ROOT (node);
}

gboolean
ada_gnode_is_leaf (GNode * node)
{
  return G_NODE_IS_LEAF (node);
}

GNode*
ada_gnode_prev_sibling (GNode * node)
{
  return g_node_prev_sibling (node);
}

GNode*
ada_gnode_next_sibling (GNode * node)
{
  return g_node_next_sibling (node);
}

GNode*
ada_gnode_first_child (GNode * node)
{
  return g_node_first_child (node);
}

/*
 *
 * object macros
 *
 */

guint32 ada_object_flags (GtkObject * object)
{
  return GTK_OBJECT_FLAGS (object);
}

void
ada_object_set_flags (GtkObject * object, guint32 flags)
{
  GTK_OBJECT_SET_FLAGS (object, flags);
}

gint
ada_object_flag_is_set (GtkObject * object, guint32 flag)
{
  return ((GTK_OBJECT_FLAGS (object) & flag) != 0);
}

void 
ada_object_unset_flags (GtkObject * object, guint32 flags)
{
  GTK_OBJECT_UNSET_FLAGS (object, flags);
}

/*
 *
 * Widget macros
 *
 */

guint32 
ada_widget_is_sensitive (GtkWidget * widget)
{
  return GTK_WIDGET_IS_SENSITIVE (widget);
}

guint32
ada_widget_drawable (GtkWidget * widget)
{
  return GTK_WIDGET_DRAWABLE (widget);
}

gint
ada_widget_allocation_height (GtkWidget* widget)
{
  return widget->allocation.height;
}

gint
ada_widget_allocation_width (GtkWidget* widget)
{
  return widget->allocation.width;
}

gint
ada_widget_allocation_x (GtkWidget* widget)
{
  return (gint) widget->allocation.x;
}

gint
ada_widget_allocation_y (GtkWidget* widget)
{
  return (gint) widget->allocation.y;
}

/*
 *
 * radio_menu_item
 *
 */

GtkWidget*
ada_radio_menu_item_new_from_widget (GtkRadioMenuItem *group)
{
  GSList *l = NULL;
  if (group)
    l = gtk_radio_menu_item_group (group);
  return gtk_radio_menu_item_new (l);
}

GtkWidget *
ada_radio_menu_item_new_with_label_from_widget (GtkRadioMenuItem *group,
						const gchar      *label)
{
  GSList *l = NULL;
  if (group)
    l = gtk_radio_menu_item_group (group);
  return gtk_radio_menu_item_new_with_label (l, label);
}

/*********************
 * Gtk_Container
 *********************/

GtkWidget* ada_gtk_container_get_focus_child (GtkContainer* container) {
  return container->focus_child;
}

/*********************
 * Gdk_Font support
 *********************/

gint
ada_gdk_font_get_ascent (GdkFont* font) {
  return font->ascent;
}

gint
ada_gdk_font_get_descent (GdkFont* font) {
  return font->descent;
}

/********************
 * Font_Selection
 ********************/

GtkWidget*
ada_gtk_font_selection_dialog_get_cancel (GtkFontSelectionDialog* fsd)
{
  return fsd->cancel_button;
}

GtkWidget*
ada_gtk_font_selection_dialog_get_ok (GtkFontSelectionDialog* fsd)
{
  return fsd->ok_button;
}

GtkWidget*
ada_gtk_font_selection_dialog_get_apply (GtkFontSelectionDialog* fsd)
{
  return fsd->apply_button;
}

/********************
 * Paned
 ********************/

gboolean
ada_paned_get_child1_resize (GtkPaned* widget) {
  return widget->child1_resize;
}

gboolean
ada_paned_get_child2_resize (GtkPaned* widget) {
  return widget->child2_resize;
}

gboolean
ada_paned_get_child1_shrink (GtkPaned* widget) {
  return widget->child1_shrink;
}

gboolean
ada_paned_get_child2_shrink (GtkPaned* widget) {
  return widget->child2_shrink;
}

GtkWidget*
ada_paned_get_child1 (GtkPaned* widget) {
  return widget->child1;
}

GtkWidget*
ada_paned_get_child2 (GtkPaned* widget) {
  return widget->child2;
}

/********************
 * Progress_Bar
 ********************/

GtkAdjustment*
ada_progress_get_adjustment (GtkProgress* widget) {
  return widget->adjustment;
}

guint
ada_progress_get_activity_mode (GtkProgress* widget) {
  return widget->activity_mode;
}

/********************
 * GdkPoint
 ********************/

guint
ada_gdk_point_size () {
  return sizeof (GdkPoint);
}

/*
 *
 *  GdkCursor
 *
 */

GdkCursor* 
ada_gdk_cursor_new (gint cursor_type)
{
  return gdk_cursor_new (cursor_type);
}


/**********************************************************
 **  Graphic contexts
 **********************************************************/

GdkGCValues*
ada_gdk_gc_new_values ()
{
  return (GdkGCValues*) g_new (GdkGCValues, 1);;
}

void
ada_gdk_gc_free_values (GdkGCValues* values)
{
  g_free (values);
}

void
ada_gdk_gc_set_background (GdkGCValues* values,
			   GdkColor     color)
{
  values->background = color;
}

void
ada_gdk_gc_set_foreground (GdkGCValues* values,
			   GdkColor     color)
{
  values->foreground = color;
}

void
ada_gdk_gc_set_clip_origin (GdkGCValues* values,
			    gint x,
			    gint y)
{
  values->clip_x_origin = x;
  values->clip_y_origin = y;
}

void
ada_gdk_gc_set_exposures (GdkGCValues* values,
			  gint exposures)
{
  values->graphics_exposures = exposures;
}

void
ada_gdk_gc_set_fill (GdkGCValues* values,
		     GdkFill      fill)
{
  values->fill = fill;
}

void
ada_gdk_gc_set_font (GdkGCValues* values,
		     GdkFont*     font)
{
  values->font = font;
}

void
ada_gdk_gc_set_function (GdkGCValues* values,
			 GdkFunction  function)
{
  values->function = function;
}

void
ada_gdk_gc_set_line_attributes (GdkGCValues* values,
				gint         line_width,
				GdkLineStyle line_style,
				GdkCapStyle  cap_style,
				GdkJoinStyle join_style)
{
  values->line_width = line_width;
  values->line_style = line_style;
  values->cap_style  = cap_style;
  values->join_style = join_style;
}

void
ada_gdk_gc_set_subwindow (GdkGCValues*      values,
			  GdkSubwindowMode  subwindow_mode)
{
  values->subwindow_mode = subwindow_mode;
}

void
ada_gdk_gc_set_ts_origin (GdkGCValues* values,
			  gint         x,
			  gint         y)
{
  values->ts_x_origin = x;
  values->ts_y_origin = y;
}

/**********************************************************
 **  Support for events
 **********************************************************/

#ifdef _WIN32
#define ada_gdk_invalid_gdouble_value 1.79769313486232e308
#define ada_gdk_invalid_gint_value (2<<31-1)
#define ada_gdk_invalid_guint_value (2<<32-1)
#define ada_gdk_invalid_guint32_value (2<<32-1)
#define ada_gdk_invalid_gulong_value (2<<32-1)

#else
extern const gdouble ada_gdk_invalid_gdouble_value;
extern const gint    ada_gdk_invalid_gint_value;
extern const guint   ada_gdk_invalid_guint_value;
extern const guint32 ada_gdk_invalid_guint32_value;
extern const gulong  ada_gdk_invalid_gulong_value;
#endif

GdkAtom ada_make_atom (gulong num)
{
  return _GDK_MAKE_ATOM (num);
}

gdouble ada_gdk_event_get_x (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.x;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.x;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.x;
    case GDK_CONFIGURE:
      return event->configure.x;
    default:
      break;
    }
  return ada_gdk_invalid_gdouble_value;
}

gdouble ada_gdk_event_get_y (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.y;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.y;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.y;
    case GDK_CONFIGURE:
      return event->configure.y;
    default:
      break;
    }
  return ada_gdk_invalid_gdouble_value;
}

gint ada_gdk_event_get_width (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_CONFIGURE:
      return event->configure.width;
    default:
      break;
    }
  return ada_gdk_invalid_gint_value;
}

gint ada_gdk_event_get_height (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_CONFIGURE:
      return event->configure.height;
    default:
      break;
    }
  return ada_gdk_invalid_gint_value;
}

gdouble ada_gdk_event_get_x_root (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.x_root;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.x_root;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.x_root;
    default:
      break;
    }
  return ada_gdk_invalid_gdouble_value;
}

gdouble ada_gdk_event_get_y_root (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.y_root;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.y_root;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.y_root;
    default:
      break;
    }
  return ada_gdk_invalid_gdouble_value;
}

guint ada_gdk_event_get_button (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.button;
    default:
      break;
    }
  return ada_gdk_invalid_guint_value;
}

guint ada_gdk_event_get_state (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.state;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.state;
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      return event->key.state;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.state;
    case GDK_PROPERTY_NOTIFY:
      return event->property.state;
    default:
      break;
    }
  return ada_gdk_invalid_guint_value;
}

GdkWindow* ada_gdk_event_get_subwindow (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.subwindow;
    default:
      break;
    }
  return NULL;
}

gint ada_gdk_event_get_mode (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.mode;
    default:
      break;
    }
  return ada_gdk_invalid_gint_value;
}

gint ada_gdk_event_get_detail (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.detail;
    default:
      break;
    }
  return ada_gdk_invalid_gint_value;
}

gint ada_gdk_event_get_focus (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.focus;
    default:
      break;
    }
  return ada_gdk_invalid_gint_value;
}

GdkDevice *ada_gdk_event_get_device_id (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.device;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.device;
    case GDK_PROXIMITY_IN:
    case GDK_PROXIMITY_OUT:
      return event->proximity.device;
    default:
      break;
    }
  return NULL;
}

void
ada_gdk_event_get_area (GdkEvent *event, GdkRectangle *area)
{
  if (event->type == GDK_EXPOSE)
    *area = event->expose.area;
  else
    {
      area->width = ada_gdk_invalid_gint_value;
    }
}

gint ada_gdk_event_get_count (GdkEvent * event)
{
  if (event->type == GDK_EXPOSE)
    return event->expose.count;
  return ada_gdk_invalid_gint_value;
}

gint ada_gdk_event_get_in (GdkEvent * event)
{
  if (event->type == GDK_FOCUS_CHANGE) 
    return event->focus_change.in;
  return ada_gdk_invalid_gint_value;
}

gint ada_gdk_event_get_is_hint (GdkEvent * event)
{
  if (event->type == GDK_MOTION_NOTIFY)
    return event->motion.is_hint;
  return ada_gdk_invalid_gint_value;
}

gint ada_gdk_event_get_key_val (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      return event->key.keyval;
    default:
      break;
    }
  return ada_gdk_invalid_gint_value;
}

char* ada_gdk_event_get_string (GdkEvent * event)
{
  switch (event->type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      return event->key.string;
    default:
      break;
    }
  return NULL;
}

GdkAtom ada_gdk_event_get_atom (GdkEvent * event)
{
  if (event->type == GDK_PROPERTY_NOTIFY)
    return event->property.atom;
  return NULL;
}

guint ada_gdk_event_get_property_state (GdkEvent * event)
{
  if (event->type == GDK_PROPERTY_NOTIFY)
    return event->property.state;
  return ada_gdk_invalid_guint_value;
}

gint ada_gdk_event_get_visibility_state (GdkEvent * event)
{
  if (event->type == GDK_VISIBILITY_NOTIFY)
    return event->visibility.state;
  return ada_gdk_invalid_gint_value;
}

GdkAtom ada_gdk_event_get_selection (GdkEvent * event)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    return event->selection.selection;
  return NULL;
}

GdkAtom ada_gdk_event_get_target (GdkEvent * event)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    return event->selection.target;
  return NULL;
}

GdkAtom ada_gdk_event_get_property (GdkEvent * event)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    return event->selection.property;
  return NULL;
}

guint32 ada_gdk_event_get_requestor (GdkEvent * event)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    return event->selection.requestor;
  return ada_gdk_invalid_guint32_value;
}

GdkAtom ada_gdk_event_get_message_type (GdkEvent * event)
{
  if (event->type == GDK_CLIENT_EVENT)
    return event->client.message_type;
  return NULL;
}

GdkEvent * ada_gdk_event_create (gint type, GdkWindow* win)
{
  GdkEvent static_event;
  GdkEvent* event;
  
  static_event.any.type   = type;
  static_event.any.window = win;
  gdk_window_ref (win);
  switch (type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      static_event.key.string = NULL;
      break;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      static_event.crossing.subwindow = NULL;
      break;
    case GDK_DRAG_ENTER:
    case GDK_DRAG_LEAVE:
    case GDK_DRAG_MOTION:
    case GDK_DRAG_STATUS:
    case GDK_DROP_START:
    case GDK_DROP_FINISHED:
      /* ??? will create an error in gdk_event_copy */
      static_event.dnd.context = NULL;
      break;
    }
  event = gdk_event_copy(&static_event);
  return event;
}

short * ada_gdk_event_client_get_s (GdkEventClient * event)
{
  return event->data.s;
}

char * ada_gdk_event_client_get_b (GdkEventClient * event)
{
  return event->data.b;
}

long * ada_gdk_event_client_get_l (GdkEventClient * event)
{
  return event->data.l;
}

gushort ada_gdk_event_client_get_data_format (GdkEventClient * event)
{
  return event->data_format;
}

GdkEventType ada_gdk_event_get_type (GdkEventAny * event)
{
  return event->type;
}

gint8 ada_gdk_event_get_send_event (GdkEventAny * event)
{
  return event->send_event;
}

GdkWindow * ada_gdk_event_get_window (GdkEventAny * event)
{
  return event->window;
}

void ada_gdk_event_set_window (GdkEventAny * event, GdkWindow * window)
{
  event->window = window;
}

/*******************************************************************
 **  Setting the fields of events
 *******************************************************************/

gint ada_gdk_event_set_x (GdkEvent * event, gdouble x)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      event->motion.x = x;
      break;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      event->button.x = x;
      break;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.x = x;
      break;
    case GDK_CONFIGURE:
      event->configure.x = x;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_y (GdkEvent * event, gdouble y)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      event->motion.y = y;
      break;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      event->button.y = y;
      break;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.y = y;
      break;
    case GDK_CONFIGURE:
      event->configure.y = y;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_xroot (GdkEvent * event, gdouble xroot)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      event->motion.x_root = xroot;
      break;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      event->button.x_root = xroot;
      break;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.x_root = xroot;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_yroot (GdkEvent * event, gdouble yroot)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      event->motion.y_root = yroot;
      break;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      event->button.y_root = yroot;
      break;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.y_root = yroot;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_time (GdkEvent * event, guint32 time)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      event->motion.time = time;
      break;
      
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      event->button.time = time;
      break;
      
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      event->key.time = time;
      break;
      
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.time = time;
      break;
      
    case GDK_PROPERTY_NOTIFY:
      event->property.time = time;
      break;
      
    case GDK_SELECTION_NOTIFY:
      event->selection.time = time;
      break;
      
    case GDK_PROXIMITY_IN:
    case GDK_PROXIMITY_OUT:
      event->proximity.time = time;
      break;

    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_width (GdkEvent * event, gint width)
{
  switch (event->type)
    {
    case GDK_CONFIGURE:
      event->configure.width = width;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_height (GdkEvent * event, gint height)
{
  switch (event->type)
    {
    case GDK_CONFIGURE:
      event->configure.height = height;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_button (GdkEvent * event, guint button)
{
  switch (event->type)
    {
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      event->button.button = button;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_state (GdkEvent * event, guint state)
{
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
      event->motion.state = state;
      break;
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      event->button.state = state;
      break;
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      event->key.state = state;
      break;
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.state = state;
      break;
    case GDK_PROPERTY_NOTIFY:
      event->property.state = state;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_subwindow (GdkEvent * event, GdkWindow* win)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.subwindow = win;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_string (GdkEvent * event, char* str)
{
  switch (event->type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      event->key.length = strlen (str);
      event->key.string = str;
      break;
    default:
      return 0;
    }
  return 1;
}


gint ada_gdk_event_set_mode (GdkEvent * event, gint mode)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.mode = mode;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_detail (GdkEvent * event, gint detail)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.detail = detail;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_focus (GdkEvent * event, gint focus)
{
  switch (event->type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      event->crossing.focus = focus;
      break;
    default:
      return 0;
    }
  return 1;
}

gint ada_gdk_event_set_area (GdkEvent * event, GdkRectangle area)
{
  if (event->type == GDK_EXPOSE)
    event->expose.area = area;
  else
    return 0;
  return 1;
}

gint ada_gdk_event_set_in (GdkEvent * event, gint in)
{
  if (event->type == GDK_FOCUS_CHANGE)
    {
      event->focus_change.in = in;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_is_hint (GdkEvent * event, gint is_hint)
{
  if (event->type == GDK_MOTION_NOTIFY)
    {
      event->motion.is_hint = is_hint;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_key_val (GdkEvent * event, gint keyval)
{
  switch (event->type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      event->key.keyval = keyval;
      break;
    default:
      return 0;
    }
  return 1;
}
gint ada_gdk_event_set_atom (GdkEvent * event, GdkAtom atom)
{
  if (event->type == GDK_PROPERTY_NOTIFY)
    {
      event->property.atom = atom;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_property_state (GdkEvent * event, guint state)
{
  if (event->type == GDK_PROPERTY_NOTIFY)
    {
      event->property.state = state;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_visibility_state (GdkEvent * event, gint state)
{
  if (event->type == GDK_VISIBILITY_NOTIFY)
    {
      event->visibility.state = state;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_selection (GdkEvent * event, GdkAtom selection)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    {
      event->selection.selection = selection;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_target (GdkEvent * event, GdkAtom target)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    {
      event->selection.target = target;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_property (GdkEvent * event, GdkAtom property)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    {
      event->selection.property = property;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_requestor (GdkEvent * event, guint32 requestor)
{
  if (event->type == GDK_SELECTION_NOTIFY)
    {
      event->selection.requestor = requestor;
      return 1;
    }
  return 0;
}

gint ada_gdk_event_set_message_type (GdkEvent * event, GdkAtom type)
{
  if (event->type == GDK_CLIENT_EVENT)
    {
      event->client.message_type = type;
      return 1;
    }
  return 0;
}

/********************
 * GtkAdjustment
 ********************/

gdouble
ada_gtk_adjustment_get_page_size (GtkAdjustment * adjustment)
{
  return adjustment->page_size;
}

gdouble
ada_gtk_adjustment_get_page_increment (GtkAdjustment * adjustment)
{
  return adjustment->page_increment;
}

gdouble
ada_adjustment_get_lower (GtkAdjustment * adjustment)
{
  return adjustment->lower;
}

gdouble
ada_adjustment_get_upper (GtkAdjustment * adjustment)
{
  return adjustment->upper;
}

void
ada_adjustment_set_page_increment (GtkAdjustment * adjustment,
				   gdouble value)
{
  adjustment->page_increment = value;
}

void
ada_adjustment_set_step_increment (GtkAdjustment * adjustment,
				   gdouble value)
{
  adjustment->step_increment = value;
}

void
ada_gtk_adjustment_set_lower (GtkAdjustment * adjustment, gdouble lower)
{
  adjustment->lower = lower;
}

void
ada_gtk_adjustment_set_upper (GtkAdjustment * adjustment, gdouble upper)
{
  adjustment->upper = upper;
}

gdouble
ada_gtk_adjustment_get_step_increment (GtkAdjustment * adjustment)
{
  return adjustment->step_increment;
}

void
ada_adjustment_set_page_size (GtkAdjustment * adjustment,
			      gdouble value)
{
  adjustment->page_size = value;
}

/*
 *
 * GtkStyle
 *
 */

void
ada_style_set_font_description (GtkStyle* style, PangoFontDescription* font) {
  style->font_desc = font;
}

PangoFontDescription*
ada_style_get_font_description (GtkStyle* style) {
  return style->font_desc;
}

void
ada_style_set_fg (GtkStyle* style, gint state, GdkColor* color)
{
  if (color != NULL)
    style->fg[state] = *color;
}

GdkColor*
ada_style_get_fg (GtkStyle* style, gint state)
{
  return style->fg + state;
}

void
ada_style_set_bg (GtkStyle* style, gint state, GdkColor* color)
{
  if (color != NULL)
    style->bg[state] = *color;
}

GdkColor*
ada_style_get_bg (GtkStyle* style, gint state)
{
  return style->bg + state;
}

void
ada_style_set_light (GtkStyle* style, gint state, GdkColor* color)
{
  style->light[state] = *color;
}

GdkColor*
ada_style_get_light (GtkStyle* style, gint state)
{
  return style->light + state;
}

void
ada_style_set_dark (GtkStyle* style, gint state, GdkColor* color)
{
  style->dark[state] = *color;
}

GdkColor*
ada_style_get_dark (GtkStyle* style, gint state)
{
  return style->dark + state;
}

void
ada_style_set_mid (GtkStyle* style, gint state, GdkColor* color)
{
  style->mid[state] = *color;
}

GdkColor*
ada_style_get_mid (GtkStyle* style, gint state)
{
  return style->mid + state;
}

void
ada_style_set_text (GtkStyle* style, gint state, GdkColor* color)
{
  style->text[state] = *color;
}

GdkColor*
ada_style_get_text (GtkStyle* style, gint state)
{
  return style->text + state;
}

void
ada_style_set_base (GtkStyle* style, gint state, GdkColor* color)
{
  style->base[state] = *color;
}

GdkColor*
ada_style_get_base (GtkStyle* style, gint state)
{
  return style->base + state;
}

void
ada_style_set_black (GtkStyle* style, GdkColor* color)
{
  style->black = *color;
}

GdkColor*
ada_style_get_black (GtkStyle* style)
{
  return &style->black;
}

void
ada_style_set_white (GtkStyle* style, GdkColor* color)
{
  style->white = *color;
}

GdkColor*
ada_style_get_white (GtkStyle* style)
{
  return &style->white;
}

void
ada_style_set_fg_gc (GtkStyle* style, gint state, GdkGC* gc)
{
  style->fg_gc[state] = gc;
}

GdkGC*
ada_style_get_fg_gc (GtkStyle * style, gint state)
{
  return style->fg_gc [state];
}

void
ada_style_set_bg_gc (GtkStyle* style, gint state, GdkGC* gc)
{
  style->bg_gc[state] = gc;
}

GdkGC*
ada_style_get_bg_gc (GtkStyle * style, gint state)
{
  return style->bg_gc [state];
}

void
ada_style_set_light_gc (GtkStyle* style, gint state, GdkGC* gc)
{
  style->light_gc[state] = gc;
}

GdkGC*
ada_style_get_light_gc (GtkStyle * style, gint state)
{
  return style->light_gc [state];
}

void
ada_style_set_dark_gc (GtkStyle* style, gint state, GdkGC* gc)
{
  style->dark_gc[state] = gc;
}

GdkGC*
ada_style_get_dark_gc (GtkStyle * style, gint state)
{
  return style->dark_gc[state];
}

void
ada_style_set_mid_gc (GtkStyle* style, gint state, GdkGC* gc)
{
  style->mid_gc[state] = gc;
}

GdkGC*
ada_style_get_mid_gc (GtkStyle * style, gint state)
{
  return style->mid_gc [state];
}

void
ada_style_set_text_gc (GtkStyle* style, gint state, GdkGC* gc)
{
  style->text_gc[state] = gc;
}

GdkGC*
ada_style_get_text_gc (GtkStyle * style, gint state)
{
  return style->text_gc [state];
}

void
ada_style_set_base_gc (GtkStyle* style, gint state, GdkGC* gc)
{
  style->base_gc[state] = gc;
}

GdkGC*
ada_style_get_base_gc (GtkStyle * style, gint state)
{
  return style->base_gc [state];
}

void
ada_style_set_black_gc (GtkStyle* style, GdkGC* gc)
{
  style->black_gc = gc;
}

GdkGC*
ada_style_get_black_gc (GtkStyle * style)
{
  return style->black_gc;
}


void
ada_style_set_white_gc (GtkStyle* style, GdkGC* gc)
{
  style->white_gc = gc;
}

GdkGC*
ada_style_get_white_gc (GtkStyle * style)
{
  return style->white_gc;
}



void
ada_style_set_bg_pixmap (GtkStyle* style, gint state, GdkPixmap *pix)
{
  style->bg_pixmap[state] = pix;
}

GdkPixmap*
ada_style_get_bg_pixmap (GtkStyle* style, gint state)
{
  return style->bg_pixmap[state];
}

gint
ada_style_get_x_thickness (GtkStyle* style) {
  return style->xthickness;
}

gint
ada_style_get_y_thickness (GtkStyle* style) {
  return style->ythickness;
}

/***************************************************
 *  Functions for Objects
 ***************************************************/

GType
ada_gobject_get_type (GObject* object)
{
  return G_OBJECT_TYPE (object);
}

/***************************************************
 *  Functions for GClosure
 ***************************************************/

void*
ada_gclosure_get_data (GClosure *closure)
{
  return closure->data;
}

/***************************************************
 *  Functions for GValue
 ***************************************************/

gpointer
ada_gvalue_get_pointer (GValue* value)
{
  return value->data[0].v_pointer;
}

void
ada_gvalue_nth (GValue* value, guint num, GValue* val)
{
  *val = *(value + num);
}

int ada_c_gvalue_size () {
  return sizeof (GValue);
}

void
ada_gvalue_set (GValue* value, void *val)
{
  if G_VALUE_HOLDS_CHAR (value)
    g_value_set_char (value, *(gchar*)val);
  else if G_VALUE_HOLDS_UCHAR (value)
    g_value_set_uchar (value, *(guchar*)val);
  else if G_VALUE_HOLDS_BOOLEAN (value)
    g_value_set_boolean (value, *(char*)val);
  else if G_VALUE_HOLDS_INT (value)
    g_value_set_int (value, *(gint*)val);
  else if G_VALUE_HOLDS_UINT (value)
    g_value_set_uint (value, *(guint*)val);
  else if G_VALUE_HOLDS_LONG (value)
    g_value_set_long (value, *(glong*)val);
  else if G_VALUE_HOLDS_ULONG (value)
    g_value_set_ulong (value, *(gulong*)val);
  else if G_VALUE_HOLDS_FLOAT (value)
    g_value_set_float (value, *(gfloat*)val);
  else if G_VALUE_HOLDS_DOUBLE (value)
    g_value_set_double (value, *(gdouble*)val);
  else if G_VALUE_HOLDS_POINTER (value)
    g_value_set_pointer (value, *(gpointer*)val);
  else
    fprintf (stderr, "GtkAda: Return value type not supported\n");
}

/***************************************************
 *  Functions to get the field of a color selection
 *  dialog
 ***************************************************/

GtkWidget*
ada_colorsel_dialog_get_colorsel (GtkColorSelectionDialog* dialog)
{
  return dialog->colorsel;
}

GtkWidget*
ada_colorsel_dialog_get_ok_button (GtkColorSelectionDialog* dialog)
{
  return dialog->ok_button;
}

GtkWidget*
ada_colorsel_dialog_get_cancel_button (GtkColorSelectionDialog* dialog)
{
  return dialog->cancel_button;
}

GtkWidget*
ada_colorsel_dialog_get_help_button (GtkColorSelectionDialog* dialog)
{
  return dialog->help_button;
}

/********************************************************
 *  Functions to get the fields of a gamma curve
 ********************************************************/

GtkWidget*
ada_gamma_curve_get_curve (GtkGammaCurve* widget)
{
  return widget->curve;
}

gfloat
ada_gamma_curve_get_gamma (GtkGammaCurve* widget)
{
  return widget->gamma;
}

/******************************************
 ** Functions for Alignment
 ******************************************/

gfloat
ada_alignment_get_xalign (GtkAlignment* widget)
{
   return widget->xalign;
}

gfloat
ada_alignment_get_xscale (GtkAlignment* widget)
{
   return widget->xscale;
}

gfloat
ada_alignment_get_yalign (GtkAlignment* widget)
{
   return widget->yalign;
}

gfloat
ada_alignment_get_yscale (GtkAlignment* widget)
{
   return widget->yscale;
}

/******************************************
 ** Functions for Aspect_Frame
 ******************************************/

gfloat
ada_aspect_frame_get_ratio (GtkAspectFrame* widget)
{
   return widget->ratio;
}

gfloat
ada_aspect_frame_get_xalign (GtkAspectFrame* widget)
{
   return widget->xalign;
}

gfloat
ada_aspect_frame_get_yalign (GtkAspectFrame* widget)
{
   return widget->yalign;
}

/******************************************
 ** Functions for Layout
 ******************************************/

guint
ada_gtk_layout_get_width (GtkLayout* layout) {
  /** ??? Should not be needed with gtk_layout_get_size,
      ??? but kept for compatibility with GVD **/
  return layout->width;
}

guint
ada_gtk_layout_get_height (GtkLayout* layout) {
  /** ??? Should not be needed with gtk_layout_get_size,
      ??? but kept for compatibility with GVD **/
  return layout->height;
}

GdkWindow*
ada_gtk_layout_get_bin_window (GtkLayout* layout) {
  return layout->bin_window;
}

/******************************************
 ** Functions for Viewport
 ******************************************/

GdkWindow*
ada_gtk_viewport_get_bin_window (GtkViewport* viewport) {
  return viewport->bin_window;
}

/******************************************
 ** Functions for Text_Attributes
 ******************************************/

PangoFontDescription*
ada_text_attributes_get_font (GtkTextAttributes* text_attr)
{
  return text_attr->font;
}

void
ada_text_attributes_set_font (GtkTextAttributes* text_attr,
                              PangoFontDescription* font)
{
  g_return_if_fail (font != NULL);

  /* Free the family name pointer if already allocated */
  pango_font_description_free (text_attr->font);
  
  /* set the font. Make sure to strdup the font->family_name field
     to avoid dangling pointers. This memory will be deallocated
     during the final unref */
  text_attr->font = pango_font_description_copy (font);
}

/******************************************
 ** Functions for Text_Iter
 ******************************************/

void
ada_text_iter_copy (const GtkTextIter *source,
                    GtkTextIter *dest)
{
  *dest = *source;
}

int ada_c_gtk_text_iter_size () {
  return sizeof (GtkTextIter);
}

/******************************************
 ** Functions for Text_View
 ******************************************/

GtkAdjustment*
ada_text_view_get_hadj (GtkTextView* widget)
{
  return widget->hadjustment;
}

GtkAdjustment*
ada_text_view_get_vadj (GtkTextView* widget)
{
  return widget->vadjustment;
}

/******************************************
 ** Functions for File_Selection
 ******************************************/

GtkWidget*
ada_file_selection_get_action_area (GtkFileSelection* widget)
{
   return widget->action_area;
}
GtkWidget*
ada_file_selection_get_button_area (GtkFileSelection* widget)
{
   return widget->button_area;
}
GtkWidget*
ada_file_selection_get_cancel_button (GtkFileSelection* widget)
{
   return widget->cancel_button;
}

GtkWidget*
ada_file_selection_get_dir_list (GtkFileSelection* widget)
{
   return widget->dir_list;
}

GtkWidget*
ada_file_selection_get_file_list (GtkFileSelection* widget)
{
   return widget->file_list;
}

GtkWidget*
ada_file_selection_get_help_button (GtkFileSelection* widget)
{
   return widget->help_button;
}

GtkWidget*
ada_file_selection_get_history_pulldown (GtkFileSelection* widget)
{
   return widget->history_pulldown;
}

GtkWidget*
ada_file_selection_get_ok_button (GtkFileSelection* widget)
{
   return widget->ok_button;
}

GtkWidget*
ada_file_selection_get_selection_entry (GtkFileSelection* widget)
{
   return widget->selection_entry;
}

GtkWidget*
ada_file_selection_get_selection_text (GtkFileSelection* widget)
{
   return widget->selection_text;
}

/******************************************
 ** Functions for Dialog
 ******************************************/

GtkWidget*
ada_dialog_get_action_area (GtkDialog* widget)
{
  /** ??? Obsolete with Gtk.Dialog.Add_* **/
   return widget->action_area;
}

GtkWidget*
ada_dialog_get_vbox (GtkDialog* widget)
{
  /** ??? Obsolete with Gtk.Dialog.Add_* **/
   return widget->vbox;
}

/******************************************
 ** Functions for Combo
 ******************************************/

GtkWidget*
ada_combo_get_entry (GtkCombo* widget)
{
  return widget->entry;
}

GtkWidget*
ada_combo_get_popup_window (GtkCombo* widget)
{
  return widget->popwin;
}

void
ada_combo_set_entry (GtkCombo* widget, GtkWidget* entry)
{
  widget->entry = entry;
}

GtkWidget*
ada_combo_get_list (GtkCombo* widget)
{
  return widget->list;
}

/********************************************
 ** Functions for Widget
 ********************************************/

gint
ada_widget_get_state (GtkWidget *widget) {
  return widget->state;
}

GdkWindow*
ada_widget_get_window (GtkWidget* widget)
{
  return widget->window;
}

void
ada_widget_set_window (GtkWidget* widget, GdkWindow *window)
{
  widget->window = window;
}

gint
ada_widget_get_motion_notify (GtkWidget* widget, GdkEvent* event)
{
  return (GTK_WIDGET_GET_CLASS (widget)
	  ->motion_notify_event)(widget, (GdkEventMotion*)event);
}

gint
ada_widget_has_default_motion_notify (GtkWidget* widget)
{
  return (GTK_WIDGET_GET_CLASS (widget)->motion_notify_event) != 0;
}

/*******************************************
 ** Functions for Notebook
 *******************************************/

GList*
ada_notebook_get_children (GtkNotebook* widget)
{
  /** ??? Should use gtk_container_get_children instead **/
  return widget->children;
}

/**********************************************
 ** Functions for Box
 **********************************************/

GtkWidget*
ada_box_get_child (GtkBox* widget, gint num)
{
  if (num < g_list_length (widget->children))
    return ((GtkBoxChild*)(g_list_nth_data (widget->children, num)))->widget;
  else
    return NULL;
}

/**********************************************
 ** Functions for Glib.Glist
 **********************************************/

GList*
ada_list_next (GList* list)
{
  if (list)
    return list->next;
  else
    return NULL;
}

GList*
ada_list_prev (GList* list)
{
  if (list)
    return list->prev;
  else
    return NULL;
}

gpointer
ada_list_get_data (GList* list)
{
  return list->data;
}

/**********************************************
 ** Functions for Glib.GSlist
 **********************************************/

GSList*
ada_gslist_next (GSList* list)
{
  if (list)
    return list->next;
  else
    return NULL;
}

gpointer
ada_gslist_get_data (GSList* list)
{
  return list->data;
}

/******************************************
 ** Functions for Fixed
 ******************************************/

GList*
ada_fixed_get_children (GtkFixed* widget)
{
  /** ??? Should use gtk_container_get_children instead **/
   return widget->children;
}

/******************************************
 ** Functions for Status bar
 ******************************************/

GSList *
ada_status_get_messages (GtkStatusbar* widget)
{
  return widget->messages;
}

/******************************************
 ** Functions for GtkList
 ******************************************/

GList*
ada_list_get_children (GtkList* widget)
{
  /** ??? Should use gtk_container_get_children instead **/
   return widget->children;
}

GList*
ada_list_get_selection (GtkList* widget)
{
   return widget->selection;
}

/******************************************
 ** Functions for Gtk_CList
 ******************************************/

gint
ada_clist_get_focus_row (GtkCList* widget)
{
  return widget->focus_row;
}

gint
ada_clist_get_columns (GtkCList* clist)
{
  return clist->columns;
}

gint
ada_clist_get_rows (GtkCList* clist)
{
  return clist->rows;
}

GList*
ada_clist_get_selection (GtkCList* widget)
{
  return widget->selection;
}

gint
ada_clist_get_selection_mode (GtkCList* widget)
{
  return widget->selection_mode;
}

GdkWindow*
ada_clist_get_clist_window (GtkCList* widget)
{
  return widget->clist_window;
}

GList*
ada_clist_get_row_list (GtkCList* widget)
{
  return widget->row_list;
}

GtkSortType
ada_gtk_clist_get_sort_type (GtkCList* widget) {
  return widget->sort_type;
}

gint
ada_gtk_clist_get_sort_column (GtkCList* widget) {
  return widget->sort_column;
}

void
ada_gtk_clist_set_cell_contents (GtkCList* clist,
				 GtkCListRow* row,
				 gint column,
				 GtkCellType type,
				 char* string,
				 guint8 spacing,
				 GdkPixmap *pixmap,
				 GdkBitmap *mask)
{
  GTK_CLIST_GET_CLASS (clist)->set_cell_contents
    (clist, row, column, type, string, spacing, pixmap, mask);
}

int
ada_gtk_clist_get_text (GtkCList* clist,
			GtkCListRow * row,
			gint column,
			gchar** string)
{
  if (row->cell[column].type == GTK_CELL_TEXT)
    *string = GTK_CELL_TEXT (row->cell[column])->text;
  else if (row->cell[column].type == GTK_CELL_PIXTEXT)
    *string = GTK_CELL_PIXTEXT (row->cell[column])->text;
  else
    return 0;
  return 1;
}

int
ada_gtk_clist_get_pixmap (GtkCList    *clist,
			  GtkCListRow *row,
			  gint         column,
			  GdkPixmap  **pixmap,
			  GdkBitmap  **mask)
{
  if (row->cell[column].type == GTK_CELL_PIXMAP) {
    *pixmap = GTK_CELL_PIXMAP (row->cell[column])->pixmap;
    *mask = GTK_CELL_PIXMAP (row->cell[column])->mask;
  } else if (row->cell[column].type == GTK_CELL_PIXTEXT) {
    *pixmap = GTK_CELL_PIXTEXT (row->cell[column])->pixmap;
    *mask = GTK_CELL_PIXTEXT (row->cell[column])->mask;
  } else {
    return 0;
  }
  return 1;
}

gpointer
ada_gtk_clist_get_row_data (GtkCList    *clist,
			    GtkCListRow *row)
{
  return row->data;
}

void
ada_gtk_clist_set_row_data_full (GtkCList        *clist,
				 GtkCListRow     *row,
				 gpointer         data,
				 GtkDestroyNotify destroy)
{
  row->data = data;
  row->destroy = destroy;
}

/******************************************
 ** Functions for CTree
 ******************************************/

GtkCTreeExpanderStyle
ada_ctree_get_expander_style (GtkCTree* widget)
{
  return widget->expander_style;
}

GtkCTreeLineStyle
ada_ctree_get_line_style (GtkCTree* widget)
{
  return widget->line_style;
}

gboolean
ada_ctree_get_show_stub (GtkCTree* widget)
{
  return widget->show_stub;
}

gint
ada_ctree_get_tree_column (GtkCTree* widget)
{
   return widget->tree_column;
}

gint
ada_ctree_get_tree_indent (GtkCTree* widget)
{
   return widget->tree_indent;
}

gint
ada_ctree_get_tree_spacing (GtkCTree* widget)
{
   return widget->tree_spacing;
}

GtkCTreeRow*
ada_ctree_node_get_row (GtkCTreeNode* node)
{
  return GTK_CTREE_ROW (node);
}

GtkCTreeNode*
ada_ctree_row_get_children (GtkCTreeRow* row)
{
  g_return_val_if_fail (row != NULL, NULL);
  return row->children;
}

guint
ada_ctree_row_get_expanded (GtkCTreeRow* row)
{
  g_assert (row != NULL);
  return row->expanded;
}

guint
ada_ctree_row_get_is_leaf (GtkCTreeRow* row)
{
  g_assert (row != NULL);
  return row->is_leaf;
}

GtkCTreeNode*
ada_ctree_row_get_parent (GtkCTreeRow* row)
{
  g_return_val_if_fail (row != NULL, NULL);
  return row->parent;
}

GtkCTreeNode*
ada_ctree_row_get_sibling (GtkCTreeRow* row)
{
  g_return_val_if_fail (row != NULL, NULL);
  return row->sibling;
}


/*
 *
 * GdkWindowAttr
 *
 */

GdkWindowAttr*
ada_gdk_window_attr_new (void)
{
  GdkWindowAttr *result;
  
  result = (GdkWindowAttr*) g_new (GdkWindowAttr, 1);

  if (result)
    {
      result->title = NULL;
      result->visual = NULL;
      result->colormap = NULL;
      result->cursor = NULL;
      result->wmclass_name = NULL;
      result->wmclass_class = NULL;
      /* 
       * Here, we only set the pointers to NULL to avoid any dangling
       * pointer. All the other values are left as is. It is the
       * responsibility of the client to make sure they are properly
       * set before they are accessed.
       */
    }

  return result;
}

void
ada_gdk_window_attr_destroy (GdkWindowAttr *window_attr)
{
  g_return_if_fail (window_attr != NULL);

  if (window_attr->title) g_free (window_attr->title);
  if (window_attr->wmclass_name) g_free (window_attr->wmclass_name);
  if (window_attr->wmclass_class) g_free (window_attr->wmclass_class);

  g_free (window_attr);
}

void
ada_gdk_window_attr_set_title (GdkWindowAttr *window_attr,
			       gchar * title)
{
  g_return_if_fail (window_attr != NULL);

  if (window_attr->title) g_free (window_attr->title);
  window_attr->title = g_strdup (title);
}

gchar*
ada_gdk_window_attr_get_title (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->title;
}

void
ada_gdk_window_attr_set_event_mask (GdkWindowAttr *window_attr,
				    gint event_mask)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->event_mask = event_mask;
}

gint
ada_gdk_window_attr_get_event_mask (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->event_mask;
}

void
ada_gdk_window_attr_set_x (GdkWindowAttr * window_attr, gint x)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->x = x;
}

gint
ada_gdk_window_attr_get_x (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->x;
}

void
ada_gdk_window_attr_set_y (GdkWindowAttr * window_attr, gint y)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->y = y;
}
  
gint
ada_gdk_window_attr_get_y (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->y;
}

void
ada_gdk_window_attr_set_width (GdkWindowAttr * window_attr, gint width)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->width = width;
}
  
gint
ada_gdk_window_attr_get_width (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->width;
}

void
ada_gdk_window_attr_set_height (GdkWindowAttr * window_attr, gint height)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->height = height;
}
  
gint
ada_gdk_window_attr_get_height (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->height;
}

void
ada_gdk_window_attr_set_wclass (GdkWindowAttr *window_attr,
				GdkWindowClass wclass)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->wclass = wclass;
}

GdkWindowClass
ada_gdk_window_attr_get_wclass (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, GDK_INPUT_OUTPUT);
  
  return window_attr->wclass;
}

void
ada_gdk_window_attr_set_visual (GdkWindowAttr *window_attr,
				GdkVisual *visual)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->visual = visual;
}

GdkVisual*
ada_gdk_window_attr_get_visual (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->visual;
}

void
ada_gdk_window_attr_set_colormap (GdkWindowAttr *window_attr,
				  GdkColormap *colormap)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->colormap = colormap;
}

GdkColormap*
ada_gdk_window_attr_get_colormap (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->colormap;
}

void
ada_gdk_window_attr_set_window_type (GdkWindowAttr *window_attr,
				     GdkWindowType window_type)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->window_type = window_type;
}

GdkWindowType
ada_gdk_window_attr_get_window_type (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, GDK_WINDOW_ROOT);
  
  return window_attr->window_type;
}

void
ada_gdk_window_attr_set_cursor (GdkWindowAttr *window_attr,
				GdkCursor *cursor)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->cursor = cursor;
}

GdkCursor*
ada_gdk_window_attr_get_cursor (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->cursor;
}

void
ada_gdk_window_attr_set_wmclass_name (GdkWindowAttr *window_attr,
				      gchar *wmclass_name)
{
  g_return_if_fail (window_attr != NULL);

  if (window_attr->wmclass_name) g_free (window_attr->wmclass_name);
  window_attr->wmclass_name = g_strdup (wmclass_name);
}

gchar*
ada_gdk_window_attr_get_wmclass_name (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->wmclass_name;
}

void
ada_gdk_window_attr_set_wmclass_class (GdkWindowAttr *window_attr,
				      gchar *wmclass_class)
{
  g_return_if_fail (window_attr != NULL);
  
  if (window_attr->wmclass_class) g_free (window_attr->wmclass_class);
  window_attr->wmclass_class = g_strdup (wmclass_class);
}

gchar*
ada_gdk_window_attr_get_wmclass_class (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->wmclass_class;
}

void
ada_gdk_window_attr_set_override_redirect (GdkWindowAttr *window_attr,
					   gboolean override_redirect)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->override_redirect = override_redirect;
}

gboolean
ada_gdk_window_attr_get_override_redirect (GdkWindowAttr * window_attr)
{
  g_return_val_if_fail (window_attr != NULL, FALSE);
  
  return window_attr->override_redirect;
}

/*
 *
 * Gdk properties
 *
 */

void
ada_gdk_property_get (GdkWindow	 *window,
		      GdkAtom     property,
		      GdkAtom     type,
		      gulong      offset,
		      gulong      length,
		      gint        pdelete,
		      GdkAtom    *actual_property_type,
		      gint       *actual_format,
		      gint       *actual_length,
		      guchar    **data,
		      gint       *success)
{
  *success = gdk_property_get (window, property, type, offset, length,
			       pdelete, actual_property_type, actual_format,
			       actual_length, data);
}


/******************************************
 ** GEnumClass                           **
 ******************************************/

int ada_c_enum_value_size () {
  return sizeof (GEnumValue);
}

GEnumValue* ada_genum_nth_value (GEnumClass* klass, guint nth) {
  return (nth < klass->n_values) ? &(klass->values[nth]) : NULL;
}

gint ada_genum_get_value (GEnumValue* value) {
  return value->value;
}

gchar* ada_genum_get_name (GEnumValue* value) {
  return value->value_name;
}

gchar* ada_genum_get_nick (GEnumValue* value) {
  return value->value_nick;
}

/******************************************
 ** GFlags                               **
 ******************************************/

GFlagsValue* ada_gflags_nth_value (GFlagsClass* klass, guint nth) {
  return (nth < klass->n_values) ? &(klass->values[nth]) : NULL;
}

gint ada_gflags_get_value (GFlagsValue* value) {
  return value->value;
}

gchar* ada_gflags_get_name (GFlagsValue* value) {
  return value->value_name;
}

gchar* ada_gflags_get_nick (GFlagsValue* value) {
  return value->value_nick;
}

/******************************************
 ** GParamSpec                           **
 ******************************************/

char* ada_gparam_get_name (GParamSpec* param) {
  return param->name;
}

GParamFlags ada_gparam_get_flags (GParamSpec* param) {
  return param->flags;
}

GType ada_gparam_get_value_type (GParamSpec* param) {
  return G_PARAM_SPEC_VALUE_TYPE (param);
}

void ada_gparam_set_value_type (GParamSpec* param, GType value_type) {
  G_PARAM_SPEC_VALUE_TYPE (param) = value_type;
}

gint8 ada_gparam_get_minimum_char (GParamSpecChar* param) {
  return param->minimum;
}

gint8 ada_gparam_get_maximum_char (GParamSpecChar* param) {
  return param->maximum;
}

gint8 ada_gparam_get_default_char (GParamSpecChar* param) {
  return param->default_value;
}

guint8 ada_gparam_get_minimum_uchar (GParamSpecUChar* param) {
  return param->minimum;
}

guint8 ada_gparam_get_maximum_uchar (GParamSpecUChar* param) {
  return param->maximum;
}

guint8 ada_gparam_get_default_uchar (GParamSpecUChar* param) {
  return param->default_value;
}

gboolean ada_gparam_get_default_boolean (GParamSpecBoolean* param) {
  return param->default_value;
}

gint ada_gparam_get_minimum_int (GParamSpecInt* param) {
  return param->minimum;
}

gint ada_gparam_get_maximum_int (GParamSpecInt* param) {
  return param->maximum;
}

gint ada_gparam_get_default_int (GParamSpecInt* param) {
  return param->default_value;
}

guint ada_gparam_get_minimum_uint (GParamSpecUInt* param) {
  return param->minimum;
}

guint ada_gparam_get_maximum_uint (GParamSpecUInt* param) {
  return param->maximum;
}

guint ada_gparam_get_default_uint (GParamSpecUInt* param) {
  return param->default_value;
}

glong ada_gparam_get_minimum_long (GParamSpecLong* param) {
  return param->minimum;
}

glong ada_gparam_get_maximum_long (GParamSpecLong* param) {
  return param->maximum;
}

glong ada_gparam_get_default_long (GParamSpecLong* param) {
  return param->default_value;
}

gulong ada_gparam_get_minimum_ulong (GParamSpecULong* param) {
  return param->minimum;
}

gulong ada_gparam_get_maximum_ulong (GParamSpecULong* param) {
  return param->maximum;
}

gulong ada_gparam_get_default_ulong (GParamSpecULong* param) {
  return param->default_value;
}

gunichar ada_gparam_get_default_unichar (GParamSpecUnichar* param) {
  return param->default_value;
}

gint ada_gparam_get_default_enum (GParamSpecEnum* param) {
  return param->default_value;
}

GEnumClass* ada_gparam_get_enum_class_enum (GParamSpecEnum* param) {
  return param->enum_class;
}

GFlagsClass* ada_gparam_get_flags_flags (GParamSpecFlags* param) {
  return param->flags_class;
}

glong ada_gparam_get_default_flags (GParamSpecFlags* param) {
  return param->default_value;
}

gfloat ada_gparam_get_minimum_gfloat (GParamSpecFloat* param) {
  return param->minimum;
}

gfloat ada_gparam_get_maximum_gfloat (GParamSpecFloat* param) {
  return param->maximum;
}

gfloat ada_gparam_get_default_gfloat (GParamSpecFloat* param) {
  return param->default_value;
}

gfloat ada_gparam_get_epsilon_gfloat (GParamSpecFloat* param) {
  return param->epsilon;
}

gdouble ada_gparam_get_minimum_gdouble (GParamSpecDouble* param) {
  return param->minimum;
}

gdouble ada_gparam_get_maximum_gdouble (GParamSpecDouble* param) {
  return param->maximum;
}

gdouble ada_gparam_get_default_gdouble (GParamSpecDouble* param) {
  return param->default_value;
}

gdouble ada_gparam_get_epsilon_gdouble (GParamSpecDouble* param) {
  return param->epsilon;
}

gchar* ada_gparam_default_string (GParamSpecString* param) {
  return param->default_value;
}

gchar* ada_gparam_cset_first_string (GParamSpecString* param) {
  return param->cset_first;
}

gchar* ada_gparam_cset_nth_string (GParamSpecString* param) {
  return param->cset_nth;
}

gchar ada_gparam_substitutor_string (GParamSpecString* param) {
  return param->substitutor;
}

gboolean ada_gparam_ensure_non_null_string (GParamSpecString* param) {
  return param->ensure_non_null != 0;
}

/******************************************
 ** New widgets
 ******************************************/

void ada_set_properties_handlers (gpointer klass,
				  GObjectSetPropertyFunc set_handler,
				  GObjectGetPropertyFunc get_handler)
{
  G_OBJECT_CLASS (klass)->set_property = set_handler;
  G_OBJECT_CLASS (klass)->get_property = get_handler;
}

GObjectGetPropertyFunc ada_real_get_property_handler (GObject* object)
{
  GTypeQuery query;
  g_type_query (G_TYPE_FROM_INSTANCE (object), &query);
  return *(GObjectGetPropertyFunc*)((char*)(G_OBJECT_GET_CLASS (object))
				  + query.class_size
				    - sizeof (GObjectGetPropertyFunc)
				  - sizeof (GObjectSetPropertyFunc));
}

void
ada_set_real_get_property_handler (gpointer klass,
				   GObjectGetPropertyFunc handler)
{
  GTypeQuery query;
  g_type_query (G_TYPE_FROM_CLASS (klass), &query);
  *(GObjectGetPropertyFunc*)((char*)(klass)
			     + query.class_size
			     - sizeof (GObjectGetPropertyFunc)
			     - sizeof (GObjectSetPropertyFunc)) = handler;
}

GObjectSetPropertyFunc ada_real_set_property_handler (GObject* object)
{
  GTypeQuery query;
  g_type_query (G_TYPE_FROM_INSTANCE (object), &query);
  return *(GObjectSetPropertyFunc*)((char*)(G_OBJECT_GET_CLASS (object))
				  + query.class_size
				  - sizeof (GObjectSetPropertyFunc));
}

void
ada_set_real_set_property_handler (gpointer klass,
				   GObjectSetPropertyFunc handler)
{
  GTypeQuery query;
  g_type_query (G_TYPE_FROM_CLASS (klass), &query);
  *(GObjectSetPropertyFunc*)((char*)(klass)
			    + query.class_size
			    - sizeof (GObjectSetPropertyFunc)) = handler;
}

void
ada_genum_create_enum_value
(gint value, gchar* name, gchar* nick, GEnumValue* val)
{
  val->value = value;
  val->value_name = g_strdup (name);
  val->value_nick = g_strdup (nick);
}

/******************************************
 ** GType                                **
 ******************************************/

GType ada_g_object_get_type (GObject* object) {
  return G_OBJECT_TYPE (object);
}

GType ada_gtype_fundamental (GType type) {
  return G_TYPE_FUNDAMENTAL (type);
}
