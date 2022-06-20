/*
-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2022, AdaCore                   --
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
#include <string.h>
#include <glib.h>
#include <glib/gspawn.h>
#include <pango/pango.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>

#ifdef GDK_WINDOWING_QUARTZ
#include "misc_osx.h"
#endif

#ifdef GDK_WINDOWING_WIN32
#include <windows.h>
#include <ddeml.h>
#endif

// Workaround bug in gtk+ 3.14.5
#ifndef GTK_IS_PLUG
int GTK_IS_PLUG(void* object) { return 0; }
#endif

/********************************************************************
 *  Returns the major/minor/macro version number of Gtk+. This is
 *  needed as the windows version uses a different convention for the
 *  corresponding variables gtk_{major/minor/micro)_version than the
 *  unix version.
 ********************************************************************/

guint
ada_gtk_major_version ()
{
  return GTK_MAJOR_VERSION;
}

guint
ada_gtk_minor_version ()
{
  return GTK_MINOR_VERSION;
}

guint
ada_gtk_micro_version ()
{
  return GTK_MICRO_VERSION;
}

/********************************************************************
 **  wrappers for functions which vary on Windows
 ********************************************************************/

gchar *
ada_g_filename_from_uri (const gchar *uri,
			 gchar **hostname,
			 GError **error) {
#ifdef GDK_WINDOWING_WIN32
  return g_filename_from_uri_utf8 (uri, hostname, error);
#else
  return g_filename_from_uri (uri, hostname, error);
#endif
}

gchar *
ada_g_filename_from_utf8 (const gchar *utf8string,
		      gssize len,
		      gsize *bytes_read,
		      gsize *bytes_written,
			  GError **error)
{
#ifdef GDK_WINDOWING_WIN32
  return g_filename_from_utf8_utf8
    (utf8string, len, bytes_read, bytes_written, error);
#else
  return g_filename_from_utf8
    (utf8string, len, bytes_read, bytes_written, error);
#endif
}

gchar *
ada_g_filename_to_uri (const gchar *filename,
		       const gchar *hostname,
		       GError **error)
{
#ifdef GDK_WINDOWING_WIN32
  return g_filename_to_uri_utf8 (filename, hostname, error);
#else
  return g_filename_to_uri (filename, hostname, error);
#endif
}

gchar *
ada_g_filename_to_utf8 (const gchar *opsysstring,
			gssize len,
			gsize *bytes_read,
			gsize *bytes_written,
			GError **error)
{
#ifdef GDK_WINDOWING_WIN32
  return g_filename_to_utf8_utf8
    (opsysstring, len, bytes_read, bytes_written, error);
#else
  return g_filename_to_utf8
    (opsysstring, len, bytes_read, bytes_written, error);
#endif
}

GdkPixbuf *
ada_gdk_pixbuf_new_from_file (const char *filename,
			      GError **error)
{
#ifdef GDK_WINDOWING_WIN32
  return gdk_pixbuf_new_from_file_utf8 (filename, error);
#else
  return gdk_pixbuf_new_from_file (filename, error);
#endif
}

GdkPixbufAnimation *
ada_gdk_pixbuf_animation_new_from_file (const char *filename,
					GError **error)
{
#ifdef GDK_WINDOWING_WIN32
  return gdk_pixbuf_animation_new_from_file_utf8 (filename, error);
#else
  return gdk_pixbuf_animation_new_from_file (filename, error);
#endif
}

/********************************************************************
 ** gmodule wrappers, deactivated under Windows
 ********************************************************************/

#ifdef GDK_WINDOWING_WIN32
gboolean ada_g_module_supported (void) { return FALSE; };

gchar*
ada_g_module_build_path (const gchar *directory,
			 const gchar *module_name)
{
  return "";
}
GModule*
ada_g_module_open (const gchar *file_name,
	       GModuleFlags flags)
{
  return 0;
}

gboolean
ada_g_module_close (GModule *module)
{
  return TRUE;
}

gboolean
ada_g_module_symbol (GModule *module,
		 const gchar *symbol_name,
		 gpointer *symbol)
{
  return FALSE;
}

const gchar*
ada_g_module_name (GModule *module)
{
  return "";
}

const gchar*
ada_g_module_error (void)
{
  return "modules not supported under Windows";
}
#else
gboolean ada_g_module_supported (void)
{
  return g_module_supported();
};

gchar*
ada_g_module_build_path (const gchar *directory,
			 const gchar *module_name)
{
  return g_module_build_path (directory, module_name);
}
GModule*
ada_g_module_open (const gchar *file_name,
	       GModuleFlags flags)
{
  return g_module_open (file_name, flags);
}

gboolean
ada_g_module_close (GModule *module)
{
  return g_module_close (module);
}

gboolean
ada_g_module_symbol (GModule *module,
		 const gchar *symbol_name,
		 gpointer *symbol)
{
  return g_module_symbol (module, symbol_name, symbol);
}

const gchar*
ada_g_module_name (GModule *module)
{
  return g_module_name (module);
}

const gchar*
ada_g_module_error (void)
{
  return g_module_error ();
}
#endif

/********************************************************************
 **  var_arg wrappers.
 ********************************************************************/

gpointer
ada_g_object_new (GType object_type)
{
  return g_object_new (object_type, NULL);
}

gpointer
ada_g_dialog_new (GType object_type, GtkDialogFlags flags)
{
   return g_object_new
      (object_type,
       "use-header-bar", (flags & GTK_DIALOG_USE_HEADER_BAR) != 0,
       NULL);
}

void
ada_g_object_get_ulong (gpointer object,
		        const gchar *property_name,
		        gulong *property)
{
  g_object_get (object, property_name, property, NULL);
}


void
ada_g_object_set_string (gpointer object,
			 const gchar *property_name,
			 const gchar *property)
{
  g_object_set (object, property_name, property, NULL);
}

void
ada_g_object_set_int (gpointer object,
		      const gchar *property_name,
		      gint property)
{
  g_object_set (object, property_name, property, NULL);
}

void
ada_g_object_set_ulong (gpointer object,
		        const gchar *property_name,
		        gulong property)
{
  g_object_set (object, property_name, property, NULL);
}

void
ada_g_object_set_ptr (gpointer object,
		      const gchar *property_name,
		      void *property)
{
  g_object_set (object, property_name, property, NULL);
}

void
ada_g_object_set_float (gpointer object,
			const gchar *property_name,
			gfloat property)
{
  g_object_set (object, property_name, property, NULL);
}

void
ada_g_object_set_double (gpointer object,
			 const gchar *property_name,
			 gdouble property)
{
  g_object_set (object, property_name, property, NULL);
}

void
ada_g_signal_emit_by_name (gpointer     instance,
			   const gchar *detailed_signal)
{
  g_signal_emit_by_name (instance, detailed_signal);
}

void
ada_g_signal_emit_by_name_ptr (gpointer     instance,
			       const gchar *detailed_signal,
			       void *arg)
{
  g_signal_emit_by_name (instance, detailed_signal, arg);
}

void
ada_g_signal_emit_by_name_ptr_ptr (gpointer     instance,
			           const gchar *detailed_signal,
			           void *arg1,
			           void *arg2)
{
  g_signal_emit_by_name (instance, detailed_signal, arg1, arg2);
}

void
ada_g_signal_emit_by_name_int_ptr (gpointer     instance,
			           const gchar *detailed_signal,
			           gint arg1,
			           void *arg2)
{
  g_signal_emit_by_name (instance, detailed_signal, arg1, arg2);
}

void
ada_gtk_list_store_set_ptr (GtkListStore *list_store,
                            GtkTreeIter  *iter,
                            gint          col,
                            void         *val)
{
  gtk_list_store_set (list_store, iter, col, val, -1);
}


void
ada_gtk_list_store_set_int (GtkListStore *list_store,
                            GtkTreeIter  *iter,
                            gint          col,
                            gint          val)
{
  gtk_list_store_set (list_store, iter, col, val, -1);
}

void
ada_gtk_tree_store_set_ptr (GtkTreeStore *tree_store,
			    GtkTreeIter  *iter,
			    gint          col,
			    void         *val)
{
  gtk_tree_store_set (tree_store, iter, col, val, -1);
}

void
ada_gtk_tree_store_set_int (GtkTreeStore *tree_store,
			    GtkTreeIter  *iter,
			    gint          col,
			    gint          val)
{
  gtk_tree_store_set (tree_store, iter, col, val, -1);
}

void
ada_gtk_tree_store_set_ulong (GtkTreeStore *list_store,
                            GtkTreeIter  *iter,
                            gint          col,
                            gulong        val)
{
  gtk_tree_store_set (list_store, iter, col, val, -1);
}


GtkWidget*
ada_gtk_dialog_new_with_buttons (const gchar     *title,
                                 GtkWindow       *parent,
                                 GtkDialogFlags   flags)
{
  return gtk_dialog_new_with_buttons (title, parent, flags,
                                      NULL /* first_button_text*/, NULL);
}

gboolean
ada_gdk_pixbuf_save (GdkPixbuf  *pixbuf,
		     const char *filename,
		     const char *type,
		     GError    **error,
		     char       *key,
		     char       *value)
{
  return gdk_pixbuf_save (pixbuf, filename, type, error, key, value, NULL);
}

void
ada_g_log (const gchar    *log_domain,
	   GLogLevelFlags  log_level,
	   const gchar    *message)
{
  g_log (log_domain, log_level, "%s", message);
}

void
c_sprintf (char *s, char *format, int arg1, int arg2, int arg3)
{
  sprintf (s, format, arg1, arg2, arg3);
}

void
ada_gtk_tree_model_get (GtkTreeModel *tree_model,
                        GtkTreeIter  *iter,
			gint         column,
			void         *data)
{
   gtk_tree_model_get (tree_model, iter, column, data, -1);
}

/********************************************************************
 **  This function should only be used for debug purposes.
 ********************************************************************/

guint
ada_gtk_debug_get_ref_count (GObject* object) {
  return G_OBJECT (object)->ref_count;
}

/******************************************
 ** GSignal                              **
 ******************************************/

const gchar*
ada_gsignal_query_signal_name (GSignalQuery* query)
{
  return query->signal_name;
}

const GType*
ada_gsignal_query_params (GSignalQuery* query, guint* n_params)
{
  *n_params = query->n_params;
  return query->param_types;
}

guint
ada_gsignal_query_id (GSignalQuery* query)
{
  return query->signal_id;
}

GType
ada_gsignal_query_return_type (GSignalQuery* query)
{
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
 **     GObjectGetPropertyFunc real_get_property;
 **                                            // pointer to the get_property
 **                                            // in user's code
 **     GObjectSetPropertyFunc real_set_property;
 **                                            // likewise for set_property
 **  };
 *********************************************************************/

typedef struct {
   GType type;            // The type. This also acts as a lock while
                          // initializing the class record.
   void (*class_init)(gpointer type);           // ada class init
   GObjectSetPropertyFunc ada_property_setter;
   GObjectGetPropertyFunc ada_property_getter;
} AdaGObjectClassRecord;
typedef AdaGObjectClassRecord* AdaGObjectClass;
//  Type must be synchronized with Ada.

GType ada_type_from_class (GObjectClass* klass)
{
  return G_TYPE_FROM_CLASS (klass);
}

/** Reuse the dummy padding pointers set by gtk+. They are never used by
 *  gtk+, and just there for binary compatibility. Simpler to use them than
 *  do pointer arithmetics to find the pointer on the class structure.
 */
#define ADA_CLASS_FROM_C_CLASS(class) (AdaGObjectClass)(class->pdummy[0])
#define SET_ADA_CLASS_FROM_C_CLASS(class, adaclass) \
   (class->pdummy[0] = (gpointer)adaclass)

AdaGObjectClass ada_gobject_class_from_object(GObject* object) {
   return ADA_CLASS_FROM_C_CLASS(G_OBJECT_GET_CLASS(object));
}


/** The class_init for the gobject types created in Ada
 * The class_data is of type AdaGObjectClass, as set by
 * ada_initialize_class_record
 **/

static void
ada_class_record_init (GObjectClass* klass, gpointer class_data)
{
   GType type = G_TYPE_FROM_CLASS (klass);
   GType parent = g_type_parent (type);
   GTypeQuery query;
   GTypeQuery parent_query;
   const AdaGObjectClass ada_klass = (AdaGObjectClass)class_data;

   g_type_query (type, &query);
   g_type_query (parent, &parent_query);

   /* Initialize the function pointers for the new signals to NULL */

   memset ((char*)(klass) + parent_query.class_size, 0,
           query.class_size - parent_query.class_size);

   /* Set a pointer to the AdaGObjectClass, so that we can retrieve it
    * later from a type.
    */
   SET_ADA_CLASS_FROM_C_CLASS(klass, ada_klass);

   if (ada_klass->class_init) {
      ada_klass->class_init (klass);
   }
}

int
ada_initialize_class_record
  (GType         ancestor,
   gint          nsignals,
   char*         signals[],
   GType         parameters[],
   gint          max_parameters,
   GType         returns[],
   gint          max_returns,
   AdaGObjectClass klass,
   gchar*        type_name)
{
   // Make this function thread-safe and ensure we only initialize the class
   // once
   if (g_once_init_enter (&klass->type)) {
       /* Note: The memory allocated in this function is never freed. No need
          to worry, since this is only allocated once per user's widget type,
          and might be used until the end of the application */

       GTypeQuery query;
       int j;

       /* We need to know the ancestor's class/instance sizes */
       g_type_query (ancestor, &query);

       /*************************
        * This code is the equivalent of type_name@@_get_type in C. In Ada, the
        * type will be accessible only once at least one instance of it has
        * been created (whereas in C the GType is created at elaboration time).
        *************************/

       GTypeInfo info;
       info.class_size = query.class_size
              + nsignals * sizeof (void*);
              //+ sizeof (AdaGObjectClass);
       info.base_init = NULL;
       info.base_finalize = NULL;
       info.class_init = (GClassInitFunc)(&ada_class_record_init);
       info.class_finalize = NULL;
       info.class_data = (gconstpointer)klass;
       info.instance_size = query.instance_size;
       info.n_preallocs = 0;
       info.instance_init = NULL;
       info.value_table = NULL;

       GType new_type = g_type_register_static (
             ancestor  /* parent_type */,
             type_name /* type_name */,
             &info     /* info */,
             0         /* flags */);

       /*************************
        * This code is generally called by g_object_new (which itself is called
        * from type_name_new() in C). Its result is to create and initialized
        * (via class_init) the class the first time an instance of it is
        * created. In Ada, we do not us a _class_init, so we initialize the
        * signals immediately after creating the class.
        *************************/

       for (j = 0; j < nsignals; j++) {
          int count = 0;
          GClosure *closure;

          while (count < max_parameters &&
                  (parameters [j * max_parameters + count] != G_TYPE_NONE))
          {
                count++;
          }

          closure = g_signal_type_cclosure_new
              (new_type, query.class_size + j * sizeof (void*)); /* offset */

          GType return_type = G_TYPE_NONE;
          GSignalAccumulator acc = NULL;

          if (j < max_returns) {
             return_type = returns[j];
             if (return_type == G_TYPE_BOOLEAN) {
                acc = g_signal_accumulator_true_handled;
             }
          }

          /* id = */ g_signal_newv
            (signals[j],                       /* signal_name */
             new_type,                         /* itype */
             G_SIGNAL_RUN_LAST,                /* signal_flags */
             closure,                          /* class_closure */
             acc,                              /* accumulator */
             NULL,                             /* accu_data */
             g_cclosure_marshal_VOID__VOID,    /* c_marshaller, unused at the
               Ada level ??? This probably makes the widget unusable from C */
             return_type,                      /* return_type */
             count,                            /* n_params */
             parameters + j * max_parameters); /* param_types */
       }

       /* Do not call g_type_class_ref here, since that would prevent us
        * from adding interfaces later on. Instead, rely on the class_init
        * function
        */
       g_once_init_leave (&klass->type, new_type); // sets klass->type
       return 1;
   }
   return 0;
}

#define ADA_GTK_OVERRIDE_METHOD(TypeName, method) \
   void \
   ada_##TypeName##_override_##method ( \
         GObjectClass* klass, gpointer handler) { \
      if (handler && GTK_IS_##TypeName (klass)) { \
          GTK_##TypeName (klass)->method = handler; \
      } \
   }

ADA_GTK_OVERRIDE_METHOD(WIDGET_CLASS, size_allocate)
ADA_GTK_OVERRIDE_METHOD(WIDGET_CLASS, draw)
ADA_GTK_OVERRIDE_METHOD(WIDGET_CLASS, realize)
ADA_GTK_OVERRIDE_METHOD(WIDGET_CLASS, get_preferred_width)
ADA_GTK_OVERRIDE_METHOD(WIDGET_CLASS, get_preferred_height)
ADA_GTK_OVERRIDE_METHOD(WIDGET_CLASS, get_preferred_width_for_height)
ADA_GTK_OVERRIDE_METHOD(WIDGET_CLASS, get_preferred_height_for_width)

void ada_inherited_WIDGET_CLASS_size_allocate (
      AdaGObjectClass klass, GtkWidget* widget, GtkAllocation* rect)
{
   GObjectClass* objklass = g_type_class_ref (klass->type);
   GObjectClass* parent_class = g_type_class_peek_parent (objklass);
   GTK_WIDGET_CLASS (parent_class)->size_allocate (widget, rect);
   g_type_class_unref (objklass);
}

void ada_inherited_WIDGET_CLASS_realize (
      AdaGObjectClass klass, GtkWidget* widget)
{
   GObjectClass* objklass = g_type_class_ref (klass->type);
   GObjectClass* parent_class = g_type_class_peek_parent (objklass);
   GTK_WIDGET_CLASS (parent_class)->realize (widget);
   g_type_class_unref (objklass);
}

void ada_inherited_WIDGET_CLASS_get_preferred_width (
      AdaGObjectClass klass, GtkWidget* widget, gint* min, gint* natural)
{
   GObjectClass* objklass = g_type_class_ref (klass->type);
   GObjectClass* parent_class = g_type_class_peek_parent (objklass);
   GTK_WIDGET_CLASS (parent_class)->get_preferred_width (widget, min, natural);
   g_type_class_unref (objklass);
}

void ada_inherited_WIDGET_CLASS_get_preferred_width_for_height (
      AdaGObjectClass klass, GtkWidget* widget, gint height, gint* min, gint* natural)
{
   GObjectClass* objklass = g_type_class_ref (klass->type);
   GObjectClass* parent_class = g_type_class_peek_parent (objklass);
   GTK_WIDGET_CLASS (parent_class)->get_preferred_width_for_height
      (widget, height, min, natural);
   g_type_class_unref (objklass);
}

void ada_inherited_WIDGET_CLASS_get_preferred_height (
      AdaGObjectClass klass, GtkWidget* widget, gint* min, gint* natural)
{
   GObjectClass* objklass = g_type_class_ref (klass->type);
   GObjectClass* parent_class = g_type_class_peek_parent (objklass);
   GTK_WIDGET_CLASS (parent_class)->get_preferred_height
     (widget, min, natural);
   g_type_class_unref (objklass);
}

void ada_inherited_WIDGET_CLASS_get_preferred_height_for_width (
      AdaGObjectClass klass, GtkWidget* widget, gint width, gint* min, gint* natural)
{
   GObjectClass* objklass = g_type_class_ref (klass->type);
   GObjectClass* parent_class = g_type_class_peek_parent (objklass);
   GTK_WIDGET_CLASS (parent_class)->get_preferred_height_for_width
     (widget, width, min, natural);
   g_type_class_unref (objklass);
}

gboolean ada_inherited_WIDGET_CLASS_draw (
      AdaGObjectClass klass, GtkWidget* widget, cairo_t *cr)
{
   GObjectClass* objklass = g_type_class_ref (klass->type);
   GObjectClass* parent_class = g_type_class_peek_parent (objklass);
   g_type_class_unref (objklass);
   return GTK_WIDGET_CLASS (parent_class)->draw (widget, cr);
}

/*****************************************************
 ** Gtk.Selection and Gtk.Dnd functions
 *****************************************************/

guint ada_gtk_dnd_context_targets_count (GdkDragContext* context)
{
  return g_list_length (gdk_drag_context_list_targets (context));
}

void ada_gtk_dnd_context_get_targets (GdkDragContext* context, GdkAtom* result)
{
  GList *glist = gdk_drag_context_list_targets (context);
  GdkAtom* tmp = result;
  while (glist != NULL)
    {
      *tmp++ = (GdkAtom)glist->data;
//      gchar *name = gdk_atom_name ((GdkAtom)glist->data);
//      *tmp++ = name;
      glist = glist->next;
    }
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

/**********************************************************
 **  Support for events
 **********************************************************/

#ifdef _WIN32
#define ada_gdk_invalid_gdouble_value 1.79769313486232e308
#define ada_gdk_invalid_gint_value ((2<<31) - 1)
#define ada_gdk_invalid_guint_value (guint)((2LL<<32) - 1)
#define ada_gdk_invalid_guint32_value (guint32)((2LL<<32) - 1)
#define ada_gdk_invalid_gulong_value (gulong)((2LL<<32) - 1)

#else
extern const gdouble ada_gdk_invalid_gdouble_value;
extern const gint    ada_gdk_invalid_gint_value;
extern const guint   ada_gdk_invalid_guint_value;
extern const guint32 ada_gdk_invalid_guint32_value;
extern const gulong  ada_gdk_invalid_gulong_value;
#endif

GdkAtom
ada_make_atom (gulong num)
{
  return _GDK_MAKE_ATOM (num);
}

GdkEventType
ada_gdk_event_get_event_type (GdkEvent *event) {
  return event->type;
}

guint
ada_gdk_event_get_button (GdkEvent * event)
{
  guint button;
  if (!gdk_event_get_button(event, &button)) {
    return ada_gdk_invalid_guint_value;
  }
  return button;
}

GdkModifierType
ada_gdk_event_get_state (GdkEvent * event)
{
  GdkModifierType state;
  if (!gdk_event_get_state(event, &state)) {
    return ada_gdk_invalid_guint_value;
  }
  return state;
}

guint
ada_gdk_event_get_keyval (GdkEvent * event)
{
  guint keyval;
  if (!gdk_event_get_keyval(event, &keyval)) {
    return ada_gdk_invalid_gint_value;
  }
  return keyval;
}

GdkWindow*
ada_gdk_event_get_window (GdkEvent * event)
{
  return ((GdkEventAny*)event)->window;
}

guint16
ada_gdk_event_get_keycode (GdkEvent * event)
{
  guint16 keycode;
  if (!gdk_event_get_keycode(event, &keycode)) {
    return 0;
  }
  return keycode;
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

void*
ada_cclosure_get_callback (GCClosure* closure) {
   return closure->callback;
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

int
ada_c_gvalue_size ()
{
  return sizeof (GValue);
}

void
ada_gvalue_set (GValue* value, void *val)
{
  if G_VALUE_HOLDS_CHAR (value)
    g_value_set_schar (value, *(gchar*)val);
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

/**********************************************
 ** Functions for Box
 **********************************************/

GtkWidget*
ada_box_get_child (GtkBox* widget, gint num)
{
  GList * list;
  list = gtk_container_get_children ((GtkContainer*)widget);
  if (list && num < g_list_length (list))
    return ((GtkWidget*) (g_list_nth_data (list, num)));
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
  if (list)
     return list->data;
  else
     return NULL;
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

gpointer
ada_slist_get_data (GSList* list)
{
  return list->data;
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
				GdkWindowWindowClass wclass)
{
  g_return_if_fail (window_attr != NULL);

  window_attr->wclass = wclass;
}

GdkWindowWindowClass
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

int
ada_c_enum_value_size ()
{
  return sizeof (GEnumValue);
}

GEnumValue*
ada_genum_nth_value (GEnumClass* klass, guint nth)
{
  return (nth < klass->n_values) ? &(klass->values[nth]) : NULL;
}

gint
ada_genum_get_value (GEnumValue* value)
{
  return value->value;
}

const gchar*
ada_genum_get_name (GEnumValue* value)
{
  return value->value_name;
}

const gchar*
ada_genum_get_nick (GEnumValue* value)
{
  return value->value_nick;
}

/******************************************
 ** GFlags                               **
 ******************************************/

GFlagsValue*
ada_gflags_nth_value (GFlagsClass* klass, guint nth)
{
  return (nth < klass->n_values) ? &(klass->values[nth]) : NULL;
}

gint
ada_gflags_get_value (GFlagsValue* value)
{
  return value->value;
}

const gchar*
ada_gflags_get_name (GFlagsValue* value)
{
  return value->value_name;
}

const gchar*
ada_gflags_get_nick (GFlagsValue* value)
{
  return value->value_nick;
}

/******************************************
 ** GParamSpec                           **
 ******************************************/

const char*
ada_gparam_get_name (GParamSpec* param)
{
  return param->name;
}

GParamFlags
ada_gparam_get_flags (GParamSpec* param)
{
  return param->flags;
}

GType
ada_gparam_get_owner_type (GParamSpec* param)
{
  return param->owner_type;
}

GType
ada_gparam_get_value_type (GParamSpec* param)
{
  return G_PARAM_SPEC_VALUE_TYPE (param);
}

void
ada_gparam_set_value_type (GParamSpec* param, GType value_type)
{
  G_PARAM_SPEC_VALUE_TYPE (param) = value_type;
}

gint8
ada_gparam_get_minimum_char (GParamSpecChar* param)
{
  return param->minimum;
}

gint8
ada_gparam_get_maximum_char (GParamSpecChar* param)
{
  return param->maximum;
}

gint8
ada_gparam_get_default_char (GParamSpecChar* param)
{
  return param->default_value;
}

guint8
ada_gparam_get_minimum_uchar (GParamSpecUChar* param)
{
  return param->minimum;
}

guint8
ada_gparam_get_maximum_uchar (GParamSpecUChar* param)
{
  return param->maximum;
}

guint8
ada_gparam_get_default_uchar (GParamSpecUChar* param)
{
  return param->default_value;
}

gboolean
ada_gparam_get_default_boolean (GParamSpecBoolean* param)
{
  return param->default_value;
}

gint
ada_gparam_get_minimum_int (GParamSpecInt* param)
{
  return param->minimum;
}

gint
ada_gparam_get_maximum_int (GParamSpecInt* param)
{
  return param->maximum;
}

gint
ada_gparam_get_default_int (GParamSpecInt* param)
{
  return param->default_value;
}

guint
ada_gparam_get_minimum_uint (GParamSpecUInt* param)
{
  return param->minimum;
}

guint
ada_gparam_get_maximum_uint (GParamSpecUInt* param)
{
  return param->maximum;
}

guint
ada_gparam_get_default_uint (GParamSpecUInt* param)
{
  return param->default_value;
}

glong
ada_gparam_get_minimum_long (GParamSpecLong* param)
{
  return param->minimum;
}

glong
ada_gparam_get_maximum_long (GParamSpecLong* param)
{
  return param->maximum;
}

glong
ada_gparam_get_default_long (GParamSpecLong* param)
{
  return param->default_value;
}

gulong
ada_gparam_get_minimum_ulong (GParamSpecULong* param)
{
  return param->minimum;
}

gulong
ada_gparam_get_maximum_ulong (GParamSpecULong* param)
{
  return param->maximum;
}

gulong
ada_gparam_get_default_ulong (GParamSpecULong* param)
{
  return param->default_value;
}

gunichar
ada_gparam_get_default_unichar (GParamSpecUnichar* param)
{
  return param->default_value;
}

gint
ada_gparam_get_default_enum (GParamSpecEnum* param)
{
  return param->default_value;
}

GEnumClass*
ada_gparam_get_enum_class_enum (GParamSpecEnum* param)
{
  return param->enum_class;
}

GFlagsClass*
ada_gparam_get_flags_flags (GParamSpecFlags* param)
{
  return param->flags_class;
}

glong
ada_gparam_get_default_flags (GParamSpecFlags* param)
{
  return param->default_value;
}

gfloat
ada_gparam_get_minimum_gfloat (GParamSpecFloat* param)
{
  return param->minimum;
}

gfloat
ada_gparam_get_maximum_gfloat (GParamSpecFloat* param)
{
  return param->maximum;
}

gfloat
ada_gparam_get_default_gfloat (GParamSpecFloat* param)
{
  return param->default_value;
}

gfloat
ada_gparam_get_epsilon_gfloat (GParamSpecFloat* param)
{
  return param->epsilon;
}

gdouble
ada_gparam_get_minimum_gdouble (GParamSpecDouble* param)
{
  return param->minimum;
}

gdouble
ada_gparam_get_maximum_gdouble (GParamSpecDouble* param)
{
  return param->maximum;
}

gdouble
ada_gparam_get_default_gdouble (GParamSpecDouble* param)
{
  return param->default_value;
}

gdouble
ada_gparam_get_epsilon_gdouble (GParamSpecDouble* param)
{
  return param->epsilon;
}

gchar*
ada_gparam_default_string (GParamSpecString* param)
{
  return param->default_value;
}

gchar*
ada_gparam_cset_first_string (GParamSpecString* param)
{
  return param->cset_first;
}

gchar*
ada_gparam_cset_nth_string (GParamSpecString* param)
{
  return param->cset_nth;
}

gchar
ada_gparam_substitutor_string (GParamSpecString* param)
{
  return param->substitutor;
}

gboolean
ada_gparam_ensure_non_null_string (GParamSpecString* param)
{
  return param->ensure_non_null != 0;
}

/******************************************
 ** New widgets
 ******************************************/

void
ada_install_property_handlers
   (GObjectClass* klass,
    GObjectSetPropertyFunc c_set_handler,
    GObjectGetPropertyFunc c_get_handler,
    GObjectSetPropertyFunc ada_set_handler,
    GObjectGetPropertyFunc ada_get_handler)
{
  G_OBJECT_CLASS (klass)->set_property = c_set_handler;
  G_OBJECT_CLASS (klass)->get_property = c_get_handler;

  AdaGObjectClass adaklass = ADA_CLASS_FROM_C_CLASS(klass);
  adaklass->ada_property_getter = ada_get_handler;
  adaklass->ada_property_setter = ada_set_handler;
}

GObjectGetPropertyFunc ada_real_get_property_handler (GObject* object) {
   return ada_gobject_class_from_object(object)->ada_property_getter;
}

GObjectSetPropertyFunc ada_real_set_property_handler (GObject* object) {
   return ada_gobject_class_from_object(object)->ada_property_setter;
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

GType
ada_gtype_fundamental (GType type)
{
  return G_TYPE_FUNDAMENTAL (type);
}

gboolean
ada_g_type_is_interface (GType type)
{
  return G_TYPE_IS_INTERFACE (type);
}

/******************************************
 ** Handling of tree Freeze/Thaw         **
 ******************************************/

gint
ada_gtk_tree_view_freeze_sort (GtkTreeStore* tree)
{
  gint save;
  GtkSortType order;
  gtk_tree_sortable_get_sort_column_id
    (GTK_TREE_SORTABLE (tree), &save, &order);
  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (tree), -2, order);
  return save;
}

void
ada_gtk_tree_view_thaw_sort (GtkTreeStore* tree, gint id)
{
  gint save;
  GtkSortType order;
  gtk_tree_sortable_get_sort_column_id
    (GTK_TREE_SORTABLE (tree), &save, &order);
  gtk_tree_sortable_set_sort_column_id
    (GTK_TREE_SORTABLE (tree), id, order);
}

/*****************************************************
 ** Glib
*****************************************************/

struct CustomGSource
{
  GSource source;
  gpointer user_data;
};

GSourceFuncs*
ada_allocate_g_source_funcs
  (gpointer prepare, gpointer check, gpointer dispatch, gpointer finalize)
{
  GSourceFuncs* result;
  result = (GSourceFuncs*) malloc (sizeof (GSourceFuncs));

  result->prepare  = prepare;
  result->check    = check;
  result->dispatch = dispatch;
  result->finalize = finalize;
  return result;
}

GSource*
ada_g_source_new (GSourceFuncs* type, gpointer user_data)
{
  struct CustomGSource* result =
    (struct CustomGSource*)g_source_new (type, sizeof (struct CustomGSource));
  result->user_data = user_data;
  return (GSource*)result;
}

gpointer
ada_g_source_get_user_data (GSource* source)
{
  return ((struct CustomGSource*)source)->user_data;
}

/***********************************************************
 ** Gtk_Text_Buffer
***********************************************************/

void
ada_gtk_text_buffer_insert_with_tags
 (GtkTextBuffer *buffer,
  GtkTextIter   *iter,
  const gchar   *text,
  gint           len,
  GtkTextTag    *tag)
{
  gtk_text_buffer_insert_with_tags
    (buffer, iter, text, len, tag, NULL);
}

GtkTextTag*
ada_gtk_text_buffer_create_tag (GtkTextBuffer* buffer, const gchar* name)
{
   return gtk_text_buffer_create_tag (buffer, name, NULL);
}

/***********************************************************
 ** Gtk_File_Chooser_Dialog
***********************************************************/

GtkWidget *
ada_gtk_file_chooser_dialog_new
  (const gchar          *title,
   GtkWindow            *parent,
   GtkFileChooserAction  action)
{
  return gtk_file_chooser_dialog_new
    (title, parent, action, NULL, (char *)NULL);
}

/***********************************************************
 ** Gtk_Recent_Chooser_Dialog
***********************************************************/

GtkWidget*
ada_gtk_recent_chooser_dialog_new
  (const gchar *title,
   GtkWindow   *parent)
{
  return gtk_recent_chooser_dialog_new (title, parent, NULL, NULL);
}

GtkWidget*
ada_gtk_recent_chooser_dialog_new_for_manager
  (const gchar      *title,
   GtkWindow        *parent,
   GtkRecentManager *manager)
{
  return gtk_recent_chooser_dialog_new_for_manager
    (title, parent, manager, NULL, NULL);
}

/**************************************************************
 **  Gtk_Bindings
**************************************************************/

void
ada_gtk_binding_entry_add_signal_NO
  (GtkBindingSet* set, guint keyval, GdkModifierType modifier,
   const gchar* signal_name)
{
  gtk_binding_entry_add_signal (set, keyval, modifier, signal_name, 0);
}

void
ada_gtk_binding_entry_add_signal_int
  (GtkBindingSet* set, guint keyval, GdkModifierType modifier,
   const gchar* signal_name, gint arg1)
{
  gtk_binding_entry_add_signal
    (set, keyval, modifier, signal_name, 1,
     G_TYPE_INT, arg1);
}

void
ada_gtk_binding_entry_add_signal_int_int
  (GtkBindingSet* set, guint keyval, GdkModifierType modifier,
   const gchar* signal_name, gint arg1, gint arg2)
{
  gtk_binding_entry_add_signal
    (set, keyval, modifier, signal_name, 2,
     G_TYPE_INT, arg1, G_TYPE_INT, arg2);
}

void
ada_gtk_binding_entry_add_signal_bool
  (GtkBindingSet* set, guint keyval, GdkModifierType modifier,
   const gchar* signal_name, gboolean arg1)
{
  gtk_binding_entry_add_signal
    (set, keyval, modifier, signal_name, 1,
     G_TYPE_BOOLEAN, arg1);
}

GdkModifierType
ada_gdk_get_default_modifier ()
{
  static GdkModifierType primary = 0;

  if (!primary)
    {
      GdkDisplay      *display = gdk_display_get_default ();
      GdkKeymap       *keymap = gdk_keymap_get_for_display (display);
      GdkModifierType real;

      g_return_val_if_fail (GDK_IS_KEYMAP (keymap), 0);

      /* Retrieve the real modifier mask */
      real = gdk_keymap_get_modifier_mask
        (keymap,
         GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR);

      primary = real;

      /* We need to translate the real modifiers into a virtual modifier
         (like Super, Meta, etc.).
         The following call adds the virtual modifiers for each real modifier
         defined in primary.
      */
      gdk_keymap_add_virtual_modifiers (keymap, &primary);

      if (primary != real) {
        /* In case the virtual and real modifiers are different, we need to
           remove the real modifier from the result, and keep only the
           virtual one.
        */
        primary &= ~real;
      }
    }

  return primary;
}

// GtkPlug is only build on X11 backends

#ifndef GDK_WINDOWING_X11
int gtk_plug_get_type() {
   return 0;
}
int gtk_socket_get_type() {
   return 0;
}
#endif

// Application handling for opening files from the explorer/finder

typedef struct {
  GtkApplication *app;    //  The app responsible for opening the file
  GFile          **files; //  The array of files to open
  gint           n_files; //  The size of the above array
} ada_gtk_open_data;

/*
 * Idle callback responsible for actually calling g_application_open with the
 * proper files to open.
 */
gboolean
ada_gtk_application_open_files (gpointer ptr)
{
  int i;
  ada_gtk_open_data *data = (ada_gtk_open_data*)ptr;

  g_application_open (G_APPLICATION (data->app),
		      data->files, data->n_files, NULL);

  for (i = 0; i < data->n_files; i++)
    g_object_unref (data->files[i]);
  g_free (data->files);
  g_free (data);

  return G_SOURCE_REMOVE;
}

#ifdef GDK_WINDOWING_QUARTZ
void ada_gtk_quartz_application_open_files
  (GFile** files, gint n_files, gpointer user_data)
{
  ada_gtk_open_data *data = malloc (sizeof (ada_gtk_open_data));

  data->app     = (GtkApplication*)user_data;
  data->files   = files;
  data->n_files = n_files;

  ada_gtk_application_open_files (data);
}
#endif

#ifdef GDK_WINDOWING_WIN32

/* The DDE API do not support user data in the DDE callback. We thus need to
   use the below global data to support the feature */
GtkApplication *ada_gtk_win32_app;
DWORD ada_gtk_win32_id_instance;

HDDEDATA CALLBACK ada_gtk_open_document_win32
  (UINT uType,
   UINT uFmt,
   HCONV hconv,
   HSZ hsz1,
   HSZ hsz2,
   HDDEDATA hData,
   ULONG_PTR dwData1,
   ULONG_PTR dwData2)
{
  switch (uType) {
    case XTYP_ADVDATA:
      return (HDDEDATA) DDE_FACK;

    case XTYP_CONNECT:
      return (HDDEDATA) TRUE;

    case XTYP_DISCONNECT:
      return (HDDEDATA) NULL;

    case XTYP_REGISTER:
    case XTYP_UNREGISTER:
      return (HDDEDATA) NULL;

    case XTYP_EXECUTE:
      {
        DWORD dataLen;
        PWSTR strData = (PWSTR) DdeAccessData (hData, &dataLen);
        DWORD j;
        PWSTR path = NULL;
        LPCWSTR openCmd = L"FileOpen:";
        DWORD utfSize;
        PSTR utfPath;

        for (j = 0; j < dataLen; j++) {
          if (strData[j] == ':') {
            path = &strData[j + 1];
            break;
          } else if (strData[j] != openCmd[j]) {
            return (HDDEDATA) DDE_FNOTPROCESSED;
          }
        }

        utfSize = WideCharToMultiByte
          (CP_UTF8,
           0,
           path,
           dataLen - 9,
           NULL,
           0, /* indicates that we want to get the size of the result */
           NULL, NULL);

        utfPath = (char*)malloc (utfSize + 1);

        WideCharToMultiByte
          (CP_UTF8,
           0,
           path,
           dataLen - 9,
           utfPath,
           utfSize,
           NULL, NULL);
        utfPath[utfSize] = '\0';

        ada_gtk_open_data *data = malloc (sizeof(ada_gtk_open_data));
        data->app     = ada_gtk_win32_app;
        data->files   = (GFile **) malloc (sizeof(GFile*));
        data->n_files = 1;
        data->files[0] = g_file_new_for_path ((const char *)utfPath);
        free (utfPath);

        g_idle_add (ada_gtk_application_open_files, data);

        DdeUnaccessData (hData); /* Release the resource */
      }

      return (HDDEDATA) DDE_FACK;
  };

  return (HDDEDATA) DDE_FNOTPROCESSED;
}

#endif /* WIN32 */

static void
ada_gtk_application_startup (GtkApplication *application) {
#if defined (GDK_WINDOWING_QUARTZ)

  init_osx_open_files_event_handler
    (ada_gtk_quartz_application_open_files, (gpointer)application);

#elif defined (GDK_WINDOWING_WIN32)

  /* On Windows, we use a DDE server to handle events from the Shell */

  HSZ hszAppName;
  gchar* appName =
    g_application_get_application_id (G_APPLICATION (application));
  gchar* shortAppName = appName;

  /* DDE server only supports a short application name. We extract the last
     part of the full application name for that. So that for example:
     com.adacore.GPS will use GPS as DDE server name */
  for (; *appName != '\0'; appName++) {
    if (*appName == '.') shortAppName = appName + 1;
  }

  ada_gtk_win32_app = application;

  /* Initialize the DDE framework */
  DdeInitializeW (&ada_gtk_win32_id_instance,
                  (PFNCALLBACK) &ada_gtk_open_document_win32,
                  APPCLASS_STANDARD |
                  CBF_FAIL_ADVISES | CBF_FAIL_POKES | CBF_FAIL_SELFCONNECTIONS,
                  0);

  /* And register the DDE Service to the name server */
  hszAppName = DdeCreateStringHandleA
    (ada_gtk_win32_id_instance, shortAppName, CP_WINANSI);
  DdeNameService
    (ada_gtk_win32_id_instance,   /* instance identifier */
     hszAppName, /*  handle to service name string */
     0, /* reserved */
     DNS_REGISTER | DNS_FILTERON);
#endif
}

static void
ada_gtk_application_shutdown (GtkApplication *application) {
#ifdef GDK_WINDOWING_WIN32
  DdeUninitialize (ada_gtk_win32_id_instance);
#endif
}

void
ada_gtk_app_activate (void){
#ifdef GDK_WINDOWING_QUARTZ
  ada_activate_app();
#endif
}

#ifdef GDK_WINDOWING_QUARTZ
/* Enable the full-screen button on OSX for the main window */
static void
ada_gtk_quartz_window_added
  (GtkApplication *application,
   GtkWindow      *window,
   gpointer       user_data)
{
  ada_gtk_osx_allow_fullscreen (window);
}
#endif /* QUARTZ */

typedef enum
  {
    GTKADA_APPLICATION_FLAGS_NONE,
    GTKADA_APPLICATION_HANDLES_OPEN =   (1 << 0),
    GTKADA_APPLICATION_OSX_FULLSCREEN = (1 << 1)
  } GtkadaApplicationFlags;

/* Called by the GtkAda.Application instance during initialisation */
void
ada_gtk_setup_application(GtkApplication *app, GtkadaApplicationFlags flags)
{
  if ((flags & GTKADA_APPLICATION_HANDLES_OPEN) != 0)
    {
      g_signal_connect
	(app, "startup", G_CALLBACK (ada_gtk_application_startup), NULL);
      g_signal_connect
	(app, "shutdown", G_CALLBACK (ada_gtk_application_shutdown), NULL);
    }
#ifdef GDK_WINDOWING_QUARTZ
  if ((flags & GTKADA_APPLICATION_OSX_FULLSCREEN) != 0)
    {
      g_signal_connect
	(app, "window-added", G_CALLBACK (ada_gtk_quartz_window_added), NULL);
    }
#endif
}

/* Wrappers for gspawn */
gboolean gnat_spawn_async (const gchar           *working_directory,
                           gchar                **argv,
                           gchar                **envp,
                           GSpawnFlags            flags,
                           GSpawnChildSetupFunc   child_setup,
                           gpointer               user_data,
                           GPid                  *child_pid,
                           GError               **error) {
  return g_spawn_async
    (working_directory, argv, envp, flags, child_setup, user_data,
     child_pid, error);
 }

gboolean gnat_spawn_async_with_pipes (const gchar          *working_directory,
                                      gchar               **argv,
                                      gchar               **envp,
                                      GSpawnFlags           flags,
                                      GSpawnChildSetupFunc  child_setup,
                                      gpointer              user_data,
                                      GPid                 *child_pid,
                                      gint                 *standard_input,
                                      gint                 *standard_output,
                                      gint                 *standard_error,
                                      GError              **error) {
  return g_spawn_async_with_pipes
    (working_directory, argv, envp, flags, child_setup, user_data, child_pid,
     standard_input, standard_output, standard_error, error);
}

gboolean gnat_spawn_async_with_fds (const gchar          *working_directory,
                                    gchar               **argv,
                                    gchar               **envp,
                                    GSpawnFlags           flags,
                                    GSpawnChildSetupFunc  child_setup,
                                    gpointer              user_data,
                                    GPid                 *child_pid,
                                    gint                  stdin_fd,
                                    gint                  stdout_fd,
                                    gint                  stderr_fd,
                                    GError              **error) {
  return g_spawn_async_with_fds
    (working_directory, argv, envp, flags, child_setup, user_data,
     child_pid, stdin_fd, stdout_fd, stderr_fd, error);
}

gboolean gnat_spawn_sync      (const gchar          *working_directory,
                               gchar               **argv,
                               gchar               **envp,
                               GSpawnFlags           flags,
                               GSpawnChildSetupFunc  child_setup,
                               gpointer              user_data,
                               gchar               **standard_output,
                               gchar               **standard_error,
                               gint                 *exit_status,
                               GError              **error) {
  return g_spawn_sync
    (working_directory, argv, envp, flags, child_setup, user_data,
     standard_output, standard_error, exit_status, error);
}

gboolean gnat_spawn_command_line_sync (const gchar          *command_line,
                                       gchar               **standard_output,
                                       gchar               **standard_error,
                                       gint                 *exit_status,
                                       GError              **error) {
  return g_spawn_command_line_sync
    (command_line, standard_output, standard_error, exit_status, error);
}

gboolean gnat_spawn_command_line_async (const gchar          *command_line,
                                        GError              **error) {
  return g_spawn_command_line_async (command_line, error);
}

/* gutils.h */
const gchar * glib_get_home_dir () {
  return g_get_home_dir ();
}

/* constants */
const GVariantType* ada_gvariant_type_boolean = G_VARIANT_TYPE_BOOLEAN;
const GVariantType* ada_gvariant_type_byte    = G_VARIANT_TYPE_BYTE;
const GVariantType* ada_gvariant_type_int16   = G_VARIANT_TYPE_INT16;
const GVariantType* ada_gvariant_type_uint16  = G_VARIANT_TYPE_UINT16;
const GVariantType* ada_gvariant_type_int32   = G_VARIANT_TYPE_INT32;
const GVariantType* ada_gvariant_type_uint32  = G_VARIANT_TYPE_UINT32;
const GVariantType* ada_gvariant_type_int64   = G_VARIANT_TYPE_INT64;
const GVariantType* ada_gvariant_type_uint64  = G_VARIANT_TYPE_UINT64;
const GVariantType* ada_gvariant_type_double  = G_VARIANT_TYPE_DOUBLE;
const GVariantType* ada_gvariant_type_string  = G_VARIANT_TYPE_STRING;
