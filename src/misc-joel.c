#include <glib.h>
#include <gtk/gtkradiomenuitem.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkwidget.h>
 
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

void 
ada_object_unset_flags (GtkObject * object, guint32 flags)
{
  GTK_OBJECT_UNSET_FLAGS (object, flags);
}

guint32
ada_object_destroyed (GtkObject * object)
{
  return GTK_OBJECT_DESTROYED (object);
}

guint32
ada_object_floating (GtkObject * object)
{
  return GTK_OBJECT_FLOATING (object);
}

guint32
ada_object_connected (GtkObject * object)
{
  return GTK_OBJECT_CONNECTED (object);
}


/*
 *
 * Widget macros
 *
 */

guint32 
ada_widget_toplevel (GtkWidget * widget)
{
  return GTK_WIDGET_TOPLEVEL (widget);
}

guint32
ada_widget_no_window (GtkWidget * widget)
{
  return GTK_WIDGET_NO_WINDOW (widget);
}

guint32
ada_widget_realized (GtkWidget * widget)
{
  return GTK_WIDGET_REALIZED (widget);
}

guint32
ada_widget_mapped (GtkWidget * widget)
{
  return GTK_WIDGET_MAPPED (widget);
}

guint32
ada_widget_visible (GtkWidget * widget)
{
  return GTK_WIDGET_VISIBLE (widget);
}

guint32
ada_widget_drawable (GtkWidget * widget)
{
  return GTK_WIDGET_DRAWABLE (widget);
}

guint32 
ada_widget_sensitive (GtkWidget * widget)
{
  return GTK_WIDGET_SENSITIVE (widget);
}

guint32
ada_widget_parent_sensitive (GtkWidget * widget)
{
  return GTK_WIDGET_PARENT_SENSITIVE (widget);
}

guint32
ada_widget_is_sensitive (GtkWidget * widget)
{
  return GTK_WIDGET_IS_SENSITIVE (widget);
}

guint32
ada_widget_can_focus (GtkWidget * widget)
{
  return GTK_WIDGET_CAN_FOCUS (widget);
}

guint32
ada_widget_has_focus (GtkWidget * widget)
{
  return GTK_WIDGET_HAS_FOCUS (widget);
}

guint32
ada_widget_has_default (GtkWidget * widget)
{
  return GTK_WIDGET_HAS_DEFAULT (widget);
}

guint32
ada_widget_has_grab (GtkWidget * widget)
{
  return GTK_WIDGET_HAS_GRAB (widget);
}

guint32
ada_widget_basic (GtkWidget * widget)
{
  return GTK_WIDGET_BASIC (widget);
}

guint32
ada_widget_rc_style (GtkWidget * widget)
{
  return GTK_WIDGET_RC_STYLE (widget);
}

    
/*
 * 
 * toggle_buttons
 *
 */

gint 
ada_toggle_button_get_state (GtkToggleButton *toggle_button)
{
  return toggle_button->active;
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

/*
 *
 * GdkRectangle
 *
 */

GdkRectangle *
ada_gdk_rectangle_new_with_data (gint16 x, gint16 y, 
				guint16 width, guint16 height)
{
  GdkRectangle * result;

  result = g_new (GdkRectangle, 1);
  if (result)
    {
      result->x = x;
      result->y = y;
      result->width = width;
      result->height = height;
    }
  
  return result;
}

gint16
ada_gdk_rectangle_get_x (GdkRectangle * rectangle)
{
  g_return_val_if_fail (rectangle != NULL, 0);

  return rectangle->x;
}

gint16
ada_gdk_rectangle_get_y (GdkRectangle * rectangle) 
{
  g_return_val_if_fail (rectangle != NULL, 0);

  return rectangle->y;
}

guint16
ada_gdk_rectangle_get_width (GdkRectangle * rectangle)
{
  g_return_val_if_fail (rectangle != NULL, 0);
  
  return rectangle->width;
}

guint16
ada_gdk_rectangle_get_height (GdkRectangle * rectangle)
{
  g_return_val_if_fail (rectangle != NULL, 0);
  
  return rectangle->height;
}

void 
ada_gdk_rectangle_set_x (GdkRectangle * rectangle,
			 gint16 x)
{
  g_return_if_fail (rectangle != NULL);
  
  rectangle->x = x;
}


void 
ada_gdk_rectangle_set_y (GdkRectangle * rectangle,
			 gint16 y)
{
  g_return_if_fail (rectangle != NULL);
  
  rectangle->y = y;
}


void
ada_gdk_rectangle_set_width (GdkRectangle * rectangle,
			     guint16 width)
{
  g_return_if_fail (rectangle != NULL);
  
  rectangle->width = width;
}


void
ada_gdk_rectangle_set_height (GdkRectangle * rectangle,
			      guint16 height)
{
  g_return_if_fail (rectangle != NULL);
  
  rectangle->height = height;
}

void 
ada_gdk_rectangle_destroy (GdkRectangle * rectangle)
{
  g_free (rectangle);
}

/*
 *
 * GdkPoint
 *
 */

GdkPoint * 
ada_gdk_point_new_with_coordinates (const gint16 x, const gint16 y)
{
  GdkPoint * result = NULL;
  
  result = g_new (GdkPoint, 1);
  if (result)
    {
      result->x = x;
      result->y = y;
      
    }

  return result;
}

void
ada_gdk_point_set_coordinates (GdkPoint * point, 
			       const gint16 x, 
			       const gint16 y)
{
  g_return_if_fail (point != NULL);
  
  point->x = x;
  point->y = y;
}

gint16
ada_gdk_point_get_x (GdkPoint * point)
{
  g_return_val_if_fail (point != NULL, 0);
  
  return point->x;
}

gint16
ada_gdk_point_get_y (GdkPoint * point)
{
  g_return_val_if_fail (point != NULL, 0);
  
  return point->y;
}

void
ada_gdk_point_destroy (GdkPoint * point)
{
  g_free (point);
}

