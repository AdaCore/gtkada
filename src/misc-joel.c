#include <glib.h>
#include <gdk/gdktypes.h>
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


/*
 *
 *  GdkCursor
 *
 */

typedef enum
{
  ADA_GDK_NUM_GLYPHS,
  ADA_GDK_X_CURSOR,
  ADA_GDK_ARROW,
  ADA_GDK_BASED_ARROW_DOWN,
  ADA_GDK_BASED_ARROW_UP,
  ADA_GDK_BOAT,
  ADA_GDK_BOGOSITY,
  ADA_GDK_BOTTOM_LEFT_CORNER,
  ADA_GDK_BOTTOM_RIGHT_CORNER,
  ADA_GDK_BOTTOM_SIDE,
  ADA_GDK_BOTTOM_TEE,
  ADA_GDK_BOX_SPIRAL,
  ADA_GDK_CENTER_PTR,
  ADA_GDK_CIRCLE,
  ADA_GDK_CLOCK,
  ADA_GDK_COFFEE_MUG,
  ADA_GDK_CROSS,
  ADA_GDK_CROSS_REVERSE,
  ADA_GDK_CROSSHAIR,
  ADA_GDK_DIAMOND_CROSS,
  ADA_GDK_DOT,
  ADA_GDK_DOTBOX,
  ADA_GDK_DOUBLE_ARROW,
  ADA_GDK_DRAFT_LARGE,
  ADA_GDK_DRAFT_SMALL,
  ADA_GDK_DRAPED_BOX,
  ADA_GDK_EXCHANGE,
  ADA_GDK_FLEUR,
  ADA_GDK_GOBBLER,
  ADA_GDK_GUMBY,
  ADA_GDK_HAND1,
  ADA_GDK_HAND2,
  ADA_GDK_HEART,
  ADA_GDK_ICON,
  ADA_GDK_IRON_CROSS,
  ADA_GDK_LEFT_PTR,
  ADA_GDK_LEFT_SIDE,
  ADA_GDK_LEFT_TEE,
  ADA_GDK_LEFTBUTTON,
  ADA_GDK_LL_ANGLE,
  ADA_GDK_LR_ANGLE,
  ADA_GDK_MAN,
  ADA_GDK_MIDDLEBUTTON,
  ADA_GDK_MOUSE,
  ADA_GDK_PENCIL,
  ADA_GDK_PIRATE,
  ADA_GDK_PLUS,
  ADA_GDK_QUESTION_ARROW,
  ADA_GDK_RIGHT_PTR,
  ADA_GDK_RIGHT_SIDE,
  ADA_GDK_RIGHT_TEE,
  ADA_GDK_RIGHTBUTTON,
  ADA_GDK_RTL_LOGO,
  ADA_GDK_SAILBOAT,
  ADA_GDK_SB_DOWN_ARROW,
  ADA_GDK_SB_H_DOUBLE_ARROW,
  ADA_GDK_SB_LEFT_ARROW,
  ADA_GDK_SB_RIGHT_ARROW,
  ADA_GDK_SB_UP_ARROW,
  ADA_GDK_SB_V_DOUBLE_ARROW,
  ADA_GDK_SHUTTLE,
  ADA_GDK_SIZING,
  ADA_GDK_SPIDER,
  ADA_GDK_SPRAYCAN,
  ADA_GDK_STAR,
  ADA_GDK_TARGET,
  ADA_GDK_TCROSS,
  ADA_GDK_TOP_LEFT_ARROW,
  ADA_GDK_TOP_LEFT_CORNER,
  ADA_GDK_TOP_RIGHT_CORNER,
  ADA_GDK_TOP_SIDE,
  ADA_GDK_TOP_TEE,
  ADA_GDK_TREK,
  ADA_GDK_UL_ANGLE,
  ADA_GDK_UMBRELLA,
  ADA_GDK_UR_ANGLE,
  ADA_GDK_WATCH,
  ADA_GDK_XTERM,
  ADA_GDK_LAST_CURSOR,
  ADA_GDK_CURSOR_IS_PIXMAP
} AdaGdkCursorType;



GdkCursorType
ada_gdk_cursor_of (AdaGdkCursorType cursor_type)
{
  switch (cursor_type)
  {
  case ADA_GDK_NUM_GLYPHS : return GDK_NUM_GLYPHS; break;
  case ADA_GDK_X_CURSOR : return GDK_X_CURSOR; break;
  case ADA_GDK_ARROW : return GDK_ARROW; break;
  case ADA_GDK_BASED_ARROW_DOWN : return GDK_BASED_ARROW_DOWN; break;
  case ADA_GDK_BASED_ARROW_UP : return GDK_BASED_ARROW_UP; break;
  case ADA_GDK_BOAT : return GDK_BOAT; break;
  case ADA_GDK_BOGOSITY : return GDK_BOGOSITY; break;
  case ADA_GDK_BOTTOM_LEFT_CORNER : return GDK_BOTTOM_LEFT_CORNER; break;
  case ADA_GDK_BOTTOM_RIGHT_CORNER : return GDK_BOTTOM_RIGHT_CORNER; break;
  case ADA_GDK_BOTTOM_SIDE : return GDK_BOTTOM_SIDE; break;
  case ADA_GDK_BOTTOM_TEE : return GDK_BOTTOM_TEE; break;
  case ADA_GDK_BOX_SPIRAL : return GDK_BOX_SPIRAL; break;
  case ADA_GDK_CENTER_PTR : return GDK_CENTER_PTR; break;
  case ADA_GDK_CIRCLE : return GDK_CIRCLE; break;
  case ADA_GDK_CLOCK : return GDK_CLOCK; break;
  case ADA_GDK_COFFEE_MUG : return GDK_COFFEE_MUG; break;
  case ADA_GDK_CROSS : return GDK_CROSS; break;
  case ADA_GDK_CROSS_REVERSE : return GDK_CROSS_REVERSE; break;
  case ADA_GDK_CROSSHAIR : return GDK_CROSSHAIR; break;
  case ADA_GDK_DIAMOND_CROSS : return GDK_DIAMOND_CROSS; break;
  case ADA_GDK_DOT : return GDK_DOT; break;
  case ADA_GDK_DOTBOX : return GDK_DOTBOX; break;
  case ADA_GDK_DOUBLE_ARROW : return GDK_DOUBLE_ARROW; break;
  case ADA_GDK_DRAFT_LARGE : return GDK_DRAFT_LARGE; break;
  case ADA_GDK_DRAFT_SMALL : return GDK_DRAFT_SMALL; break;
  case ADA_GDK_DRAPED_BOX : return GDK_DRAPED_BOX; break;
  case ADA_GDK_EXCHANGE : return GDK_EXCHANGE; break;
  case ADA_GDK_FLEUR : return GDK_FLEUR; break;
  case ADA_GDK_GOBBLER : return GDK_GOBBLER; break;
  case ADA_GDK_GUMBY : return GDK_GUMBY; break;
  case ADA_GDK_HAND1 : return GDK_HAND1; break;
  case ADA_GDK_HAND2 : return GDK_HAND2; break;
  case ADA_GDK_HEART : return GDK_HEART; break;
  case ADA_GDK_ICON : return GDK_ICON; break;
  case ADA_GDK_IRON_CROSS : return GDK_IRON_CROSS; break;
  case ADA_GDK_LEFT_PTR : return GDK_LEFT_PTR; break;
  case ADA_GDK_LEFT_SIDE : return GDK_LEFT_SIDE; break;
  case ADA_GDK_LEFT_TEE : return GDK_LEFT_TEE; break;
  case ADA_GDK_LEFTBUTTON : return GDK_LEFTBUTTON; break;
  case ADA_GDK_LL_ANGLE : return GDK_LL_ANGLE; break;
  case ADA_GDK_LR_ANGLE : return GDK_LR_ANGLE; break;
  case ADA_GDK_MAN : return GDK_MAN; break;
  case ADA_GDK_MIDDLEBUTTON : return GDK_MIDDLEBUTTON; break;
  case ADA_GDK_MOUSE : return GDK_MOUSE; break;
  case ADA_GDK_PENCIL : return GDK_PENCIL; break;
  case ADA_GDK_PIRATE : return GDK_PIRATE; break;
  case ADA_GDK_PLUS : return GDK_PLUS; break;
  case ADA_GDK_QUESTION_ARROW : return GDK_QUESTION_ARROW; break;
  case ADA_GDK_RIGHT_PTR : return GDK_RIGHT_PTR; break;
  case ADA_GDK_RIGHT_SIDE : return GDK_RIGHT_SIDE; break;
  case ADA_GDK_RIGHT_TEE : return GDK_RIGHT_TEE; break;
  case ADA_GDK_RIGHTBUTTON : return GDK_RIGHTBUTTON; break;
  case ADA_GDK_RTL_LOGO : return GDK_RTL_LOGO; break;
  case ADA_GDK_SAILBOAT : return GDK_SAILBOAT; break;
  case ADA_GDK_SB_DOWN_ARROW : return GDK_SB_DOWN_ARROW; break;
  case ADA_GDK_SB_H_DOUBLE_ARROW : return GDK_SB_H_DOUBLE_ARROW; break;
  case ADA_GDK_SB_LEFT_ARROW : return GDK_SB_LEFT_ARROW; break;
  case ADA_GDK_SB_RIGHT_ARROW : return GDK_SB_RIGHT_ARROW; break;
  case ADA_GDK_SB_UP_ARROW : return GDK_SB_UP_ARROW; break;
  case ADA_GDK_SB_V_DOUBLE_ARROW : return GDK_SB_V_DOUBLE_ARROW; break;
  case ADA_GDK_SHUTTLE : return GDK_SHUTTLE; break;
  case ADA_GDK_SIZING : return GDK_SIZING; break;
  case ADA_GDK_SPIDER : return GDK_SPIDER; break;
  case ADA_GDK_SPRAYCAN : return GDK_SPRAYCAN; break;
  case ADA_GDK_STAR : return GDK_STAR; break;
  case ADA_GDK_TARGET : return GDK_TARGET; break;
  case ADA_GDK_TCROSS : return GDK_TCROSS; break;
  case ADA_GDK_TOP_LEFT_ARROW : return GDK_TOP_LEFT_ARROW; break;
  case ADA_GDK_TOP_LEFT_CORNER : return GDK_TOP_LEFT_CORNER; break;
  case ADA_GDK_TOP_RIGHT_CORNER : return GDK_TOP_RIGHT_CORNER; break;
  case ADA_GDK_TOP_SIDE : return GDK_TOP_SIDE; break;
  case ADA_GDK_TOP_TEE : return GDK_TOP_TEE; break;
  case ADA_GDK_TREK : return GDK_TREK; break;
  case ADA_GDK_UL_ANGLE : return GDK_UL_ANGLE; break;
  case ADA_GDK_UMBRELLA : return GDK_UMBRELLA; break;
  case ADA_GDK_UR_ANGLE : return GDK_UR_ANGLE; break;
  case ADA_GDK_WATCH : return GDK_WATCH; break;
  case ADA_GDK_XTERM : return GDK_XTERM; break;
  case ADA_GDK_LAST_CURSOR : return GDK_LAST_CURSOR; break;
  case ADA_GDK_CURSOR_IS_PIXMAP : return GDK_CURSOR_IS_PIXMAP; break;
  default : return GDK_XTERM; break;
  }
};

GdkCursor* 
ada_gdk_cursor_new (AdaGdkCursorType   cursor_type)
{
  return gdk_cursor_new (ada_gdk_cursor_of (cursor_type));
}


/*
 *
 * GdkEventAny
 *
 */

GdkEventType
ada_gdk_event_any_get_event_type (GdkEventAny * event)
{
  return event->type;
}


void
ada_gdk_event_any_set_event_type (GdkEventAny * event, GdkEventType type)
{
  event->type = type;
}


GdkWindow *
ada_gdk_event_any_get_window (GdkEventAny * event)
{
  return event->window;
}

void
ada_gdk_event_any_set_window (GdkEventAny * event,
			      GdkWindow * window)
{
  event->window = window;
}


gint8
ada_gdk_event_any_get_send_event (GdkEventAny * event)
{
  return event->send_event;
}

void
ada_gdk_event_any_set_send_event (GdkEventAny * event, gint8 send_event)
{
  event->send_event = send_event;
}

/*
 *
 *  GdkEventExpose
 *
 */

GdkRectangle *
ada_gdk_event_expose_get_area (GdkEventExpose * event)
{
  return &event->area;
}

void
ada_gdk_event_expose_set_area (GdkEventExpose * event,
			       GdkRectangle * area)
{
  event->area = *area;
}

gint
ada_gdk_event_expose_get_count (GdkEventExpose * event)
{
  return event->count;
}

void
ada_gdk_event_expose_set_count (GdkEventExpose * event, gint count)
{
  event->count = count;
}


/*
 *
 * GdkEventConfigure
 *
 */

gint16
ada_gdk_event_configure_get_x (GdkEventConfigure * event)
{
  return event->x;
}

void
ada_gdk_event_configure_set_x (GdkEventConfigure * event, gint16 x)
{
  event->x = x;
}

gint16
ada_gdk_event_configure_get_y (GdkEventConfigure * event)
{
  return event->y;
}

void
ada_gdk_event_configure_set_y (GdkEventConfigure * event, gint16 y)
{
  event->y = y;
}

gint16
ada_gdk_event_configure_get_width (GdkEventConfigure * event)
{
  return event->width;
}

void
ada_gdk_event_configure_set_width (GdkEventConfigure * event, gint16 width)
{
  event->width = width;
}

gint16
ada_gdk_event_configure_get_height (GdkEventConfigure * event)
{
  return event->height;
}

void
ada_gdk_event_configure_set_height (GdkEventConfigure * event, gint16 height)
{
  event->height = height;
}

