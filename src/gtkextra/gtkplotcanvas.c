/* gtkplotcanvas - gtkplot canvas widget for gtk+
 * Copyright 1999-2001  Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>
#include "gtkplot.h"
#include "gtkplotcanvas.h"

#define DEFAULT_WIDTH 100
#define DEFAULT_HEIGHT 150
#define DEFAULT_MARKER_SIZE 6
#define DEFAULT_FONT_HEIGHT 12
#define ARROW_LENGTH 12 
#define SHADOW_WIDTH 3 
#define GRAPH_MASK    (GDK_EXPOSURE_MASK |              \
                       GDK_POINTER_MOTION_MASK |        \
                       GDK_POINTER_MOTION_HINT_MASK |   \
                       GDK_BUTTON_PRESS_MASK |          \
                       GDK_BUTTON_RELEASE_MASK)


static gchar DEFAULT_FONT[] = "Helvetica";


static void gtk_plot_canvas_class_init 		(GtkPlotCanvasClass *klass);
static void gtk_plot_canvas_init 		(GtkPlotCanvas *plot_canvas);
static void gtk_plot_canvas_finalize 		(GtkObject *object);
static void gtk_plot_canvas_map                 (GtkWidget *widget);
static void gtk_plot_canvas_size_request        (GtkWidget *widget, 
                                                 GtkRequisition *requisition);
static gint gtk_plot_canvas_motion 		(GtkWidget *widget, 
                                                 GdkEventMotion *event);
static gint gtk_plot_canvas_button_press	(GtkWidget *widget, 
                                                 GdkEventButton *event);
static gint gtk_plot_canvas_button_release	(GtkWidget *widget, 
                                                 GdkEventButton *event);
static gint gtk_plot_canvas_focus_in		(GtkWidget *widget, 
                                                 GdkEventFocus *event);
static gint gtk_plot_canvas_focus_out		(GtkWidget *widget, 
                                                 GdkEventFocus *event);
static void gtk_plot_canvas_remove		(GtkContainer *container,
						 GtkWidget *child);

/* Drawing functions */
static void gtk_plot_canvas_draw                (GtkWidget *widget, 
                                                 GdkRectangle *area);
static void gtk_plot_canvas_paint               (GtkWidget *widget);
static gint gtk_plot_canvas_expose              (GtkWidget *widget, 
                                                 GdkEventExpose *event);
static void gtk_plot_canvas_create_pixmap       (GtkWidget *widget, 
                                                 gint width, gint height);
static void gtk_plot_canvas_set_plots_pixmap    (GtkPlotCanvas *plot_canvas);
static void gtk_plot_canvas_get_real_pixel      (GtkWidget *widget,
                                                 gdouble px, gdouble py,
                                                 gint *x, gint *y);
static void draw_selection 			(GtkPlotCanvas *canvas, 
                				 GdkRectangle area,
                				 gboolean markers);
static void draw_marker				(GtkPlotCanvas *canvas, 
						 GdkGC *gc, gint x, gint y);
static void gtk_plot_canvas_draw_text           (GtkPlotCanvas *canvas,
                                                 GtkPlotCanvasChild *child);
static void rotate_text                         (GtkPlotCanvas *canvas,
                                                 GtkPlotText text,
                                                 gint *width, gint *height,
                                                 GdkPixmap **pixmap,
                                                 GdkBitmap **mask);

static void gtk_plot_canvas_draw_grid		(GtkPlotCanvas *canvas);
static void gtk_plot_canvas_draw_child		(GtkPlotCanvas *canvas,
                           			 GtkPlotCanvasChild *child);
static void gtk_plot_canvas_set_line_attributes	(GtkPlotLine line,
                                    		 GdkGC *gc);
/* Auxiliary functions */
static GtkPlotCanvasPos posible_selection	(GdkRectangle area, 
						 gint x, gint y);
static gint roundint                            (gdouble x);


/* Signals */

enum {
        SELECT_ITEM,
        MOVE_ITEM,
        RESIZE_ITEM,
        SELECT_REGION,
        LAST_SIGNAL
};

typedef gboolean (*GtkPlotCanvasSignal1) (GtkObject *object,
                                          gpointer arg1, 
					  gdouble arg2,
					  gdouble arg3,
                                    	  gpointer user_data);

typedef gboolean (*GtkPlotCanvasSignal2) (GtkObject *object,
                                          gpointer arg1, 
					  gpointer arg2,
                                    	  gpointer user_data);

typedef gboolean (*GtkPlotCanvasSignal3) (GtkObject *object,
                                          gdouble arg1, 
                                          gdouble arg2, 
                                          gdouble arg3, 
                                          gdouble arg4, 
                                    	  gpointer user_data);

static void
gtk_plot_canvas_marshal_move_resize             (GtkObject *object,
                                                 GtkSignalFunc func,
                                                 gpointer func_data,
                                                 GtkArg * args);

static void
gtk_plot_canvas_marshal_select_item             (GtkObject *object,
                                                 GtkSignalFunc func,
                                                 gpointer func_data,
                                                 GtkArg * args);

static void
gtk_plot_canvas_marshal_select		        (GtkObject *object,
                                                 GtkSignalFunc func,
                                                 gpointer func_data,
                                                 GtkArg * args);

static GtkPlotCanvasClass *parent_class = NULL;
static guint canvas_signals[LAST_SIGNAL] = {0};

GtkType
gtk_plot_canvas_get_type (void)
{
  static GtkType plot_canvas_type = 0;

  if (!plot_canvas_type)
    {
      GtkTypeInfo plot_canvas_info =
      {
	"GtkPlotCanvas",
	sizeof (GtkPlotCanvas),
	sizeof (GtkPlotCanvasClass),
	(GtkClassInitFunc) gtk_plot_canvas_class_init,
	(GtkObjectInitFunc) gtk_plot_canvas_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      plot_canvas_type = gtk_type_unique (gtk_fixed_get_type(), &plot_canvas_info);
    }
  return plot_canvas_type;
}

GtkType
gtk_plot_canvas_child_get_type (void)
{
  static GtkType plot_canvas_child_type = 0;

  if (!plot_canvas_child_type)
    {
      GtkTypeInfo plot_canvas_child_info =
      {
       "GtkPlotCanvasChild",
       0,
       0,
       (GtkClassInitFunc) NULL,
       (GtkObjectInitFunc) NULL,
       /* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      plot_canvas_child_type = gtk_type_unique (GTK_TYPE_BOXED,
                                              &plot_canvas_child_info);
    }
  return plot_canvas_child_type;
}

static void
gtk_plot_canvas_class_init (GtkPlotCanvasClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  parent_class = gtk_type_class (gtk_fixed_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  container_class = (GtkContainerClass *) klass;

  canvas_signals[SELECT_ITEM] =
    gtk_signal_new ("select_item",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, select_item),
                    gtk_plot_canvas_marshal_select_item,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_GDK_EVENT, 
                    GTK_TYPE_PLOT_CANVAS_CHILD);

  canvas_signals[MOVE_ITEM] =
    gtk_signal_new ("move_item",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, move_item),
                    gtk_plot_canvas_marshal_move_resize,
                    GTK_TYPE_BOOL, 3, GTK_TYPE_PLOT_CANVAS_CHILD, 
                    GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE);

  canvas_signals[RESIZE_ITEM] =
    gtk_signal_new ("resize_item",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, resize_item),
                    gtk_plot_canvas_marshal_move_resize,
                    GTK_TYPE_BOOL, 3, GTK_TYPE_PLOT_CANVAS_CHILD,
                    GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE);

  canvas_signals[SELECT_REGION] =
    gtk_signal_new ("select_region",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, select_region),
                    gtk_plot_canvas_marshal_select,
                    GTK_TYPE_NONE, 4, 
                    GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE);

  gtk_object_class_add_signals (object_class, canvas_signals, LAST_SIGNAL);

  object_class->finalize = gtk_plot_canvas_finalize;

  widget_class->map = gtk_plot_canvas_map;
  widget_class->draw = gtk_plot_canvas_draw;
  widget_class->expose_event = gtk_plot_canvas_expose;
  widget_class->size_request = gtk_plot_canvas_size_request;
  widget_class->focus_in_event = gtk_plot_canvas_focus_in;
  widget_class->focus_out_event = gtk_plot_canvas_focus_out;
  widget_class->motion_notify_event = gtk_plot_canvas_motion;
  widget_class->button_press_event = gtk_plot_canvas_button_press;
  widget_class->button_release_event = gtk_plot_canvas_button_release;

  container_class->remove = gtk_plot_canvas_remove;

  klass->move_item = NULL;
  klass->resize_item = NULL;
  klass->select_item = NULL;
  klass->select_region = NULL;
}

static void
gtk_plot_canvas_marshal_move_resize           (GtkObject *object,
                                               GtkSignalFunc func,
                                               gpointer func_data,
                                               GtkArg * args)
{
  GtkPlotCanvasSignal1 rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[3]);

  rfunc = (GtkPlotCanvasSignal1) func;

  *veto = (*rfunc) (object, 
                    GTK_VALUE_POINTER (args[0]),
                    GTK_VALUE_DOUBLE (args[1]),
                    GTK_VALUE_DOUBLE (args[2]),
                    func_data);
}

static void
gtk_plot_canvas_marshal_select_item                   (GtkObject *object,
                                                       GtkSignalFunc func,
                                                       gpointer func_data,
                                                       GtkArg * args)
{
  GtkPlotCanvasSignal2 rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[2]);

  rfunc = (GtkPlotCanvasSignal2) func;

  *veto = (*rfunc) (object, 
                    GTK_VALUE_POINTER (args[0]),
                    GTK_VALUE_POINTER (args[1]),
                    func_data);
}


static void
gtk_plot_canvas_marshal_select			      (GtkObject *object,
                                                       GtkSignalFunc func,
                                                       gpointer func_data,
                                                       GtkArg * args)
{
  GtkPlotCanvasSignal3 rfunc;

  rfunc = (GtkPlotCanvasSignal3) func;

  (*rfunc) (object, 
            GTK_VALUE_DOUBLE (args[0]),
            GTK_VALUE_DOUBLE (args[1]),
            GTK_VALUE_DOUBLE (args[2]),
            GTK_VALUE_DOUBLE (args[3]),
            func_data);
}

static void
gtk_plot_canvas_init (GtkPlotCanvas *plot_canvas)
{
  GtkWidget *widget;
  GdkColor color;

  widget = GTK_WIDGET(plot_canvas);
  GTK_WIDGET_SET_FLAGS(widget, GTK_CAN_FOCUS);

  gdk_color_black(gtk_widget_get_colormap(widget), &widget->style->black);
  gdk_color_white(gtk_widget_get_colormap(widget), &widget->style->white);

  gtk_widget_set_events (widget, gtk_widget_get_events(widget)|
                         GRAPH_MASK);

  plot_canvas->cursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);

  plot_canvas->num_plots = 0;
  plot_canvas->background = widget->style->white;

  plot_canvas->flags = 0;
  plot_canvas->state = GTK_STATE_NORMAL;
  plot_canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
  plot_canvas->magnification = 1.;

  plot_canvas->show_grid = FALSE;
  plot_canvas->grid_step = 20;
  plot_canvas->grid.line_style = GTK_PLOT_LINE_SOLID;
  plot_canvas->grid.line_width = 0;

  gdk_color_parse("grey90", &color);
  gdk_color_alloc(gdk_colormap_get_system(), &color);
  plot_canvas->grid.color = color;

  plot_canvas->active_plot = NULL;
  plot_canvas->active_data = NULL;
  plot_canvas->active_point = -1;

  plot_canvas->drag_x = plot_canvas->drag_y = 0;
  plot_canvas->pointer_x = plot_canvas->pointer_y = 0;

  plot_canvas->plots = NULL;
  plot_canvas->childs = NULL;

  plot_canvas->width = DEFAULT_WIDTH;
  plot_canvas->height = DEFAULT_HEIGHT;
  plot_canvas->pixmap_width = DEFAULT_WIDTH;
  plot_canvas->pixmap_height = DEFAULT_HEIGHT;
}

GtkWidget*
gtk_plot_canvas_new (gint width, gint height, gdouble magnification)
{
  GtkPlotCanvas *plot_canvas;
  gdouble m = magnification;

  plot_canvas = gtk_type_new (gtk_plot_canvas_get_type ());

  plot_canvas->width = width;
  plot_canvas->height = height;
  plot_canvas->pixmap_width = roundint(width * m);
  plot_canvas->pixmap_height = roundint(height * m);
  gtk_plot_canvas_set_magnification(plot_canvas, m);

  return GTK_WIDGET (plot_canvas);
}


static void
gtk_plot_canvas_finalize (GtkObject *object)
{
  GtkPlotCanvas *plot_canvas;
  GList *list;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT_CANVAS (object));

  plot_canvas = GTK_PLOT_CANVAS (object);
  
  list = plot_canvas->childs;
  while(list){
    GtkPlotCanvasChild *child;

    child = (GtkPlotCanvasChild *) list->data;

    if(child->data)
         g_free(child->data);

    g_free(child);

    plot_canvas->childs = g_list_remove_link(plot_canvas->childs, list);
    g_list_free_1(list);
    list = plot_canvas->childs;
  }


  if (GTK_OBJECT_CLASS (parent_class)->finalize)
    (*GTK_OBJECT_CLASS (parent_class)->finalize) (object);

  gtk_psfont_unref();
}

static void
gtk_plot_canvas_remove(GtkContainer *container, GtkWidget *child)
{
  GtkPlotCanvas *canvas;
  GList *list;

  canvas = GTK_PLOT_CANVAS(container);
  gtk_plot_canvas_cancel_action(canvas);

  list = canvas->plots;
  while(list){
   if(list->data == child){
      canvas->plots = g_list_remove_link(canvas->plots, list);
      g_list_free_1(list);
      canvas->num_plots--;
      break;
   }
   list = list->next;
  }
 
  GTK_CONTAINER_CLASS(parent_class)->remove(container, child);
}

static void
gtk_plot_canvas_draw (GtkWidget *widget, GdkRectangle *area)
{
  GTK_WIDGET_CLASS(parent_class)->draw(widget, area);

  gtk_plot_canvas_paint(widget);
}

static void
gtk_plot_canvas_paint (GtkWidget *widget)
{
  GtkPlotCanvas *canvas;
  GList *childs;
  GdkGC *gc;

  canvas = GTK_PLOT_CANVAS(widget);

  if(!canvas->pixmap) return;

  gc = gdk_gc_new(canvas->pixmap);
  gdk_gc_set_foreground(gc, &canvas->background);

  gdk_draw_rectangle(canvas->pixmap,
                     gc,
                     TRUE,
                     0,0,canvas->pixmap_width, canvas->pixmap_height);

  gtk_plot_canvas_draw_grid(canvas);

  gtk_plot_canvas_set_plots_pixmap(canvas);

  childs = canvas->childs;
  while(childs)
   {
     GtkPlotCanvasChild *child;

     child = (GtkPlotCanvasChild *) childs->data;
     gtk_plot_canvas_draw_child(canvas, child);
     childs = childs->next;
   }

  gdk_draw_pixmap(GTK_WIDGET(canvas)->window,
                  widget->style->fg_gc[GTK_STATE_NORMAL],
                  canvas->pixmap,
                  0, 0,
                  0, 0,
                  -1, -1);

  gdk_gc_unref(gc);
}

void
gtk_plot_canvas_refresh(GtkPlotCanvas *canvas)
{
  GList *plots;
  GList *childs;
  GtkPlot *plot;
  GdkRectangle area;
  GdkGC *gc;
  GtkPlotCanvasChild *child;

  gc = gdk_gc_new(canvas->pixmap);
  gdk_gc_set_foreground(gc, &canvas->background);

  gdk_draw_rectangle(canvas->pixmap,
                     gc,
                     TRUE,
                     0,0,canvas->pixmap_width, canvas->pixmap_height);

  gtk_plot_canvas_draw_grid(canvas);

  plots = canvas->plots;
  while(plots)
    {
      plot = GTK_PLOT(plots->data);
      gtk_plot_set_drawable(plot, canvas->pixmap);
      area.x = GTK_WIDGET(plot)->allocation.x;
      area.y = GTK_WIDGET(plot)->allocation.y;
      area.width = GTK_WIDGET(plot)->allocation.width;
      area.height = GTK_WIDGET(plot)->allocation.height;
      gtk_plot_paint(plot, &area);
      plots = plots->next;
    }

  childs = canvas->childs;
  while(childs)
   {
     child = (GtkPlotCanvasChild *) childs->data;
     gtk_plot_canvas_draw_child(canvas, child);
     childs = childs->next;
   }

  gdk_draw_pixmap(GTK_WIDGET(canvas)->window,
                  GTK_WIDGET(canvas)->style->fg_gc[GTK_STATE_NORMAL],
                  canvas->pixmap,
                  0, 0,
                  0, 0,
                  -1, -1);

  gdk_gc_unref(gc);

}

static void
gtk_plot_canvas_draw_grid(GtkPlotCanvas *canvas)
{
  GdkGC *gc;
  gint x, y;

  if(!canvas->pixmap) return;
  if(!canvas->show_grid) return;

  gc = gdk_gc_new(canvas->pixmap); 
  gtk_plot_canvas_set_line_attributes(canvas->grid, gc);

  for(x = 0; x < canvas->pixmap_width; x += canvas->grid_step)
      gdk_draw_line(canvas->pixmap, gc, x, 0, x, canvas->pixmap_height);

  for(y = 0; y < canvas->pixmap_height; y += canvas->grid_step)
      gdk_draw_line(canvas->pixmap, gc, 0, y, canvas->pixmap_width, y);

  gdk_gc_unref(gc);
}

static void
gtk_plot_canvas_map(GtkWidget *widget)
{
  GtkPlotCanvas *plot_canvas;

  plot_canvas=GTK_PLOT_CANVAS(widget);

  GTK_WIDGET_CLASS(parent_class)->map(widget);

  if(!plot_canvas->pixmap){
      gtk_plot_canvas_create_pixmap(widget, 
                                    plot_canvas->pixmap_width, 
                                    plot_canvas->pixmap_height);
      gtk_plot_canvas_paint(widget);
      return;
  }

  gtk_plot_canvas_refresh(plot_canvas);
  gdk_window_set_cursor(widget->window, plot_canvas->cursor);

}


static gint 
gtk_plot_canvas_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GtkPlotCanvas *canvas;
  GtkPlot *active_plot;
  GtkPlotData *active_dataset;
  gint active_point;
  GtkAllocation *allocation = NULL;
  GdkRectangle area;
  gint x, y, dx, dy;
  gint new_x, new_y;
  gint new_width, new_height;
  gint cursor;

  canvas = GTK_PLOT_CANVAS(widget);
  gtk_widget_get_pointer(widget, &x, &y);

  area = canvas->active_item.allocation;
  new_x = area.x;
  new_y = area.y;
  new_width = area.width;
  new_height = area.height;

  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) 
       cursor = GDK_TOP_LEFT_ARROW;
  else
       switch(canvas->drag_point){
            case GTK_PLOT_CANVAS_TOP_LEFT: 
                 cursor = GDK_UL_ANGLE; 
                 break;
            case GTK_PLOT_CANVAS_TOP_RIGHT:
                 cursor = GDK_UR_ANGLE; 
                 break;
            case GTK_PLOT_CANVAS_TOP:
                 cursor = GDK_TOP_SIDE;
                 break;
            case GTK_PLOT_CANVAS_BOTTOM_LEFT:
                 cursor = GDK_LL_ANGLE;
                 break;
            case GTK_PLOT_CANVAS_BOTTOM_RIGHT:
                 cursor = GDK_LR_ANGLE;
                 break;
            case GTK_PLOT_CANVAS_BOTTOM:
                 cursor = GDK_BOTTOM_SIDE;
                 break;
            case GTK_PLOT_CANVAS_LEFT:
                 cursor = GDK_LEFT_SIDE;
                 break;
            case GTK_PLOT_CANVAS_RIGHT:
                 cursor = GDK_RIGHT_SIDE;
                 break;
            default:
                 cursor = GDK_TOP_LEFT_ARROW;
       }

  if(cursor != canvas->cursor->type){
         gdk_cursor_destroy(canvas->cursor);
         canvas->cursor = gdk_cursor_new(cursor);
         gdk_window_set_cursor(widget->window, canvas->cursor);
  }    
    

  active_plot = canvas->active_plot;
  active_dataset = canvas->active_data;
  active_point = canvas->active_point;
 
  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) return TRUE;
  if(active_plot == NULL && canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION)
     return TRUE;

  if(active_plot)
         allocation = &GTK_WIDGET(canvas->active_plot)->allocation;

  switch(canvas->action){
     case GTK_PLOT_CANVAS_ACTION_DRAG:
       if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_MOVE){
         draw_selection(canvas, canvas->drag_area, TRUE);
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         dx = x - canvas->drag_x;
         dy = y - canvas->drag_y;
         area.x = canvas->active_item.allocation.x + dx; 
         area.y = canvas->active_item.allocation.y + dy;
         draw_selection(canvas, area, TRUE);
         canvas->drag_area = area;
       }
       break;
     case GTK_PLOT_CANVAS_ACTION_RESIZE:
       switch(canvas->drag_point){
            case GTK_PLOT_CANVAS_TOP_LEFT: 
            case GTK_PLOT_CANVAS_TOP_RIGHT:
            case GTK_PLOT_CANVAS_TOP:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_Y_RESIZE){
                       dy = y - canvas->drag_y;
                       new_y = canvas->active_item.allocation.y + dy;
                       new_height = canvas->active_item.allocation.height - dy;
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_TOP_LEFT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_x = canvas->active_item.allocation.x + dx;
                          new_width = canvas->active_item.allocation.width - dx;
                  }
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_TOP_RIGHT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_width = canvas->active_item.allocation.width + dx;
                  }
               }
               gdk_cursor_destroy(canvas->cursor);
               canvas->cursor = gdk_cursor_new(cursor);
               gdk_window_set_cursor(widget->window, canvas->cursor);
               break;
            case GTK_PLOT_CANVAS_BOTTOM_LEFT:
            case GTK_PLOT_CANVAS_BOTTOM_RIGHT:
            case GTK_PLOT_CANVAS_BOTTOM:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_Y_RESIZE){
                       dy = y - canvas->drag_y;
                       new_height = canvas->active_item.allocation.height + dy;
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_BOTTOM_LEFT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_x = canvas->active_item.allocation.x + dx;
                          new_width = canvas->active_item.allocation.width - dx;
                  }
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_BOTTOM_RIGHT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_width = canvas->active_item.allocation.width + dx;
                  }
               }
               break;
            case GTK_PLOT_CANVAS_LEFT:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                       dx = x - canvas->drag_x;
                       new_x = canvas->active_item.allocation.x + dx;
                       new_width = canvas->active_item.allocation.width - dx;
               }
               break;
            case GTK_PLOT_CANVAS_RIGHT:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                       dx = x - canvas->drag_x;
                       new_width = canvas->active_item.allocation.width + dx;
               }
               break;
            case GTK_PLOT_CANVAS_IN:
            case GTK_PLOT_CANVAS_OUT:
            default:
               break;
       }

       if(new_width >= canvas->active_item.min_width &&
          new_height >= canvas->active_item.min_height){ 
                canvas->pointer_x = x;
                canvas->pointer_y = y;
                draw_selection(canvas, canvas->drag_area, TRUE);
                area.x = new_x;
                area.y = new_y;
                area.width = new_width;
                area.height = new_height;
                draw_selection(canvas, area, TRUE);
                canvas->drag_area = area;
       }
       break;
     case GTK_PLOT_CANVAS_ACTION_SELECTION:
         draw_selection(canvas, canvas->drag_area, FALSE);
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         area.x = MIN(canvas->pointer_x, canvas->drag_x); 
         area.y = MIN(canvas->pointer_y, canvas->drag_y); 
         area.width = abs(x - canvas->drag_x);
         area.height = abs(y - canvas->drag_y);
         canvas->drag_area = area;
         draw_selection(canvas, canvas->drag_area, FALSE);
 	 break;
     case GTK_PLOT_CANVAS_ACTION_INACTIVE:
     default:
         break;
  }

  return TRUE;

}

static gint
gtk_plot_canvas_button_press(GtkWidget *widget, GdkEventButton *event)
{
  GtkPlotCanvas *canvas = NULL;
  GtkPlotCanvasChild active_item;
  GtkPlot *active_plot = NULL;
  GList *plots = NULL;
  GList *dataset = NULL;
  GList *childs = NULL;
  GtkPlot *plot = NULL;
  GtkPlot *click_plot = NULL;
  GtkPlotData *data = NULL;
  GtkPlotText *child_text;
  GtkAllocation internal_allocation;
  GtkAllocation legends_allocation;
  GdkRectangle area, drag_area;
  GtkPlotAxis *axis[4];
  GdkModifierType mods;
  gint i = 0;
  gint x = 0, y = 0;
  gint xi = 0, yi = 0;
  gint px = 0, py = 0;
  gint pwidth = 0, pheight = 0;
  gint tx = 0, ty = 0;
  gint rx = 0, ry = 0;
  gint twidth = 0, theight = 0;
  gboolean veto;
  gboolean new_selection = FALSE;
  gboolean double_click = FALSE;
  guint state = GTK_STATE_NORMAL;
  GtkPlotCanvasPos pos, new_pos = 0;
  gdouble m;

  gdk_window_get_pointer(widget->window, NULL, NULL, &mods);
  if(!(mods & GDK_BUTTON1_MASK)) return FALSE;
  double_click = (event->button == GDK_2BUTTON_PRESS);
 
  canvas = GTK_PLOT_CANVAS(widget);
  active_plot = canvas->active_plot;
  m = canvas->magnification;

/*
  if(double_click && canvas->state == GTK_STATE_SELECTED) return TRUE;
*/
  gdk_pointer_ungrab(event->time);

  if(!GTK_WIDGET_HAS_FOCUS(widget)) gtk_widget_grab_focus(widget);

  gtk_widget_get_pointer(widget, &x, &y);

/**********************************************************************/
  childs = canvas->childs;

  while(childs)
    {
      GtkPlotCanvasChild *child;
      gint x1, x2, y1, y2;
      gint width, height;

      child = (GtkPlotCanvasChild *)childs->data;
      gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(widget), 
                                child->rx1, child->ry1,
                                &x1, &y1);
      gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(widget), 
                                child->rx2, child->ry2,
                                &x2, &y2);

      width = abs(x2 - x1);
      height = abs(y2 - y1);

      area.x = MIN(x1, x2);
      area.y = MIN(y1, y2);
      area.width = width;
      area.height = height;

      if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
          active_item.data = child;
          active_item.allocation = area;
          active_item.min_width = 0;
          active_item.min_height = 0;
          active_item.type = child->type;
          active_item.state = GTK_STATE_SELECTED;
          active_item.flags = child->flags;
          state = GTK_STATE_SELECTED;
          new_selection = TRUE;
          new_pos = pos;
      }

      childs = childs->next;
    }

/**********************************************************************/
/*  text = canvas->text;

  while(text)
    {
      child_text = (GtkPlotText *)text->data;
      gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(widget), 
                                child_text->x, child_text->y,
                                &tx, &ty);

      gtk_plot_text_get_area(child_text, m, &rx, &ry, &twidth, &theight); 

      area.x = tx + rx;
      area.y = ty + ry;
      area.width = twidth;
      area.height = theight;

      if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
          active_item.data = text->data;
          active_item.allocation = area;
          active_item.type = GTK_PLOT_CANVAS_TEXT;
          active_item.state = GTK_STATE_SELECTED;
          active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
          state = GTK_STATE_SELECTED;
          new_selection = TRUE;
          new_pos = pos;
      }

      text = text->next;
    }
*/
/**********************************************************************/
  plots = canvas->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;

       axis[0]=&plot->left;
       axis[1]=&plot->right;
       axis[2]=&plot->top;
       axis[3]=&plot->bottom;

       for(i = 0; i <= 3; i++){
          if(axis[i]->title_visible){
            child_text = &axis[i]->title;
            gtk_plot_canvas_get_real_pixel(GTK_WIDGET(plot), 
                                           child_text->x, child_text->y,
                                           &tx, &ty);
            gtk_plot_text_get_area(child_text, m, &rx, &ry, &twidth, &theight);

            area.x = tx + rx;
            area.y = ty + ry;
            area.width = twidth;
            area.height = theight;

            if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
                active_item.data = axis[i];
                active_item.allocation = area;
                active_item.type = GTK_PLOT_CANVAS_TITLE;
                active_item.state = GTK_STATE_SELECTED;
                active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
                state = GTK_STATE_SELECTED;
                click_plot = plot;
                new_selection = TRUE;
                new_pos = pos;
                break;
            }
          }
       }

       plots = plots->next;
     }

/**********************************************************************/
 
  plots = canvas->plots;
  while(plots)
   {
     plot = (GtkPlot *)plots->data;

     if(plot->show_legends){

       legends_allocation = gtk_plot_legends_get_allocation (plot);
       area.x = legends_allocation.x;
       area.y = legends_allocation.y;
       area.width = legends_allocation.width;
       area.height = legends_allocation.height;

       if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
           active_item.data = plot;
           active_item.allocation = area;
           active_item.type = GTK_PLOT_CANVAS_LEGENDS;
           active_item.state = GTK_STATE_SELECTED;
           active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
           state = GTK_STATE_SELECTED;
           click_plot = plot;
           new_selection = TRUE;
           new_pos = pos;
           break;
       }
     }
     plots=plots->next;
   } 

/**********************************************************************/
  if(!new_selection){

     plots = canvas->plots;
     while(plots)
        {
          plot = (GtkPlot *)plots->data;

          internal_allocation = gtk_plot_get_internal_allocation (plot);
          px = internal_allocation.x;
          py = internal_allocation.y;
          pwidth = internal_allocation.width;
          pheight = internal_allocation.height;

          area.x = px - 6;
          area.y = py;
          area.width = 6;
          area.height = pheight;
          if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
              active_item.data = &plot->left;
              active_item.allocation = area;
              active_item.type = GTK_PLOT_CANVAS_AXIS;
              active_item.state = GTK_STATE_SELECTED;
              active_item.flags = GTK_PLOT_CANVAS_FROZEN;
              state = GTK_STATE_SELECTED;
              new_pos = pos;
              click_plot = plot;
          }
          area.x = px + pwidth;
          if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
              active_item.data = &plot->right;
              active_item.allocation = area;
              active_item.type = GTK_PLOT_CANVAS_AXIS;
              active_item.state = GTK_STATE_SELECTED;
              active_item.flags = GTK_PLOT_CANVAS_FROZEN;
              state = GTK_STATE_SELECTED;
              new_pos = pos;
              click_plot = plot;
          }
          area.x = px;
          area.y = py - 6;
          area.width = pwidth;
          area.height = 6;
          if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
              active_item.data = &plot->top;
              active_item.allocation = area;
              active_item.type = GTK_PLOT_CANVAS_AXIS;
              active_item.state = GTK_STATE_SELECTED;
              active_item.flags = GTK_PLOT_CANVAS_FROZEN;
              state = GTK_STATE_SELECTED;
              new_pos = pos;
              click_plot = plot;
          }
          area.y = py + pheight;
          if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
              active_item.data = &plot->bottom;
              active_item.allocation = area;
              active_item.type = GTK_PLOT_CANVAS_AXIS;
              active_item.state = GTK_STATE_SELECTED;
              active_item.flags = GTK_PLOT_CANVAS_FROZEN;
              state = GTK_STATE_SELECTED;
              new_pos = pos;
              click_plot = plot;
          }
  
          area.x = px;
          area.y = py;
          area.width = pwidth;
          area.height = pheight;

          if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
              active_item.data = plot;
              active_item.allocation = area;
              active_item.min_width = 2 * DEFAULT_MARKER_SIZE;
              active_item.min_height = 2 * DEFAULT_MARKER_SIZE;
              active_item.type = GTK_PLOT_CANVAS_PLOT;
              active_item.state = GTK_STATE_SELECTED;
              active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE |
                                  GTK_PLOT_CANVAS_CAN_X_RESIZE |
                                  GTK_PLOT_CANVAS_CAN_Y_RESIZE;
              state = GTK_STATE_SELECTED;
              new_pos = pos;
              click_plot = plot;
          }

          plots=plots->next;
        } 
  }

/**********************************************************************/
  if(!active_plot) active_plot = click_plot;

  if(active_plot){
    dataset = active_plot->data_sets;

    while(dataset)
     {
       data = (GtkPlotData *)dataset->data;
       for(i = 0; i < data->num_points; i++)
        {
         gtk_plot_get_pixel(active_plot, data->x[i], data->y[i], &xi, &yi);
         if(abs(xi-x) <= DEFAULT_MARKER_SIZE && 
            abs(yi-y) <= DEFAULT_MARKER_SIZE)
          {
              active_item.type = GTK_PLOT_CANVAS_DATA;
              active_item.state = GTK_STATE_SELECTED;
              active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
              gtk_plot_get_pixel(active_plot, 
                                 canvas->active_x,
                                 canvas->active_y,
                                 &x, &y);
              active_item.allocation.x = xi;
              active_item.allocation.y = yi;
              active_item.allocation.width = 20;
              active_item.allocation.height = 20;
              active_item.data = data;
              new_pos = GTK_PLOT_CANVAS_IN;
              state = GTK_STATE_SELECTED;
              break;
          }
        }

       if(active_item.type == GTK_PLOT_CANVAS_DATA) break;
       dataset = dataset->next;
     }
  }

/**********************************************************************/
/**********************************************************************/

  if(state == GTK_STATE_SELECTED){
    gboolean new_item;

    veto = TRUE;
    gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_ITEM],
                    event, &active_item, &veto);

    new_item = (canvas->state != GTK_STATE_SELECTED ||
                active_item.type != canvas->active_item.type ||
                active_item.data != canvas->active_item.data);

    if(!new_item && active_item.type != GTK_PLOT_CANVAS_TEXT &&
       active_item.type != GTK_PLOT_CANVAS_ELLIPSE &&
       active_item.type != GTK_PLOT_CANVAS_RECTANGLE &&
       active_item.type != GTK_PLOT_CANVAS_LINE &&
       active_item.type != GTK_PLOT_CANVAS_CUSTOM &&
       click_plot != canvas->active_plot) new_item = TRUE;

    if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA &&
       active_item.type == GTK_PLOT_CANVAS_DATA &&
       active_item.data == canvas->active_item.data)
                new_item = !(canvas->active_point == i); 

    if(new_item &&
       ((active_item.type == GTK_PLOT_CANVAS_DATA &&
        GTK_PLOT_CANVAS_CAN_SELECT_POINT(canvas)) ||
        (active_item.type != GTK_PLOT_CANVAS_DATA &&
         GTK_PLOT_CANVAS_CAN_SELECT_ITEM(canvas)))) {

         if(veto){
           if(click_plot) canvas->active_plot = click_plot;
           gtk_plot_canvas_unselect(canvas);
           canvas->active_item = active_item;
           canvas->drag_area = active_item.allocation;
           canvas->state = GTK_STATE_SELECTED; 
           canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;

           if(active_item.type == GTK_PLOT_CANVAS_DATA){
             canvas->active_data = data;
             canvas->active_point = i;
             canvas->active_x = data->x[i];
             canvas->active_y = data->y[i];
           }

           draw_selection(canvas, active_item.allocation, TRUE);

           return TRUE;
         }
    }

    if(!new_item &&
       ((active_item.type == GTK_PLOT_CANVAS_DATA &&
        GTK_PLOT_CANVAS_CAN_DND_POINT(canvas)) ||
        (active_item.type != GTK_PLOT_CANVAS_DATA &&
         GTK_PLOT_CANVAS_CAN_DND(canvas)))) {

         switch(new_pos){
          case GTK_PLOT_CANVAS_IN:
              canvas->action = GTK_PLOT_CANVAS_ACTION_DRAG;
              break;
          default:
              canvas->action = GTK_PLOT_CANVAS_ACTION_RESIZE;
         }
         gdk_pointer_grab (widget->window, FALSE,
                           GDK_POINTER_MOTION_HINT_MASK |
                           GDK_BUTTON1_MOTION_MASK |
                           GDK_BUTTON_RELEASE_MASK,
                           NULL, NULL, event->time);
         canvas->drag_point = new_pos;
         canvas->drag_x = x;
         canvas->drag_y = y;
         canvas->pointer_x = x;
         canvas->pointer_y = y;

         return TRUE;
    }

  } 

  gtk_plot_canvas_unselect(canvas);

  if(GTK_PLOT_CANVAS_CAN_SELECT(canvas)){
         active_item.data = NULL;
         active_item.allocation.x = x;
         active_item.allocation.y = y;
         active_item.allocation.width = 0;
         active_item.allocation.height = 0;
         drag_area = active_item.allocation;
         active_item.type = GTK_PLOT_CANVAS_NONE;
         active_item.state = GTK_STATE_SELECTED;
         active_item.flags = GTK_PLOT_CANVAS_CAN_X_RESIZE |
                             GTK_PLOT_CANVAS_CAN_Y_RESIZE;
         veto = TRUE;

         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_ITEM],
                         &canvas->active_item, &veto);
         if(veto){
           canvas->active_item = active_item;
           canvas->active_item.data = click_plot;
           canvas->active_plot = click_plot;
           canvas->state = GTK_STATE_SELECTED;
           canvas->action = GTK_PLOT_CANVAS_ACTION_SELECTION;
           canvas->drag_point = new_pos;
           canvas->drag_x = x;
           canvas->drag_y = y;
           canvas->pointer_x = x;
           canvas->pointer_y = y;
           canvas->drag_area = active_item.allocation;
           gdk_pointer_grab (widget->window, FALSE,
                             GDK_POINTER_MOTION_HINT_MASK |
                             GDK_BUTTON1_MOTION_MASK |
                             GDK_BUTTON_RELEASE_MASK,
                             NULL, NULL, event->time);
           draw_selection(canvas, active_item.allocation, FALSE);
         }
  }

  return TRUE;  
}

 
static gint
gtk_plot_canvas_button_release(GtkWidget *widget, GdkEventButton *event)
{
  GtkPlotCanvas *canvas;
  GtkPlotCanvasChild *child;
  GtkPlotText *text;
  GtkPlotAxis *axis = NULL;
  GtkPlot *active_plot;
  gint x, y;
  gdouble dx, dy;
  gdouble new_x, new_y;
  gdouble new_width, new_height;
  gdouble x1 = 0., y1 = 0., x2 = 0., y2 = 0.;
  gboolean veto = TRUE;

  canvas = GTK_PLOT_CANVAS(widget); 

  gdk_pointer_ungrab(event->time);

  if(GTK_WIDGET_MAPPED(widget)){
      gdk_cursor_destroy(canvas->cursor);
      canvas->cursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);
      gdk_window_set_cursor(widget->window, 
                            canvas->cursor);
  }

  active_plot = canvas->active_plot;
  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) return TRUE;
  if(active_plot == NULL && canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION)
     return TRUE;


  gtk_plot_canvas_get_position(canvas,
                               canvas->drag_area.width, 
                               canvas->drag_area.height, 
                               &new_width, &new_height);

  gtk_plot_canvas_get_position(canvas,
                               canvas->drag_area.x, 
                               canvas->drag_area.y, 
                               &new_x, &new_y);

  gtk_plot_canvas_get_position(canvas,
                               canvas->drag_area.x - 
                               canvas->active_item.allocation.x, 
                               canvas->drag_area.y - 
                               canvas->active_item.allocation.y, 
                               &dx, &dy);


  if(canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION) {
   switch(canvas->active_item.type){
    case GTK_PLOT_CANVAS_TEXT:
         child = (GtkPlotCanvasChild *)canvas->active_item.data;
         text = (GtkPlotText *)child->data;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, 
                         text->x + dx, text->y + dy, &veto);
         if(!veto) break;
         text->x += dx;
         text->y += dy;
         x1 += dx;
         x2 += dx;
         y1 += dy;
         y2 += dy;
         break;
    case GTK_PLOT_CANVAS_RECTANGLE:
    case GTK_PLOT_CANVAS_LINE:
    case GTK_PLOT_CANVAS_ELLIPSE:
    case GTK_PLOT_CANVAS_CUSTOM:
         child = (GtkPlotCanvasChild *)canvas->active_item.data;
         x1 = MIN(child->rx1, child->rx2);
         y1 = MIN(child->ry1, child->ry2);
         x2 = MAX(child->rx1, child->rx2);
         y2 = MAX(child->ry1, child->ry2);
         x1 += dx;
         y1 += dy;
         x2 = x1 + new_width;
         y2 = y1 + new_height;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         child, 
                         x1, x2, &veto);
         if(canvas->action != GTK_PLOT_CANVAS_ACTION_DRAG)
            gtk_signal_emit(GTK_OBJECT(canvas), 
                            canvas_signals[RESIZE_ITEM],
                            child, new_width, new_height, &veto);
         if(!veto) break;
         if(child->rx1 <= child->rx2) {
              child->rx1 = x1; 
              child->rx2 = x2; 
         } else {
              child->rx1 = x2; 
              child->rx2 = x1; 
         }
         if(child->ry1 <= child->ry2) {
              child->ry1 = y1; 
              child->ry2 = y2; 
         } else {
              child->ry1 = y2; 
              child->ry2 = y1; 
         }
         break;
    case GTK_PLOT_CANVAS_TITLE:
         axis = (GtkPlotAxis *)canvas->active_item.data;
         text = &axis->title;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, 
                         text->x + dx, text->y + dy, &veto);
         if(!veto) break;
         text->x += dx;
         text->y += dy;
         break;
    case GTK_PLOT_CANVAS_LEGENDS:
         gtk_plot_legends_get_position(active_plot, &new_x, &new_y);
         new_x += dx / active_plot->width;
         new_y += dy / active_plot->height;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, new_x, new_y, 
                         &veto);
         if(!veto) break;
         gtk_plot_legends_move(active_plot, new_x, new_y);
         break;
    case GTK_PLOT_CANVAS_PLOT:
         gtk_signal_emit(GTK_OBJECT(canvas), 
                         canvas_signals[MOVE_ITEM],
                         &canvas->active_item, new_x, new_y, &veto);
         if(canvas->action != GTK_PLOT_CANVAS_ACTION_DRAG)
            gtk_signal_emit(GTK_OBJECT(canvas), 
                            canvas_signals[RESIZE_ITEM],
                            &canvas->active_item, new_width, new_height, &veto);
         if(!veto) break;
         gtk_plot_move_resize(active_plot, 
                              new_x, 
                              new_y,
                              new_width,
                              new_height);
         break;
    case GTK_PLOT_CANVAS_DATA:
         if(canvas->active_data == NULL) break;
         if(canvas->active_point == -1) break;

         gtk_plot_get_pixel(canvas->active_plot, 
                            canvas->active_data->x[canvas->active_point],
                            canvas->active_data->y[canvas->active_point],
                            &x, &y);

         x += (canvas->pointer_x - canvas->drag_x);
         y += (canvas->pointer_y - canvas->drag_y);

         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, x, y, &veto);
         if(!veto) break;

         gtk_plot_get_point(canvas->active_plot, x, y, &new_x, &new_y);

         canvas->active_data->x[canvas->active_point] = new_x;
         canvas->active_data->y[canvas->active_point] = new_y;

         break;
    case GTK_PLOT_CANVAS_AXIS:
    case GTK_PLOT_CANVAS_NONE:
    default:
         break;
   }
  } else {
    gtk_plot_canvas_get_position(canvas, 
                                 canvas->drag_x, canvas->drag_y,
				 &x1, &y1);
    gtk_plot_canvas_get_position(canvas, 
                                 canvas->pointer_x, canvas->pointer_y,
				 &x2, &y2);
    new_width = abs(canvas->pointer_x - canvas->drag_x);
    new_height = abs(canvas->pointer_y - canvas->drag_y);
    gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_REGION],
                    x1, y1, x2, y2);
    canvas->state = GTK_STATE_NORMAL;
    canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
    return TRUE;
  }

  canvas->drag_x = canvas->pointer_x; 
  canvas->drag_y = canvas->pointer_y; 
  canvas->active_item.allocation.x = canvas->drag_area.x;
  canvas->active_item.allocation.y = canvas->drag_area.y;

  if(veto){
      gtk_plot_canvas_refresh(canvas);
      canvas->active_item.allocation = canvas->drag_area;
      if((canvas->active_item.type == GTK_PLOT_CANVAS_DATA &&
          canvas->action == GTK_PLOT_CANVAS_ACTION_SELECTION) ||
         (canvas->active_item.type != GTK_PLOT_CANVAS_DATA &&
          canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION))
             draw_selection(canvas, canvas->active_item.allocation, TRUE);
  } else {
      canvas->state = GTK_STATE_NORMAL;
  }  

  if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA &&
     canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION)
             canvas->state = GTK_STATE_NORMAL;
  canvas->active_data = NULL;
  canvas->active_point = -1;
  canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
 
  return TRUE;

}

static gint
gtk_plot_canvas_focus_in(GtkWidget *widget, GdkEventFocus *event)
{
  GTK_WIDGET_SET_FLAGS(widget, GTK_HAS_FOCUS);
  return FALSE;
}


static gint
gtk_plot_canvas_focus_out(GtkWidget *widget, GdkEventFocus *event)
{
  GTK_WIDGET_UNSET_FLAGS(widget, GTK_HAS_FOCUS);
  gtk_plot_canvas_unselect(GTK_PLOT_CANVAS(widget));

  return FALSE;
}

void
gtk_plot_canvas_add_plot (GtkPlotCanvas *plot_canvas, 
			  GtkPlot *plot, gdouble x, gdouble y)
{
  gint width, height;

  width = plot_canvas->pixmap_width, 
  height = plot_canvas->pixmap_height;

  gtk_plot_set_magnification(plot, plot_canvas->magnification);
  gtk_widget_set_usize(GTK_WIDGET(plot), width, height); 
  gtk_plot_move(plot, x, y);

  plot->left.title.x = plot->x - .1;
  plot->right.title.x = plot->x + plot->width + .1;
  plot->top.title.y = plot->y - .05;
  plot->bottom.title.y = plot->y + plot->height + .05;

  plot_canvas->plots = g_list_append(plot_canvas->plots, plot);
  gtk_plot_canvas_set_plots_pixmap(plot_canvas);

  gtk_fixed_put(GTK_FIXED(plot_canvas), GTK_WIDGET(plot), 0, 0);

  GTK_WIDGET(plot)->allocation.width = width;
  GTK_WIDGET(plot)->allocation.height = height;

  plot_canvas->active_plot = plot;

  plot_canvas->num_plots++;
}

static void
gtk_plot_canvas_set_plots_pixmap(GtkPlotCanvas *plot_canvas)
{
  GdkRectangle area;
  GList *plots;
  GtkPlot *plot;

  if(!plot_canvas->pixmap) return;
  plots = plot_canvas->plots;
  while(plots)
    {
      plot = GTK_PLOT(plots->data);
      gtk_plot_set_drawable(plot, plot_canvas->pixmap);
      area.x = GTK_WIDGET(plot)->allocation.x;
      area.y = GTK_WIDGET(plot)->allocation.y;
      area.width = GTK_WIDGET(plot)->allocation.width;
      area.height = GTK_WIDGET(plot)->allocation.height;
      gtk_widget_draw(GTK_WIDGET(plot), &area);
      plots = plots->next;
    }

}

void
gtk_plot_canvas_set_background (GtkPlotCanvas *canvas, GdkColor *color)
{

  g_return_if_fail (canvas != NULL);
  g_return_if_fail (GTK_IS_PLOT_CANVAS (canvas));

  canvas->background = *color;

  if(GTK_WIDGET_REALIZED(GTK_WIDGET(canvas)))
       gtk_plot_canvas_paint(GTK_WIDGET(canvas));

}

void
gtk_plot_canvas_get_pixel(GtkPlotCanvas *canvas, gdouble px, gdouble py,
                          gint *x, gint *y)
{
  *x = canvas->pixmap_width * px;
  *y = canvas->pixmap_height * py;
}

void
gtk_plot_canvas_get_position(GtkPlotCanvas *plot_canvas, gint x, gint y,
                             gdouble *px, gdouble *py)
{
  *px = (gdouble) x / (gdouble) plot_canvas->pixmap_width;
  *py = (gdouble) y / (gdouble) plot_canvas->pixmap_height;
}

static void
gtk_plot_canvas_get_real_pixel(GtkWidget *widget, gdouble px, gdouble py,
                               gint *x, gint *y)
{
  *x = widget->allocation.x + widget->allocation.width * px;
  *y = widget->allocation.y + widget->allocation.height * py;
}

void
gtk_plot_canvas_set_active_plot (GtkPlotCanvas *plot_canvas, GtkPlot *plot)
{
  plot_canvas->active_plot = plot;
}

void
gtk_plot_canvas_unselect (GtkPlotCanvas *plot_canvas)
{

  if(plot_canvas->state == GTK_STATE_SELECTED){
    if(plot_canvas->active_item.type == GTK_PLOT_CANVAS_NONE)
       draw_selection(plot_canvas, plot_canvas->drag_area, FALSE); 
    else
       draw_selection(plot_canvas, plot_canvas->drag_area, TRUE); 
  }

  plot_canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
  plot_canvas->state = GTK_STATE_NORMAL;
  plot_canvas->active_item.type = GTK_PLOT_CANVAS_NONE;
  plot_canvas->active_item.data = NULL;

  if(GTK_WIDGET_MAPPED(GTK_WIDGET(plot_canvas))){
      gdk_cursor_destroy(plot_canvas->cursor);
      plot_canvas->cursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);
      gdk_window_set_cursor(GTK_WIDGET(plot_canvas)->window, 
                            plot_canvas->cursor);
  }
}

void
gtk_plot_canvas_cancel_action (GtkPlotCanvas *plot_canvas)
{
  gtk_plot_canvas_unselect(plot_canvas);
  gdk_pointer_ungrab(GDK_CURRENT_TIME);
}


GtkPlot *
gtk_plot_canvas_get_active_plot(GtkPlotCanvas *canvas)
{
  return canvas->active_plot;
}


GtkPlotData *
gtk_plot_canvas_get_active_data(GtkPlotCanvas *canvas)
{
  return canvas->active_data;
}

void
gtk_plot_canvas_get_active_point(GtkPlotCanvas *canvas, gdouble *x, gdouble *y)
{
  x = &canvas->active_data->x[canvas->active_point];
  y = &canvas->active_data->y[canvas->active_point];
}

GtkPlotCanvasChild *
gtk_plot_canvas_get_active_item(GtkPlotCanvas *canvas)
{
  return &canvas->active_item;
}

void
gtk_plot_canvas_set_size(GtkPlotCanvas *canvas, gint width, gint height)
{
  GList *plots;
  GtkPlot *plot;
  gdouble m = canvas->magnification;

  canvas->width = width;
  canvas->height = height;
  canvas->pixmap_width = roundint(m * width);
  canvas->pixmap_height = roundint(m * height);

  if(GTK_WIDGET_MAPPED(canvas)){ 
    gtk_plot_canvas_create_pixmap(GTK_WIDGET(canvas), 
                                  canvas->pixmap_width, 
                                  canvas->pixmap_height);
    plots = canvas->plots;
    while(plots)
     {
       plot = (GtkPlot *)plots->data;
 
       gtk_widget_set_usize(GTK_WIDGET(plot), 
                            canvas->pixmap_width, canvas->pixmap_height); 

       plots = plots->next;
     }
    gtk_widget_set_usize(GTK_WIDGET(canvas), 
                         canvas->pixmap_width, canvas->pixmap_height);
  }

}

static void
gtk_plot_canvas_create_pixmap(GtkWidget *widget, gint width, gint height)
{
  GtkPlotCanvas *canvas;
  GdkGC* gc;
  gint pixmap_width, pixmap_height;

  canvas = GTK_PLOT_CANVAS(widget);
  if (!canvas->pixmap)
    canvas->pixmap = gdk_pixmap_new (widget->window,
                                     width,
                                     height, -1);
  else{
    gdk_window_get_size(canvas->pixmap, &pixmap_width, &pixmap_height);
    if(width != pixmap_width || height != pixmap_height)
        gdk_pixmap_unref(canvas->pixmap);
        canvas->pixmap = gdk_pixmap_new (widget->window,
                                         width,
                                         height, -1);
  }

  gc = gdk_gc_new(canvas->pixmap);
  gdk_gc_set_foreground(gc, &canvas->background);

  gdk_draw_rectangle(canvas->pixmap,
                     gc,
                     TRUE,
                     0, 0, 
                     canvas->pixmap_width, 
                     canvas->pixmap_height);

  gtk_plot_canvas_set_plots_pixmap(canvas);

  gdk_gc_unref(gc);

}

static gint
gtk_plot_canvas_expose(GtkWidget *widget, GdkEventExpose *event)
{
  GtkPlotCanvas *canvas;
  GdkPixmap *pixmap;

  if(!GTK_WIDGET_DRAWABLE(widget)) return FALSE;

  canvas = GTK_PLOT_CANVAS(widget);

  if(!canvas->pixmap){
      gtk_plot_canvas_create_pixmap(widget, 
                                    canvas->pixmap_width, 
                                    canvas->pixmap_height);
      gtk_plot_canvas_paint(widget);
      return FALSE;
  }

  pixmap = canvas->pixmap;
  gdk_draw_pixmap(GTK_WIDGET(canvas)->window,
                  widget->style->fg_gc[GTK_STATE_NORMAL],
                  pixmap, 
                  event->area.x, 
                  event->area.y,
                  event->area.x, 
                  event->area.y,
                  event->area.width, event->area.height);
  return FALSE;

}


static void
gtk_plot_canvas_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkPlotCanvas *canvas;

  canvas = GTK_PLOT_CANVAS(widget);

  GTK_WIDGET_CLASS(parent_class)->size_request(widget, requisition);

  widget->requisition.width = MAX(canvas->pixmap_width, requisition->width);
  widget->requisition.height = MAX(canvas->pixmap_height, requisition->height);
}


void
gtk_plot_canvas_set_magnification(GtkPlotCanvas *canvas, 
                                  gdouble magnification)
{
  GList *plots;
  GtkPlot *plot;
  gdouble m;

  m = canvas->magnification = magnification;

  plots = canvas->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;
 
       gtk_plot_set_magnification(plot, m); 

       plots = plots->next;
     }

  gtk_plot_canvas_set_size(canvas, 
                           canvas->width, 
                           canvas->height);
}

static void
draw_selection (GtkPlotCanvas *canvas, 
                GdkRectangle area,
                gboolean markers)
{
  GdkGC *xor_gc;
  GdkGCValues values;

  gdk_gc_get_values(GTK_WIDGET(canvas)->style->fg_gc[0], &values);
  values.function = GDK_INVERT;
  values.foreground = GTK_WIDGET(canvas)->style->white;
  values.subwindow_mode = GDK_INCLUDE_INFERIORS;
  xor_gc = gdk_gc_new_with_values(GTK_WIDGET(canvas)->window,
                                  &values,
                                  GDK_GC_FOREGROUND |
                                  GDK_GC_FUNCTION |
                                  GDK_GC_SUBWINDOW);

  if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA){
    gint x, y;
    gdouble old_x, old_y;
    gdouble new_x, new_y;

    area.x -= area.width / 2;
    area.y -= area.height / 2;
    gdk_draw_rectangle (GTK_WIDGET(canvas)->window,
                        xor_gc,
                        FALSE,
                        area.x, area.y,
                        area.width, area.height);         

    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + 1, area.y + area.height/2, 
                  area.x + 6, area.y + area.height/2);
    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + area.width - 1, area.y + area.height / 2, 
                  area.x + area.width - 6, area.y + area.height / 2);
    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + area.width/2, area.y + 1, 
                  area.x + area.width/2, area.y + 6);
    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + area.width/2, area.y + area.height - 1, 
                  area.x + area.width/2, area.y + area.height - 6);

    if(canvas->action == GTK_PLOT_CANVAS_ACTION_DRAG){
      old_x = canvas->active_data->x[canvas->active_point];
      old_y = canvas->active_data->y[canvas->active_point];

      gtk_plot_get_pixel(canvas->active_plot, old_x, old_y, &x, &y);

      x += (canvas->pointer_x - canvas->drag_x);
      y += (canvas->pointer_y - canvas->drag_y);

      gtk_plot_get_point(canvas->active_plot, x, y, &new_x, &new_y);

      canvas->active_data->x[canvas->active_point] = new_x;
      canvas->active_data->y[canvas->active_point] = new_y;

      gtk_plot_set_drawable(canvas->active_plot, GTK_WIDGET(canvas)->window);
      gtk_plot_draw_data(canvas->active_plot, xor_gc, canvas->active_data);
      gtk_plot_set_drawable(canvas->active_plot, canvas->pixmap);

      canvas->active_data->x[canvas->active_point] = old_x;
      canvas->active_data->y[canvas->active_point] = old_y;
    }

    return;
  }


  if(markers){
     gdk_draw_rectangle (GTK_WIDGET(canvas)->window,
                         xor_gc,
                         FALSE,
                         area.x, area.y,
                         area.width, area.height);         
     draw_marker(canvas, xor_gc, area.x, area.y); 
     draw_marker(canvas, xor_gc, area.x, area.y + area.height); 
     draw_marker(canvas, xor_gc, area.x + area.width, area.y); 
     draw_marker(canvas, xor_gc, area.x + area.width, area.y + area.height); 
     if(area.height > DEFAULT_MARKER_SIZE * 2){
       draw_marker(canvas, xor_gc, area.x, area.y + area.height / 2); 
       draw_marker(canvas, xor_gc, area.x + area.width, 
                                   area.y + area.height / 2); 
     }
     if(area.width > DEFAULT_MARKER_SIZE * 2){
       draw_marker(canvas, xor_gc, area.x + area.width / 2, area.y); 
       draw_marker(canvas, xor_gc, area.x + area.width / 2, 
                                   area.y + area.height); 
     }
  } else {
     gdk_gc_set_line_attributes(xor_gc, 1, 1, 0 ,0 );

     gdk_draw_rectangle (GTK_WIDGET(canvas)->window,
                         xor_gc,
                         FALSE,
                         area.x, area.y,
                         area.width, area.height);         
  }
  gdk_gc_unref(xor_gc);
}

static void
draw_marker(GtkPlotCanvas *canvas, GdkGC *gc, gint x, gint y)
{
  GdkDrawable *darea;

  darea = GTK_WIDGET(canvas)->window;

  gdk_draw_rectangle(darea, gc, TRUE,
                     x - DEFAULT_MARKER_SIZE / 2, y - DEFAULT_MARKER_SIZE / 2,
                     DEFAULT_MARKER_SIZE + 1, DEFAULT_MARKER_SIZE + 1);
}

static GtkPlotCanvasPos
posible_selection(GdkRectangle area, gint x, gint y)
{
  GtkPlotCanvasPos return_value = GTK_PLOT_CANVAS_OUT;

  if(x >= area.x - DEFAULT_MARKER_SIZE / 2 &&
     x <= area.x + DEFAULT_MARKER_SIZE / 2){
       if(y >= area.y - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_TOP_LEFT;
       if(y >= area.y + area.height - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_BOTTOM_LEFT;
       if(y >= area.y + area.height / 2 - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height / 2 + DEFAULT_MARKER_SIZE / 2. &&
          area.height > DEFAULT_MARKER_SIZE * 2)
                      return_value = GTK_PLOT_CANVAS_LEFT;
  }

  if(x >= area.x + area.width - DEFAULT_MARKER_SIZE / 2 &&
     x <= area.x + area.width + DEFAULT_MARKER_SIZE / 2){
       if(y >= area.y - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_TOP_RIGHT;
       if(y >= area.y + area.height - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_BOTTOM_RIGHT;
       if(y >= area.y + area.height / 2 - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height / 2 + DEFAULT_MARKER_SIZE / 2. &&
          area.height > DEFAULT_MARKER_SIZE * 2)
                      return_value = GTK_PLOT_CANVAS_RIGHT;
  }

  if(x >= area.x + area.width / 2 - DEFAULT_MARKER_SIZE / 2 &&
     x <= area.x + area.width / 2 + DEFAULT_MARKER_SIZE / 2 &&
     area.width > DEFAULT_MARKER_SIZE * 2){
       if(y >= area.y - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_TOP;
       if(y >= area.y + area.height - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_BOTTOM;
  }

  if(return_value == GTK_PLOT_CANVAS_OUT){
     if (x >= area.x && x <= area.x + area.width &&
         y >= area.y && y <= area.y + area.height)
                      return_value = GTK_PLOT_CANVAS_IN;
  }

  return (return_value);
}    

/**********************************************************************/

void
gtk_plot_canvas_grid_set_visible(GtkPlotCanvas *canvas, gboolean visible)
{
  canvas->show_grid= visible;
}

void
gtk_plot_canvas_grid_set_step(GtkPlotCanvas *canvas, gint step)
{
  canvas->grid_step = step;
}

void
gtk_plot_canvas_grid_set_attributes(GtkPlotCanvas *canvas,
		 	            GtkPlotLineStyle style,
			            gint width,
			            GdkColor *color)
{
  if(color)
      canvas->grid.color = *color;
  canvas->grid.line_width = width;
  canvas->grid.line_style = style;
}

/**********************************************************************/
GtkPlotCanvasChild *
gtk_plot_canvas_put_text (GtkPlotCanvas *canvas,
                          gdouble x, gdouble y, gint angle,
                          const gchar *font, gint height,
                          GdkColor *fg, GdkColor *bg,
			  gboolean transparent,
                          GtkJustification justification,
                          const gchar *text)
{
  GtkWidget *widget;
  GtkPlotText *text_attr;
  GtkPlotCanvasChild *child;

  widget = GTK_WIDGET(canvas);

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_TEXT);

  text_attr = (GtkPlotText *)child->data;

  text_attr->x = x;
  text_attr->y = y;
  text_attr->angle = angle;
  text_attr->fg = widget->style->black;
  text_attr->bg = widget->style->white;
  text_attr->justification = justification;
  text_attr->transparent = transparent;

  if(!font) {
    text_attr->font = g_strdup(DEFAULT_FONT);
    text_attr->height = DEFAULT_FONT_HEIGHT;
  } else {
    text_attr->font = g_strdup(font);
    text_attr->height = height;
  }

  text_attr->text = NULL;
  if(text) text_attr->text = g_strdup(text);

  if(fg != NULL)
    text_attr->fg = *fg;

  if(bg != NULL)
    text_attr->bg = *bg;


  canvas->childs = g_list_append(canvas->childs, child);
  gtk_plot_canvas_draw_text(canvas, child);

  return child;
}

GtkPlotCanvasChild *
gtk_plot_canvas_put_line(GtkPlotCanvas *canvas,
                         gdouble x1, gdouble y1, 
                         gdouble x2, gdouble y2, 
		 	 GtkPlotLineStyle style,
			 gint width,
			 GdkColor *color,
			 GtkPlotCanvasArrow arrow_mask)
{
  GtkPlotCanvasChild *child;
  GtkPlotCanvasLine *line;

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_LINE);
  line = (GtkPlotCanvasLine *)child->data;

  gdk_color_black(gdk_colormap_get_system(), &line->line.color); 
  
  line->arrow_length = ARROW_LENGTH;

  gtk_plot_canvas_line_set_attributes(child, style, width, color, arrow_mask);

  gtk_plot_canvas_put_child(canvas, child, x1, y1, x2, y2);
  return child;
}

GtkPlotCanvasChild *
gtk_plot_canvas_put_rectangle(GtkPlotCanvas *canvas,
                              gdouble x1, gdouble y1, 
                              gdouble x2, gdouble y2, 
		  	      GtkPlotLineStyle style,
			      gint width,
			      GdkColor *fg,
			      GdkColor *bg,
			      GtkPlotBorderStyle border,
			      gboolean fill)
{
  GtkPlotCanvasChild *child;
  GtkPlotCanvasRectangle *rectangle;

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_RECTANGLE);
  rectangle = (GtkPlotCanvasRectangle *)child->data;
 
  gdk_color_black(gdk_colormap_get_system(), &rectangle->line.color); 
  gdk_color_white(gdk_colormap_get_system(), &rectangle->bg); 

  gtk_plot_canvas_rectangle_set_attributes(child, style, width, 
                                           fg, bg, border, fill);
  rectangle->shadow_width = SHADOW_WIDTH;

  gtk_plot_canvas_put_child(canvas, child, x1, y1, x2, y2);
  return child;
}

GtkPlotCanvasChild *
gtk_plot_canvas_put_ellipse(GtkPlotCanvas *canvas,
                            gdouble x1, gdouble y1, 
                            gdouble x2, gdouble y2, 
		  	    GtkPlotLineStyle style,
			    gint width,
			    GdkColor *fg,
			    GdkColor *bg,
			    gboolean fill)
{
  GtkPlotCanvasChild *child;
  GtkPlotCanvasEllipse *ellipse;

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_ELLIPSE);
  ellipse = (GtkPlotCanvasEllipse *)child->data;
 
  gdk_color_black(gdk_colormap_get_system(), &ellipse->line.color); 
  gdk_color_white(gdk_colormap_get_system(), &ellipse->bg); 

  gtk_plot_canvas_ellipse_set_attributes(child, style, width, 
                                         fg, bg, fill);

  gtk_plot_canvas_put_child(canvas, child, x1, y1, x2, y2);
  return child;
}

void
gtk_plot_canvas_line_set_attributes(GtkPlotCanvasChild *child,
		 	            GtkPlotLineStyle style,
			            gint width,
			            GdkColor *color,
			            GtkPlotCanvasArrow mask)
{
  GtkPlotCanvasLine *line;

  g_return_if_fail(child->type == GTK_PLOT_CANVAS_LINE);

  line = (GtkPlotCanvasLine *)child->data;  
  if(color)
      line->line.color = *color;
  line->line.line_width = width;
  line->line.line_style = style;
  line->arrow_mask = mask;
}

void
gtk_plot_canvas_rectangle_set_attributes(GtkPlotCanvasChild *child,
		  	                 GtkPlotLineStyle style,
			                 gint width,
			                 GdkColor *fg,
			                 GdkColor *bg,
				         GtkPlotBorderStyle border,
			                 gboolean fill)
{
  GtkPlotCanvasRectangle *rectangle;

  g_return_if_fail(child->type == GTK_PLOT_CANVAS_RECTANGLE);

  rectangle = (GtkPlotCanvasRectangle *)child->data;  
  if(fg)
    rectangle->line.color = *fg;
  if(bg)
    rectangle->bg = *bg;
  rectangle->line.line_width = width;
  rectangle->line.line_style = style;
  rectangle->filled = fill;
  rectangle->border = border;
}

void
gtk_plot_canvas_ellipse_set_attributes(GtkPlotCanvasChild *child,
		  	               GtkPlotLineStyle style,
			               gint width,
			               GdkColor *fg,
			               GdkColor *bg,
			               gboolean fill)
{
  GtkPlotCanvasEllipse *ellipse;

  g_return_if_fail(child->type == GTK_PLOT_CANVAS_ELLIPSE);

  ellipse = (GtkPlotCanvasEllipse *)child->data;  
  if(fg)
    ellipse->line.color = *fg;
  if(bg)
    ellipse->bg = *bg;
  ellipse->line.line_width = width;
  ellipse->line.line_style = style;
  ellipse->filled = fill;
}


GtkPlotCanvasChild *
gtk_plot_canvas_child_new(GtkPlotCanvasType type)
{
  GtkPlotCanvasChild *child;
  child = g_new(GtkPlotCanvasChild, 1);
  child->type = type;

  child->flags = GTK_PLOT_CANVAS_CAN_MOVE | 
                 GTK_PLOT_CANVAS_CAN_X_RESIZE |
                 GTK_PLOT_CANVAS_CAN_Y_RESIZE;

  child->min_width = -1;
  child->min_height = -1;

  switch(type){
    case GTK_PLOT_CANVAS_LINE:
	child->data = g_new(GtkPlotCanvasLine, 1);
        break;    
    case GTK_PLOT_CANVAS_RECTANGLE:
	child->data = g_new(GtkPlotCanvasRectangle, 1);
        break;    
    case GTK_PLOT_CANVAS_ELLIPSE:
	child->data = g_new(GtkPlotCanvasEllipse, 1);
        break;    
    case GTK_PLOT_CANVAS_TEXT:
	child->data = g_new(GtkPlotText, 1);
        child->flags = GTK_PLOT_CANVAS_CAN_MOVE;
        break;    
    case GTK_PLOT_CANVAS_CUSTOM:
        child->flags = GTK_PLOT_CANVAS_CAN_MOVE;
        child->data = NULL;
    default:
	break;
  }
  
  child->draw_child = NULL;
  child->print_child = NULL;
  return child;
}

void
gtk_plot_canvas_put_child(GtkPlotCanvas *canvas, 
                          GtkPlotCanvasChild *child,
			  gdouble x1, gdouble y1, 
                          gdouble x2, gdouble y2)
{
  child->rx1 = x1;
  child->ry1 = y1;
  child->rx2 = x2;
  child->ry2 = y2;

  canvas->childs = g_list_append(canvas->childs, child);
  gtk_plot_canvas_draw_child(canvas, child);
}

void
gtk_plot_canvas_child_move(GtkPlotCanvas *canvas, 
                           GtkPlotCanvasChild *child,
			   gdouble x1, gdouble y1) 
{
  child->rx2 += (x1 - child->rx1);
  child->ry2 += (y1 - child->ry1);
  child->rx1 = x1;
  child->ry1 = y1;

  gtk_plot_canvas_refresh(canvas);
}

void
gtk_plot_canvas_child_move_resize(GtkPlotCanvas *canvas, 
                                  GtkPlotCanvasChild *child,
			          gdouble x1, gdouble y1,
				  gdouble x2, gdouble y2) 
{
  child->rx1 = x1;
  child->ry1 = y1;
  child->rx2 = x2;
  child->ry2 = y2;

  gtk_plot_canvas_refresh(canvas);
}

gboolean
gtk_plot_canvas_remove_child(GtkPlotCanvas *canvas, 
                             GtkPlotCanvasChild *child)
{
  GList *list;
  gpointer data;

  list = canvas->childs;

  while(list)
   {
     data = list->data;

     if((GtkPlotCanvasChild *)data == child){
              g_free(child->data);
              g_free(child);

              canvas->childs = g_list_remove_link(canvas->childs, list);
              g_list_free_1(list);
              return TRUE;
     }
     list = list->next;
   }

  return FALSE;

}


/**********************************************************************/
static void
gtk_plot_canvas_draw_child(GtkPlotCanvas *canvas,
                           GtkPlotCanvasChild *child)
{
  GtkPlotCanvasLine *line = NULL;
  GtkPlotCanvasRectangle *rectangle = NULL;
  GtkPlotCanvasEllipse *ellipse = NULL;
  GtkPlotText *text = NULL;
  GdkPoint arrow[3];
  GdkGC *gc;
  gint rx1 = 0, ry1 = 0, rx2 = 0, ry2 = 0;
  gint xmin, xmax, ymin, ymax;
  gint width = 0, height = 0;
  gint xm = 0, ym = 0;
  gdouble angle = 0.;
  gdouble m = canvas->magnification;
  gint arrow_width = ARROW_LENGTH / 3;

  if(!canvas->pixmap) return;

  gc = gdk_gc_new(canvas->pixmap);

  gtk_plot_canvas_get_pixel(canvas, child->rx1, child->ry1, &rx1, &ry1);
  gtk_plot_canvas_get_pixel(canvas, child->rx2, child->ry2, &rx2, &ry2);

  xmin = MIN(rx1, rx2);
  xmax = MAX(rx1, rx2);
  ymin = MIN(ry1, ry2);
  ymax = MAX(ry1, ry2);

  width = abs(rx2 - rx1);
  height = abs(ry2 - ry1);

  child->allocation.x = xmin;
  child->allocation.y = ymin;
  child->allocation.width = width;
  child->allocation.height = height;

  switch(child->type){
    case GTK_PLOT_CANVAS_LINE:
        line = (GtkPlotCanvasLine *)child->data;

        if(width == 0 && height == 0) return;
        if(width != 0) 
            angle = atan2((gdouble)(ry2 - ry1), (gdouble)(rx2 - rx1));  
        else   
            angle = asin((ry2 - ry1)/height);

        arrow_width = line->arrow_length / 2;
        gtk_plot_canvas_set_line_attributes(line->line, gc);
        gdk_draw_line(canvas->pixmap, gc, rx1, ry1, rx2, ry2);
        if(line->arrow_mask & GTK_PLOT_ARROW_END){
           arrow[1].x = rx2;
           arrow[1].y = ry2;
           xm = rx2 - roundint(cos(angle) * line->arrow_length * m);
           ym = ry2 - roundint(sin(angle) * line->arrow_length * m);
           arrow[0].x = xm + roundint(sin(angle)* arrow_width * m);
           arrow[0].y = ym - roundint(cos(angle)* arrow_width * m);
           arrow[2].x = xm - roundint(sin(angle)* arrow_width * m);
           arrow[2].y = ym + roundint(cos(angle)* arrow_width * m);
           gdk_draw_polygon (canvas->pixmap, gc, TRUE, arrow, 3);
        }
        if(line->arrow_mask & GTK_PLOT_ARROW_ORIGIN){
           arrow[1].x = rx1;
           arrow[1].y = ry1;
           xm = rx1 + roundint(cos(angle) * line->arrow_length * m);
           ym = ry1 + roundint(sin(angle) * line->arrow_length * m);
           arrow[0].x = xm + roundint(sin(angle)* arrow_width * m);
           arrow[0].y = ym - roundint(cos(angle)* arrow_width * m);
           arrow[2].x = xm - roundint(sin(angle)* arrow_width * m);
           arrow[2].y = ym + roundint(cos(angle)* arrow_width * m);
           gdk_draw_polygon (canvas->pixmap, gc, TRUE, arrow, 3);
        }
        break;    
    case GTK_PLOT_CANVAS_RECTANGLE:
        rectangle = (GtkPlotCanvasRectangle *)child->data;
        if(rectangle->filled){
           gdk_gc_set_foreground(gc, &rectangle->bg);
           gdk_draw_rectangle(canvas->pixmap, gc, TRUE, 
                              xmin, ymin, width, height);
        }
        if(rectangle->line.line_style != GTK_PLOT_LINE_NONE &&
           rectangle->border != GTK_PLOT_BORDER_NONE){

            gtk_plot_canvas_set_line_attributes(rectangle->line, gc);
            gdk_draw_rectangle(canvas->pixmap, gc, FALSE, 
                               xmin, ymin, width, height);
            if(rectangle->border == GTK_PLOT_BORDER_SHADOW){
              gdk_draw_rectangle(canvas->pixmap,
                                 gc,
                                 TRUE,
                                 xmin + roundint(rectangle->shadow_width * m),
                                 ymin + height,
                                 width, roundint(rectangle->shadow_width * m));
              gdk_draw_rectangle(canvas->pixmap,
                                 gc,
                                 TRUE,
                                 xmin + width,
                                 ymin + roundint(rectangle->shadow_width * m),
                                 roundint(rectangle->shadow_width * m), height);
            }
        }
        break;    
    case GTK_PLOT_CANVAS_ELLIPSE:
        ellipse = (GtkPlotCanvasEllipse *)child->data;
        if(ellipse->filled){
           gdk_gc_set_foreground(gc, &ellipse->bg);
           gdk_draw_arc(canvas->pixmap, gc, TRUE, 
                        xmin, ymin, width, height, 0, 25000);
        }
        gtk_plot_canvas_set_line_attributes(ellipse->line, gc);
        if(ellipse->line.line_style != GTK_PLOT_LINE_NONE)
          gdk_draw_arc(canvas->pixmap, gc, FALSE, 
                        xmin, ymin, width, height, 0, 25000);
        break;    
        break;    
    case GTK_PLOT_CANVAS_TEXT:
        text = (GtkPlotText *)child->data;
        gtk_plot_canvas_draw_text(canvas, child);
        break;
    case GTK_PLOT_CANVAS_CUSTOM:
    default:
        if(child->draw_child) child->draw_child(canvas, child);
        break;
  }

  gdk_gc_unref(gc);
}

/**********************************************************************/

static void
gtk_plot_canvas_draw_text(GtkPlotCanvas *canvas,
                          GtkPlotCanvasChild *child)
{
  GtkPlotText *text;
  GdkPixmap *text_pixmap;
  GdkBitmap *text_mask;
  GdkGC *gc;
  GdkColormap *colormap;
  gint x, y;
  gint width, height;
  gint ascent, descent;
  gdouble m = canvas->magnification;
  gint tx, ty, twidth, theight;

  if(canvas->pixmap == NULL) return;

  text = (GtkPlotText *)child->data;

  x = text->x * canvas->pixmap_width;
  y = text->y * canvas->pixmap_height;

  gtk_plot_text_get_size(text, m, &width, &height, &ascent, &descent);

  gtk_plot_text_get_area(text, m, &tx, &ty, &twidth, &theight);

  tx += x;
  ty += y;
  gtk_plot_canvas_get_position(canvas, tx, ty, 
                               &child->rx1, &child->ry1);
  gtk_plot_canvas_get_position(canvas, tx + twidth, ty + theight, 
                               &child->rx2, &child->ry2);

  switch(text->justification){
    case GTK_JUSTIFY_LEFT:
      switch(text->angle){
        case 0:
            y -= ascent;
            break;
        case 90:
            y -= height;
            x -= ascent;
            break;
        case 180:
            x -= width;
            y -= descent;
            break;
        case 270:
            x -= descent;
            break;
      }
      break;
    case GTK_JUSTIFY_RIGHT:
      switch(text->angle){
        case 0:
            x -= width;
            y -= ascent;
            break;
        case 90:
            x -= ascent;
            break;
        case 180:
            y -= descent;
            break;
        case 270:
            y -= height;
            x -= descent;
            break;
      }
      break;
    case GTK_JUSTIFY_CENTER:
    default:
      switch(text->angle){
        case 0:
            x -= width / 2.;
            y -= ascent;
            break;
        case 90:
            x -= ascent;
            y -= height / 2.;
            break;
        case 180:
            x -= width / 2.;
            y -= descent;
            break;
        case 270:
            x -= descent;
            y -= height / 2.;
            break;
      }
  }

  rotate_text(canvas, *text, &width, &height, &text_pixmap, &text_mask);

  colormap = gtk_widget_get_colormap (GTK_WIDGET(canvas));
  gc = gdk_gc_new(canvas->pixmap);

  if(text->transparent){
    gdk_gc_set_clip_mask (gc, text_mask);
    gdk_gc_set_clip_origin (gc, x, y);
  }


  gdk_draw_pixmap(canvas->pixmap, gc,
                  text_pixmap, 0, 0,
                  x, y, -1, -1);

  gdk_draw_pixmap(GTK_WIDGET(canvas)->window,
                  GTK_WIDGET(canvas)->style->fg_gc[GTK_STATE_NORMAL],
                  canvas->pixmap,
                  x, y,
                  x, y,
                  width, height);

  gdk_pixmap_unref(text_pixmap);
  gdk_bitmap_unref(text_mask);
  gdk_gc_unref(gc);
}

static void
rotate_text(GtkPlotCanvas *canvas,
            GtkPlotText text,
            gint *width, gint *height,
            GdkPixmap **new_pixmap, GdkBitmap **mask)
{
  GdkWindow *window;
  GdkPixmap *old_pixmap;
  GdkImage *image;
  GdkGC *gc, *mask_gc;
  GdkColormap *colormap;
  GdkColorContext *cc;
  GdkVisual *visual;
  GdkColor color, black, mask_color;
  GdkFont *font;
  GtkPSFont *psfont, *tmp_font;
  GList *family = NULL;
  gint x, y, y0;
  gint old_width, old_height;
  gboolean bold, italic;
  gchar *aux, subs[2];
  gint fontsize;
  gint ascent, descent;
  gint numf;
  gint xp = 0, yp = 0;
  gchar *lastchar = NULL;
  gdouble m = canvas->magnification;

  window = GTK_WIDGET(canvas)->window;
  colormap = gtk_widget_get_colormap (GTK_WIDGET(canvas));
  visual = gtk_widget_get_visual (GTK_WIDGET(canvas));
  cc = gdk_color_context_new(visual, colormap);
  gc = gdk_gc_new (window);

  gtk_plot_text_get_size(&text, m, width, height, &ascent, &descent);
  old_width = *width;
  old_height = *height;
  if(text.angle == 90 || text.angle == 270)
    {
      old_width = *height;
      old_height = *width;
    }

  gtk_psfont_get_families(&family, &numf);
  font = gtk_psfont_get_gdkfont(text.font, roundint(text.height * m));
  psfont = gtk_psfont_get_font(text.font);
  tmp_font = psfont;
  italic = psfont->italic;
  bold = psfont->bold;
  fontsize = roundint(text.height * m);
  x = 0;
  y0 = y = ascent;
  aux = text.text;

  old_pixmap = gdk_pixmap_new(window, old_width, old_height, -1);
  gdk_color_white (colormap, &color);
  gdk_gc_set_foreground(gc, &color);
  gdk_draw_rectangle(old_pixmap, gc, TRUE,
                     0, 0, -1, -1);
  gdk_color_black (colormap, &black);
  gdk_gc_set_foreground(gc, &black);

  while(aux && *aux != '\0' && *aux != '\n'){
   if(*aux == '\\'){
     aux++;
     switch(*aux){
       case '0': case '1': case '2': case '3':
       case '4': case '5': case '6': case '7': case '9':
           tmp_font = gtk_psfont_find_by_family((gchar *)g_list_nth_data(family, atoi(aux)), italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '8': case 'g':
           tmp_font = gtk_psfont_find_by_family("Symbol", italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'B':
           bold = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'i':
           italic = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'S': case '^':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y -= font->ascent;
           aux++;
           break;
       case 's': case '_':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y += font->descent;
           aux++;
           break;
       case '+':
           fontsize += 3;
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '-':
           fontsize -= 3;
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'N':
           tmp_font = psfont;
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, text.height);
           y = y0;
           italic = psfont->italic;
           bold = psfont->bold;
           fontsize = text.height;
           aux++;
           break;
       case 'b':
           if(lastchar){
              x -= gdk_char_width_wc (font, *lastchar);
              if(lastchar == text.text)
                 lastchar = NULL;
              else
                 lastchar--;
           } else {
              x -= gdk_char_width_wc (font, 'X');
           }
           aux++;
           break;
       default:
           if(aux && *aux != '\0' && *aux !='\n'){
             subs[0] = *aux;
             subs[1] = '\0';
             gdk_draw_string (old_pixmap, font,
                              gc,
                              x, y,
                              subs);
             x += gdk_char_width_wc (font, *aux);
             lastchar = aux;
             aux++;
           }
           break;
     }
   } else {
     if(aux && *aux != '\0' && *aux !='\n'){
       subs[0] = *aux;
       subs[1] = '\0';
       gdk_draw_string (old_pixmap, font,
                        gc,
                        x, y,
                        subs);

       x += gdk_char_width_wc (font, *aux);
       lastchar = aux;
       aux++;
     }
   }
  }

  image = gdk_image_get(old_pixmap, 0, 0, old_width, old_height);

  *new_pixmap = gdk_pixmap_new(window, *width, *height, -1);
  gdk_gc_set_foreground(gc, &text.bg);
  gdk_draw_rectangle(*new_pixmap, gc, TRUE,
                     0, 0, -1, -1);

  *mask = gdk_pixmap_new(window, *width, *height, 1);
  mask_gc = gdk_gc_new(*mask);
  mask_color.pixel = 0;
  gdk_gc_set_foreground(mask_gc, &mask_color);
  gdk_draw_rectangle(*mask, mask_gc, TRUE,
                     0, 0, -1, -1);

  mask_color.pixel = 1;

  gdk_gc_set_foreground(gc, &text.fg);
  gdk_gc_set_foreground(mask_gc, &mask_color);

  for(y = 0; y < old_height; y++)
    for(x = 0; x < old_width; x++)
       {
         color.pixel = gdk_image_get_pixel(image, x, y);
         gdk_color_context_query_color(cc, &color);
         if(gdk_color_equal(&color, &black)){
         switch(text.angle){
          case 0:
              xp = x;
              yp = y;
              break;
          case 90:
              xp = y;
              yp = old_width - x;
              break;
          case 180:
              xp = old_width - x;
              yp = old_height - y;
              break;
          case 270:
              xp = old_height - y;
              yp = x;
              break;
          }
          gdk_draw_point(*new_pixmap, gc, xp, yp);
          gdk_draw_point(*mask, mask_gc, xp, yp);
         }
       }

  gdk_font_unref(font);
  gdk_gc_unref(gc);
  gdk_gc_unref(mask_gc);
  gdk_color_context_free(cc);
  gdk_image_destroy(image);
  gdk_pixmap_unref(old_pixmap);

  return;
}

static gint
roundint (gdouble x)
{
 gint sign = 1;

/* if(x <= 0.) sign = -1;
*/
 return (x+sign*.50999999471);

}

static void
gtk_plot_canvas_set_line_attributes(GtkPlotLine line,
                                    GdkGC *gc)
{
  GdkGCValues values;

  gdk_gc_get_values(gc, &values);
  if(values.function != GDK_XOR && values.function != GDK_INVERT)
           gdk_gc_set_foreground (gc, &line.color);

  switch(line.line_style){
   case GTK_PLOT_LINE_SOLID:
        gdk_gc_set_line_attributes(gc, line.line_width, 0, 0, 0);
        break;
   case GTK_PLOT_LINE_DOTTED:
        gdk_gc_set_dashes(gc, 0,"\2\3", 2);
        gdk_gc_set_line_attributes(gc, line.line_width,
                                   GDK_LINE_ON_OFF_DASH, 0, 0);
        break;
   case GTK_PLOT_LINE_DASHED:
        gdk_gc_set_dashes(gc, 0,"\6\4", 2);
        gdk_gc_set_line_attributes(gc, line.line_width,
                                   GDK_LINE_ON_OFF_DASH, 0, 0);
        break;
   case GTK_PLOT_LINE_DOT_DASH:
        gdk_gc_set_dashes(gc, 0,"\6\4\2\4", 4);
        gdk_gc_set_line_attributes(gc, line.line_width,
                                   GDK_LINE_ON_OFF_DASH, 0, 0);
        break;
   case GTK_PLOT_LINE_DOT_DOT_DASH:
        gdk_gc_set_dashes(gc, 0,"\6\4\2\4\2\4", 6);
        gdk_gc_set_line_attributes(gc, line.line_width,
                                   GDK_LINE_ON_OFF_DASH, 0, 0);
        break;
   case GTK_PLOT_LINE_DOT_DASH_DASH:
        gdk_gc_set_dashes(gc, 0,"\6\4\6\4\2\4", 6);
        gdk_gc_set_line_attributes(gc, line.line_width,
                                   GDK_LINE_ON_OFF_DASH, 0, 0);
        break;
   case GTK_PLOT_LINE_NONE:
   default:
        break;
  } 
}

