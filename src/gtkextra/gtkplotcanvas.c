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
#include "gtkplotlayout.h"
#include "gtkplotcanvas.h"

#define DEFAULT_MARKER_SIZE 6

static void gtk_plot_canvas_class_init 		(GtkPlotCanvasClass *class);
static void gtk_plot_canvas_init 		(GtkPlotCanvas *plot_canvas);
static void gtk_plot_canvas_finalize 		(GtkObject *object);
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

/* Drawing functions */
static void gtk_plot_canvas_get_pixel           (GtkWidget *widget,
                                                 gdouble px, gdouble py,
                                                 gint *x, gint *y);
static void draw_selection 			(GtkPlotCanvas *canvas, 
                				 GdkRectangle area,
                				 gboolean markers);
static void draw_marker				(GtkPlotCanvas *canvas, 
						 GdkGC *gc, gint x, gint y);
static GtkPlotCanvasPos posible_selection	(GdkRectangle area, 
						 gint x, gint y);

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

guint
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

      plot_canvas_type = gtk_type_unique (gtk_plot_layout_get_type(), &plot_canvas_info);
    }
  return plot_canvas_type;
}

static void
gtk_plot_canvas_class_init (GtkPlotCanvasClass *class)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_layout_get_type ());

  object_class = (GtkObjectClass *) class;
  widget_class = (GtkWidgetClass *) class;

  canvas_signals[SELECT_ITEM] =
    gtk_signal_new ("select_item",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, select_item),
                    gtk_plot_canvas_marshal_select_item,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_GDK_EVENT, GTK_TYPE_POINTER);

  canvas_signals[MOVE_ITEM] =
    gtk_signal_new ("move_item",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, move_item),
                    gtk_plot_canvas_marshal_move_resize,
                    GTK_TYPE_BOOL, 3, GTK_TYPE_POINTER, 
                    GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE);

  canvas_signals[RESIZE_ITEM] =
    gtk_signal_new ("resize_item",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, resize_item),
                    gtk_plot_canvas_marshal_move_resize,
                    GTK_TYPE_BOOL, 3, GTK_TYPE_POINTER,
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

  widget_class->focus_in_event = gtk_plot_canvas_focus_in;
  widget_class->focus_out_event = gtk_plot_canvas_focus_out;
  widget_class->motion_notify_event = gtk_plot_canvas_motion;
  widget_class->button_press_event = gtk_plot_canvas_button_press;
  widget_class->button_release_event = gtk_plot_canvas_button_release;

  class->move_item = NULL;
  class->resize_item = NULL;
  class->select_item = NULL;
  class->select_region = NULL;
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
  widget = GTK_WIDGET(plot_canvas);

  GTK_WIDGET_SET_FLAGS(widget, GTK_CAN_FOCUS);

  plot_canvas->flags = 0;
  plot_canvas->state = GTK_STATE_NORMAL;
  plot_canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;

  plot_canvas->active_plot = NULL;
  plot_canvas->active_data = NULL;
  plot_canvas->active_point = -1;

  plot_canvas->drag_x = plot_canvas->drag_y = 0;
  plot_canvas->pointer_x = plot_canvas->pointer_y = 0;
}

GtkWidget*
gtk_plot_canvas_new (gint width, gint height)
{
  GtkPlotCanvas *plot_canvas;

  plot_canvas = gtk_type_new (gtk_plot_canvas_get_type ());
  GTK_PLOT_LAYOUT(plot_canvas)->width = width;
  GTK_PLOT_LAYOUT(plot_canvas)->height = height;
  return GTK_WIDGET (plot_canvas);
}


static void
gtk_plot_canvas_finalize (GtkObject *object)
{
  GtkPlotCanvas *plot_canvas;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT_CANVAS (object));

  plot_canvas = GTK_PLOT_CANVAS (object);

  if (GTK_OBJECT_CLASS (parent_class)->finalize)
    (*GTK_OBJECT_CLASS (parent_class)->finalize) (object);
}


static gint 
gtk_plot_canvas_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GtkPlotCanvas *canvas;
  GtkPlot *active_plot;
  GtkPlotData *active_dataset;
  gint active_point;
  GtkAllocation *allocation;
  GdkRectangle area;
  gint x, y, dx, dy;
  gint new_x, new_y;
  gint new_width, new_height;

  canvas = GTK_PLOT_CANVAS(widget);
  active_plot = canvas->active_plot;
  active_dataset = canvas->active_data;
  active_point = canvas->active_point;
 
  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) return TRUE;
  if(active_plot == NULL) return TRUE;

  allocation = &GTK_WIDGET(canvas->active_plot)->allocation;

  gtk_widget_get_pointer(widget, &x, &y);

  area = canvas->active_item.area;
  new_x = area.x;
  new_y = area.y;
  new_width = area.width;
  new_height = area.height;

  switch(canvas->action){
     case GTK_PLOT_CANVAS_ACTION_DRAG:
       if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_MOVE){
         draw_selection(canvas, canvas->active_item.new_area, TRUE);
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         dx = x - canvas->drag_x;
         dy = y - canvas->drag_y;
         area.x = canvas->active_item.area.x + dx; 
         area.y = canvas->active_item.area.y + dy;
         draw_selection(canvas, area, TRUE);
         canvas->active_item.new_area = area;
       }
       break;
     case GTK_PLOT_CANVAS_ACTION_RESIZE:
       switch(canvas->drag_point){
            case GTK_PLOT_CANVAS_TOP_LEFT: 
            case GTK_PLOT_CANVAS_TOP_RIGHT:
            case GTK_PLOT_CANVAS_TOP:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_Y_RESIZE){
                       dy = y - canvas->drag_y;
                       new_y = canvas->active_item.area.y + dy;
                       new_height = canvas->active_item.area.height - dy;
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_TOP_LEFT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_x = canvas->active_item.area.x + dx;
                          new_width = canvas->active_item.area.width - dx;
                  }
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_TOP_RIGHT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_width = canvas->active_item.area.width + dx;
                  }
               }
               break;
            case GTK_PLOT_CANVAS_BOTTOM_LEFT:
            case GTK_PLOT_CANVAS_BOTTOM_RIGHT:
            case GTK_PLOT_CANVAS_BOTTOM:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_Y_RESIZE){
                       dy = y - canvas->drag_y;
                       new_height = canvas->active_item.area.height + dy;
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_BOTTOM_LEFT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_x = canvas->active_item.area.x + dx;
                          new_width = canvas->active_item.area.width - dx;
                  }
               }
               if(canvas->drag_point == GTK_PLOT_CANVAS_BOTTOM_RIGHT){
                  if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                          dx = x - canvas->drag_x;
                          new_width = canvas->active_item.area.width + dx;
                  }
               }
               break;
            case GTK_PLOT_CANVAS_LEFT:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                       dx = x - canvas->drag_x;
                       new_x = canvas->active_item.area.x + dx;
                       new_width = canvas->active_item.area.width - dx;
               }
               break;
            case GTK_PLOT_CANVAS_RIGHT:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                       dx = x - canvas->drag_x;
                       new_width = canvas->active_item.area.width + dx;
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
                draw_selection(canvas, canvas->active_item.new_area, TRUE);
                area.x = new_x;
                area.y = new_y;
                area.width = new_width;
                area.height = new_height;
                draw_selection(canvas, area, TRUE);
                canvas->active_item.new_area = area;
       }
       break;
     case GTK_PLOT_CANVAS_ACTION_SELECTION:
         draw_selection(canvas, canvas->active_item.new_area, FALSE);
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         area.x = MIN(canvas->pointer_x, canvas->drag_x); 
         area.y = MIN(canvas->pointer_y, canvas->drag_y); 
         area.width = abs(x - canvas->drag_x);
         area.height = abs(y - canvas->drag_y);
         canvas->active_item.new_area = area;
         draw_selection(canvas, canvas->active_item.new_area, FALSE);
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
  GtkPlotCanvasItem active_item;
  GtkPlot *active_plot = NULL;
  GList *plots = NULL;
  GList *dataset = NULL;
  GList *text = NULL;
  GtkPlot *plot = NULL;
  GtkPlot *click_plot = NULL;
  GtkPlotData *data = NULL;
  GtkPlotText *child_text = NULL;
  GtkAllocation internal_allocation;
  GtkAllocation legends_allocation;
  GdkRectangle area;
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

  gdk_window_get_pointer(widget->window, NULL, NULL, &mods);
  if(!(mods & GDK_BUTTON1_MASK)) return FALSE;
  double_click = (event->button == GDK_2BUTTON_PRESS);
 
  canvas = GTK_PLOT_CANVAS(widget);
  active_plot = canvas->active_plot;
/*
  if(double_click && canvas->state == GTK_STATE_SELECTED) return TRUE;
*/
  gdk_pointer_ungrab(event->time);

  if(!GTK_WIDGET_HAS_FOCUS(widget)) gtk_widget_grab_focus(widget);

  gtk_widget_get_pointer(widget, &x, &y);

/**********************************************************************/
  text = GTK_PLOT_LAYOUT(canvas)->text;

  while(text)
    {
      child_text = (GtkPlotText *)text->data;
      gtk_plot_layout_get_pixel(GTK_PLOT_LAYOUT(widget), 
                                child_text->x, child_text->y,
                                &tx, &ty);

      tx -= GTK_LAYOUT(widget)->xoffset;
      ty -= GTK_LAYOUT(widget)->yoffset;

      gtk_plot_text_get_area(*child_text, &rx, &ry, &twidth, &theight); 

      area.x = tx + rx;
      area.y = ty + ry;
      area.width = twidth;
      area.height = theight;

      if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
          active_item.data = text->data;
          active_item.area = area;
          active_item.type = GTK_PLOT_CANVAS_TEXT;
          active_item.state = GTK_STATE_SELECTED;
          active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
          state = GTK_STATE_SELECTED;
          new_selection = TRUE;
          new_pos = pos;
      }

      text = text->next;
    }

/**********************************************************************/
  plots = GTK_PLOT_LAYOUT(canvas)->plots;
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
            gtk_plot_canvas_get_pixel(GTK_WIDGET(plot), 
                                      child_text->x, child_text->y,
                                      &tx, &ty);
            gtk_plot_text_get_area(*child_text, &rx, &ry, &twidth, &theight);

            area.x = tx + rx;
            area.y = ty + ry;
            area.width = twidth;
            area.height = theight;

            if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
                active_item.data = axis[i];
                active_item.area = area;
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
 
  plots = GTK_PLOT_LAYOUT(canvas)->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;

       legends_allocation = gtk_plot_legends_get_allocation (plot);
       area.x = legends_allocation.x;
       area.y = legends_allocation.y;
       area.width = legends_allocation.width;
       area.height = legends_allocation.height;

       if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
           active_item.data = plot;
           active_item.area = area;
           active_item.type = GTK_PLOT_CANVAS_LEGENDS;
           active_item.state = GTK_STATE_SELECTED;
           active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
           state = GTK_STATE_SELECTED;
           click_plot = plot;
           new_selection = TRUE;
           new_pos = pos;
           break;
       }

       plots=plots->next;
     } 

/**********************************************************************/
  if(!new_selection){

     plots = GTK_PLOT_LAYOUT(canvas)->plots;
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
              active_item.area = area;
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
              active_item.area = area;
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
              active_item.area = area;
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
              active_item.area = area;
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
              active_item.area = area;
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
              active_item.area.x = xi;
              active_item.area.y = yi;
              active_item.area.width = 20;
              active_item.area.height = 20;
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

    if(!new_item && click_plot != canvas->active_plot) new_item = TRUE;

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
           active_item.min_width = 2 * DEFAULT_MARKER_SIZE;
           active_item.min_height = 2 * DEFAULT_MARKER_SIZE;
           canvas->active_item = active_item;
           canvas->active_item.new_area = active_item.area;
           canvas->state = GTK_STATE_SELECTED; 
           canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;

           if(active_item.type == GTK_PLOT_CANVAS_DATA){
             canvas->active_data = data;
             canvas->active_point = i;
             canvas->active_x = data->x[i];
             canvas->active_y = data->y[i];
           }

           draw_selection(canvas, active_item.area, TRUE);

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
         gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
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

  if(click_plot != NULL && GTK_PLOT_CANVAS_CAN_SELECT(canvas)){
         active_item.data = NULL;
         active_item.area.x = x;
         active_item.area.y = y;
         active_item.area.width = 0;
         active_item.area.height = 0;
         active_item.new_area = active_item.area;
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
           gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
                             GDK_POINTER_MOTION_HINT_MASK |
                             GDK_BUTTON1_MOTION_MASK |
                             GDK_BUTTON_RELEASE_MASK,
                             NULL, NULL, event->time);
           draw_selection(canvas, active_item.area, FALSE);
         }
  }

  return TRUE;  
}

 
static gint
gtk_plot_canvas_button_release(GtkWidget *widget, GdkEventButton *event)
{
  GtkPlotCanvas *canvas;
  GtkPlotText *text;
  GtkPlotAxis *axis = NULL;
  GtkPlot *active_plot;
  gint x, y;
  gdouble dx, dy;
  gdouble new_x, new_y;
  gdouble new_width, new_height;
  gdouble xmin, xmax;
  gdouble ymin, ymax;
  gboolean veto = TRUE;

  canvas = GTK_PLOT_CANVAS(widget); 

  active_plot = canvas->active_plot;

  if(active_plot == NULL) return TRUE;
  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) return TRUE;

  gdk_pointer_ungrab(event->time);

  gtk_plot_layout_get_position(GTK_PLOT_LAYOUT(canvas),
                               canvas->active_item.new_area.width, 
                               canvas->active_item.new_area.height, 
                               &new_width, &new_height);

  gtk_plot_layout_get_position(GTK_PLOT_LAYOUT(canvas),
                               canvas->active_item.new_area.x, 
                               canvas->active_item.new_area.y, 
                               &new_x, &new_y);

  gtk_plot_layout_get_position(GTK_PLOT_LAYOUT(canvas),
                               canvas->active_item.new_area.x - 
                               canvas->active_item.area.x, 
                               canvas->active_item.new_area.y - 
                               canvas->active_item.area.y, 
                               &dx, &dy);


  if(canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION) {
   switch(canvas->active_item.type){
    case GTK_PLOT_CANVAS_TEXT:
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
    gtk_plot_get_point(active_plot, 
                       MIN(canvas->drag_x, canvas->pointer_x),
                       MIN(canvas->drag_y, canvas->pointer_y),
                       &xmin, &ymax);
    gtk_plot_get_point(active_plot, 
                       MAX(canvas->drag_x, canvas->pointer_x),
                       MAX(canvas->drag_y, canvas->pointer_y),
                       &xmax, &ymin);

    new_width = abs(canvas->pointer_x - canvas->drag_x);
    new_height = abs(canvas->pointer_y - canvas->drag_y);
    gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_REGION],
                    xmin, xmax, ymin, ymax);
    canvas->state = GTK_STATE_NORMAL;
    canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
    return TRUE;
  }
 

  if(veto){
      gtk_plot_layout_refresh(GTK_PLOT_LAYOUT(canvas));
      canvas->active_item.area = canvas->active_item.new_area;
      if(canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION)
             draw_selection(canvas, canvas->active_item.area, TRUE);
  } else {
      canvas->state = GTK_STATE_NORMAL;
  }  
 
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

  width = GTK_PLOT_LAYOUT(plot_canvas)->width, 
  height = GTK_PLOT_LAYOUT(plot_canvas)->height;

  gtk_widget_set_usize(GTK_WIDGET(plot), width, height); 
  gtk_plot_move(plot, x, y);

  plot->left.title.x = plot->x - 45. / (gdouble)width;
  plot->right.title.x = plot->x + plot->width + 45. / (gdouble)width;
  plot->top.title.y = plot->y - 35. / (gdouble)height;
  plot->bottom.title.y = plot->y + plot->height + 35. / (gdouble)height;

  gtk_plot_layout_add_plot(GTK_PLOT_LAYOUT(plot_canvas), plot, 0, 0);
  plot_canvas->active_plot = plot;
}

static void
gtk_plot_canvas_get_pixel(GtkWidget *widget, gdouble px, gdouble py,
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
       draw_selection(plot_canvas, plot_canvas->active_item.area, FALSE); 
    else
       draw_selection(plot_canvas, plot_canvas->active_item.area, TRUE); 
  }
  plot_canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
  plot_canvas->state = GTK_STATE_NORMAL;
  plot_canvas->active_item.type = GTK_PLOT_CANVAS_NONE;
}


GtkPlot *
gtk_plot_canvas_get_active_plot(GtkPlotCanvas *canvas)
{
  return canvas->active_plot;
}


GtkPlotData *
gtk_plot_canvas_get_active_dataset(GtkPlotCanvas *canvas)
{
  return canvas->active_data;
}

void
gtk_plot_canvas_get_active_point(GtkPlotCanvas *canvas, gdouble *x, gdouble *y)
{
  x = &canvas->active_data->x[canvas->active_point];
  y = &canvas->active_data->y[canvas->active_point];
}

GtkPlotCanvasItem *
gtk_plot_canvas_get_active_item(GtkPlotCanvas *canvas)
{
  return &canvas->active_item;
}

void
gtk_plot_canvas_set_size(GtkPlotCanvas *canvas, gint width, gint height)
{
  GList *plots;
  GtkPlot *plot;

  plots = GTK_PLOT_LAYOUT(canvas)->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;
 
       gtk_widget_set_usize(GTK_WIDGET(plot), width, height); 

       plots = plots->next;
     }

  gtk_plot_layout_set_size(GTK_PLOT_LAYOUT(canvas), width, height);

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
    gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                        xor_gc,
                        FALSE,
                        area.x, area.y,
                        area.width, area.height);         
    gdk_draw_line(GTK_LAYOUT(canvas)->bin_window, xor_gc,
                  area.x + 1, area.y + area.height/2, 
                  area.x + 6, area.y + area.height/2);
    gdk_draw_line(GTK_LAYOUT(canvas)->bin_window, xor_gc,
                  area.x + area.width - 1, area.y + area.height / 2, 
                  area.x + area.width - 6, area.y + area.height / 2);
    gdk_draw_line(GTK_LAYOUT(canvas)->bin_window, xor_gc,
                  area.x + area.width/2, area.y + 1, 
                  area.x + area.width/2, area.y + 6);
    gdk_draw_line(GTK_LAYOUT(canvas)->bin_window, xor_gc,
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

      gtk_plot_draw_dataset(canvas->active_plot, xor_gc, canvas->active_data);
      gtk_plot_set_drawable(canvas->active_plot, GTK_LAYOUT(canvas)->bin_window);

      canvas->active_data->x[canvas->active_point] = old_x;
      canvas->active_data->y[canvas->active_point] = old_y;
    }

    return;
  }


  if(markers){
     gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
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

     gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
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

  darea = GTK_LAYOUT(canvas)->bin_window;

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
